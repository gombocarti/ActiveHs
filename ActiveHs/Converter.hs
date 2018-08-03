{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards, ViewPatterns, NamedFieldPuns #-}

module ActiveHs.Converter (
    convert
  ) where

import qualified ActiveHs.Bootstrap as B
import qualified ActiveHs.Parser as P
import qualified ActiveHs.Result as R

import           ActiveHs.GHCi (GHCi, GHCiService, EvaluationError)
import qualified ActiveHs.GHCi as GHCi
--import qualified Bootstrap as B
import qualified ActiveHs.Args as Args
import           ActiveHs.Args (Args)
--import Html (renderResult, delim, getOne)
import           ActiveHs.Logger (logMessage, LogLevel(..))
import           ActiveHs.Translation.I18N (I18N)
import qualified ActiveHs.Translation.Entries as E

import qualified Language.Haskell.Exts.Pretty as HPty
import qualified Language.Haskell.Exts.Syntax as HSyn

import qualified Data.HashSet as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import qualified Lucid as L
import qualified Text.Pandoc as Pandoc

import System.Process (readProcessWithExitCode)
import System.Cmd
import System.FilePath
import System.FastLogger (Logger)
import System.Exit
import System.Directory (getTemporaryDirectory, getModificationTime, doesFileExist, getTemporaryDirectory, createDirectoryIfMissing)
import Data.Time (UTCTime) 

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (ExceptT)
import qualified Control.Monad.Except as Except
import Data.Monoid ((<>))
import Data.List
import Data.Char hiding (Format)
import Data.String (fromString)

import qualified GHC
import           GHC.Paths (libdir)
import qualified DynFlags

----------------------------------

type Converter a = ExceptT ConversionError IO a

data ConversionError = ConversionError
  { ceGeneralInfo :: T.Text
  , ceDetails     :: T.Text
  }

conversionErrorCata :: (T.Text -> T.Text -> a)
                    -> ConversionError
                    -> a
conversionErrorCata f (ConversionError general details) =
  f general details

data ConverterConfig = ConverterConfig
  { sourceDir  :: FilePath
  , genDir     :: FilePath
  , hoogleDb   :: Maybe FilePath
  }

convert :: I18N -> ConverterConfig -> Logger -> GHCiService -> FilePath -> IO (Either ConversionError ())
convert i18n config logger ghci filename =
    runExceptT $ do
      needsConversion <- isOutOfDate output input
      when needsConversion $ do
        logMessage DEBUG logger $ output ++ " is out of date, regenerating"
        compile filename
        liftIO $ GHCi.reload ghci filename
        contents <- liftIO $ TIO.readFile filename
        case P.parse contents of
          Right doc -> extract i18n ghci config what doc
          Left err -> throwError $ ConversionError
            { ceGeneralInfo = "Error during parsing"
            , ceDetails     = T.pack $ show err
            }
  where
    input, output, object :: FilePath
    input  = (Args.sourceDir dirs) </> filename <.> "lhs"
    output = (Args.genDir dirs)    </> filename <.> "html"
    object = (Args.sourceDir dirs) </> filename <.> "o"

    compile :: FilePath -> Converter ()
    compile file = do
      GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags
        let ghciCompatible = DynFlags.updateWays $ dflags
              { DynFlags.ghcLink = DynFlags.LinkDynLib
              , DynFlags.hscTarget = DynFlags.HscAsm
              , DynFlags.ghcMode = DynFlags.CompManager
              , DynFlags.ways = [DynFlags.WayDyn]
              }
        GHC.setSessionDynFlags ghciCompatible
        target <- GHC.guessTarget file Nothing
        GHC.setTargets [target]
        GHC.load LoadAllTargets
      return ()
{-        throwError $ CompilationError
        { generalInfo = "Error during compilation"
        , file = input
        , output = err
        } -}


extract :: I18N -> GHCiService -> ConverterConfig -> String -> P.Doc -> Converter ()
extract i18n ghci config filename (P.Doc meta header contents) = do
    liftIO $ writeEx (filename <.> ext) [showEnv $ importsHiding []]
    ss' <- zipWithM processBlock [1..] contents
    let options = Pandoc.def
          { Pandoc.writerTableOfContents = True
          , Pandoc.writerSectionDivs     = True
          }
    case Pandoc.runPure $ Pandoc.writeHtml5 options (Pandoc.Pandoc meta ss') of
      Right html -> liftIO $ writeFile' (gendir </> filename <.> "html") html
      Left err -> throwError $ ConversionError { ceGeneralInfo = "Error while generating html output." }

 where
    ext :: String
    ext = "hs"
    
    lang' = case span (/= '_') . reverse $ filename of
        (l, "")                -> lang
        (l, _) | length l > 2  -> lang
        (x, _)                 -> reverse x

    writeEx :: MonadIO m => FilePath -> [String] -> m ()
    writeEx f l =
        writeFile' (exercisedir </> f) $ intercalate delim l

    writeFile' :: MonadIO m => FilePath -> String -> m ()
    writeFile' f s = liftIO $ do
        logMessage DEBUG $ f ++ " is written."
        createDirectoryIfMissing True (dropFileName f)
        writeFile f s

    readFile' :: MonadIO m => FilePath -> m String
    readFile' f = liftIO $ do
        logMessage DEBUG $ f ++ " is to read..."
        readFile f

    system' :: MonadIO m => String -> m ExitCode
    system' s = liftIO $ do
        logMessage DEBUG $ "executing " ++ s
        system s

    importsHiding :: Set.HashSet P.Name -> String
    importsHiding funnames = case header of
        HSyn.Module loc (HSyn.ModuleName modname) directives _ _ imps _ ->
            HPty.prettyPrint $ 
              HSyn.Module loc (HSyn.ModuleName "") directives Nothing Nothing
                ([mkImport modname funnames, mkImport_ ('X':magicname) modname] ++ imps) []

    mkCodeBlock :: [String] -> B.Html
    mkCodeBlock code = B.card (fromString $ intercalate "\n" code)

----------------------------

    eval :: P.Expression -> P.Correctness -> Converter R.Result
    eval expr correctness = do
      result <- liftIO $ GHCi.evaluate ghci expr i18n
      P.correctnessCata
        (const (either (throwError . evalErrorToConvError) return result))  -- Correct
        (const (either (return . evalErrorToResult) (throwError . resultToConvError) result))  -- HasError

        where
          resultToConvError :: R.Result -> Converter a
          resultToConvError r = ConversionError
            { ceGeneralInfo = i18n $ E.msg_Converter_ErroneousEval "Erroneous evaluation"
            , ceDetails = i18n $ E.msg_Converter_ShouldBeErroneous "Expression should be erroneous but it is not."
            }
          
          throwEvaluationError :: EvaluationError -> Converter R.Result
          throwEvaluationError evalError = throwError $ ConversionError
            { ceGeneralInfo = i18n $ E.msg_Converter_ErroneousEval "Erroneous evaluation"
            , ceDetails = T.pack $ show evalError
            }

    inputDescToHtml :: P.InputDesc -> T.Text -> Converter Pandoc.Block
    inputDescToHtml input ident =
      withInputDesc input $ \expr visibility correctness -> do
        res <- eval expr correctness
        return $ rawHtml $
          P.inputVisibilityCata
            (evaluation res) -- evaluation
            folded           -- folded
            (answer res)     -- answer
            mempty           -- hidden
            visibility
      where
        evaluation :: R.Result -> B.Html
        evaluation res = B.form "" $ B.card $ do
          B.textInput (inputId ident)
          L.with (renderResult res) [L.id_ (resultId ident)]

        folded :: B.Html
        folded = B.form "" $ B.card $ do
          B.textInput (inputId ident)
          L.with (B.card mempty) [L.id_ (resultId ident)]

        answer :: R.Result -> B.Html
        answer res = B.card (renderResult res)

        exercise :: B.Html
        exercise = B.form "" $ B.card $ do
          B.textInput (inputId ident)
          L.with (B.card mempty) [L.id_ (resultId ident)]

    inputId :: T.Text -> T.Text
    inputId ident = fn `T.append` ident `T.append` "_input"

    resultId :: T.Text -> T.Text
    resultId ident = fn `T.append` ident `T.append` "_result"

    fn :: T.Text
    fn = T.pack filename

    renderResult :: R.Result -> B.Html
    renderResult = undefined

    processBlock :: Int -> P.Block -> Converter Pandoc.Block
    processBlock n (P.Example inputDesc) = inputDescToHtml inputDesc (T.pack $ show n)
    processBlock n (P.OneLineExercise code) = do
      _ <- eval (P.Value code) P.Correct
      let fn = filename ++ "_" ++ show n <.> ext
      writeEx fn [showEnv $ importsHiding Set.empty, "\n" ++ "e = " ++ code]
      return $ rawHtml $ B.form "" $ do
        B.textInput (inputId (T.pack $ show n))
        L.with (B.card mempty) [L.id_ (resultId (T.pack $ show n))]
    processBlock n (P.DefinitionExercise _ visi hidden names tests) = do
        let fn = filename ++ "_" ++ show n <.> ext
            (static_, inForm, rows) = if null hidden
                then ([], visi, length visi) 
                else (visi, [], 2 + length hidden)

        writeEx fn  [ showEnv $ importsHiding names ++ "\n" ++ unlines static_
                    , unlines $ hidden, show tests
                    , show names
                    ]
        return . rawHtml $
          mkCodeBlock static_ <>
          B.textArea (inputId (T.pack $ show n))
    processBlock n (P.Raw (Pandoc.CodeBlock ("",[t],[]) l)) 
        | t `elem` ["dot","neato","twopi","circo","fdp","dfdp","latex"] = do
            tmpdir <- liftIO $ getTemporaryDirectory
            let fn = filename ++ show n
                imgname = fn <.> "png"
                outfile = (genDir config) </> fn <.> "png"
                tmpfile = tmpdir </> fn <.> if t=="latex" then "tex" else t

            writeFile' tmpfile $ unlines $ case t of
                "latex" -> 
                    [ "\\documentclass{article}"
                    , "\\usepackage{ucs}"
                    , "\\usepackage[utf8x]{inputenc}"
                    , "\\usepackage{amsmath}"
                    , "\\pagestyle{empty}"
                    -- , "\\newcommand{\\cfrac}[2]{\\displaystyle\\frac{#1}{#2}}"
                    , "\\begin{document}"
                    , "$$", l, "$$"
                    , "\\end{document}" ]
                _ ->
                    ["digraph G {", l, "}"]

            liftIO $ createDirectoryIfMissing True (dropFileName outfile)

            x <- system' $ unwords $ case t of
                "latex" ->  [ "(", "cd", dropFileName tmpfile, "&&"
                            , "latex -halt-on-error", takeFileName tmpfile, "2>&1 >/dev/null", ")"
                            , "&&", "(", "dvipng -D 150 -T tight", "-o", outfile
                            , replaceExtension tmpfile "dvi", "2>&1 >/dev/null",")"]
                _       ->  [ t, "-Tpng", "-o", outfile, tmpfile, "2>&1 >/dev/null" ]

            if x == ExitSuccess 
                then return . rawHtml $ L.img_ [L.src_ (T.pack imgname)]
                else Except.throwError $ ConversionError
                       { ceGeneralInfo = "processDot"
                       , ceDetails = T.pack $ show x
                       }
    processBlock _ (P.Raw content) = return content

------------------------------------

rawHtml :: B.Html -> Pandoc.Block
rawHtml h = Pandoc.RawBlock (Pandoc.Format "html") (TL.unpack $ L.renderText h)

{-
showBlockSimple :: Language -> String -> String -> Int -> String -> [P.Block]

showBlockSimple lang fn i rows_ cont = (:[]) $ rawHtml $ showHtmlFragment $ indent $
  [ form
    ! [ theclass $ if null cont then "interpreter" else "resetinterpreter"
      , action $ getOne "check" fn i i
      ]
    <<[ textarea 
        ! [ cols "80"
          , rows $ show rows_
          , identifier $ "tarea" ++ i
          ]
        << cont
      , br
      , input ! [thetype "submit", value $ translate lang "Check"]
      ]
  , thediv ! [theclass "answer", identifier $ "res" ++ i] << ""
  ]
-}
-----------------

showEnv :: String -> String
showEnv prelude
    = unlines ["{-# LINE 1 \"testenv\" #-}"
              , prelude
              , "{-# LINE 1 \"input\" #-}"
              ]

mkImport :: String -> Set.HashSet P.Name -> HSyn.ImportDecl ()
mkImport m d
    = HSyn.ImportDecl
        { HSyn.importAnn = ()
        , HSyn.importModule = HSyn.ModuleName () m
        , HSyn.importQualified = False
        , HSyn.importSrc = False
        , HSyn.importPkg = Nothing
        , HSyn.importAs = Nothing
        , HSyn.importSpecs = Just (HSyn.ImportSpecList () True (map (HSyn.IVar () . mkName) (Set.toList d)))
        , HSyn.importSafe = False
        }

mkName :: String -> HSyn.Name ()
mkName n@(c:_)
  | isLetter c = HSyn.Ident () n
mkName n       = HSyn.Symbol () n

mkImport_ :: String -> String -> HSyn.ImportDecl ()
mkImport_ magic m 
    = (mkImport m Set.empty) { HSyn.importQualified = True, HSyn.importAs = Just $ HSyn.ModuleName () magic }

------------------

isOutOfDate :: MonadIO m => FilePath -> FilePath -> m Bool
isOutOfDate x src = do
    a <- modTime x
    b <- modTime src
    return $ case (a, b) of
               (Nothing, Just _) -> True
               (Just t1, Just t2) -> t1 < t2
               _   -> False
 where
   modTime :: MonadIO m => FilePath -> m (Maybe UTCTime)
   modTime f = do
       a <- liftIO $ doesFileExist f
       if a
         then liftIO (fmap Just $ getModificationTime f)
         else return Nothing

