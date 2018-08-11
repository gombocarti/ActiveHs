{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards, ViewPatterns, NamedFieldPuns #-}

module ActiveHs.Converter (
    ConversionError(..)
  , convert
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
import           ActiveHs.Logger (Logger, LogLevel(..))
import qualified ActiveHs.Logger as Logger 
import qualified ActiveHs.Translation.Base as Translation
import           ActiveHs.Translation.I18N (I18N)
import qualified ActiveHs.Translation.I18N as I18N
import qualified ActiveHs.Translation.Entries as E

import qualified Language.Haskell.Exts.Pretty as HPty
import qualified Language.Haskell.Exts.Syntax as HSyn
import qualified Language.Haskell.Exts.SrcLoc as HLoc

import qualified Data.HashSet as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Lucid as L
import qualified Text.Pandoc as Pandoc
import qualified Text.Blaze.Html.Renderer.Text as B

import System.Process (readProcessWithExitCode)
import System.Cmd
import System.FilePath
import System.Exit
import System.Directory (getTemporaryDirectory, getModificationTime, doesFileExist, getTemporaryDirectory, createDirectoryIfMissing)
import Data.Time (UTCTime) 

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import qualified Control.Monad.Except as Except
import Data.Monoid ((<>))
import Data.List
import Data.Char hiding (Format)
import Data.String (fromString)

import qualified GHC
import           GHC.Paths (libdir)
import qualified DynFlags

----------------------------------

type Converter a = ReaderT (ConverterConfig, Logger) (ExceptT ConversionError IO) a

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
  { confSourceDir  :: FilePath
  , confGenDir     :: FilePath
  }

config :: Converter ConverterConfig
config = asks fst

logMessage :: LogLevel -> T.Text -> Converter ()
logMessage level msg = asks snd >>= \logger -> Logger.logMessage level logger msg

logAction :: LogLevel -> T.Text -> IO a -> Converter ()
logAction level msg m = do
  logMessage level msg
  void $ liftIO m

convert :: FilePath -> FilePath -> Logger -> GHCiService -> FilePath -> IO (Either ConversionError ())
convert sourceDir genDir logger ghci filename =
  runExceptT $
    runReaderT
      (do
        let baseName = takeBaseName filename
            input = sourceDir </> baseName <.> "lhs"
            output = sourceDir </> baseName <.> "html"
        needsConversion <- isOutOfDate output input
        when needsConversion $ do
          logMessage DEBUG $ T.append (T.pack output) " is out of date, regenerating"
          compile input
          liftIO $ GHCi.reload ghci input
          contents <- liftIO $ TIO.readFile input
          case P.parse contents of
            Right doc -> do
              doc' <- extract (I18N.mkI18N lang) ghci input doc
              let options = Pandoc.def
                    { Pandoc.writerTableOfContents = True
                    , Pandoc.writerSectionDivs     = True
                    }
              case Pandoc.runPure $ Pandoc.writeHtml5 options doc' of
                Right html -> do
                  gendir <- confGenDir <$> config
                  let path = gendir </> baseName <.> "html"
                  logAction DEBUG (T.pack $ "Writing " ++ path) $
                    TLIO.writeFile path (L.renderText $ B.bootstrapPage "ActiveHs" $ L.toHtmlRaw $ B.renderHtml html)
                Left err -> Except.throwError $ ConversionError
                  { ceGeneralInfo = "Error while generating html output."
                  , ceDetails     = ""
                  }
            Left err -> Except.throwError $ ConversionError
              { ceGeneralInfo = "Error during parsing"
              , ceDetails     = T.pack $ show err
              }
      )
      (ConverterConfig sourceDir genDir, logger)
  where
    compile :: FilePath -> Converter ()
    compile file = 
      liftIO $ GHC.runGhc (Just libdir) $ do
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
        GHC.load GHC.LoadAllTargets
        return ()
{-        throwError $ CompilationError
        { generalInfo = "Error during compilation"
        , file = input
        , output = err
        } -}


    lang :: Translation.Language
    lang = let (l, _) = span (/= '_') . reverse $ filename
           in maybe Translation.En id (Translation.parseLanguage (reverse l))

extract :: I18N -> GHCiService -> String -> P.Doc -> Converter Pandoc.Pandoc
extract i18n ghci filename (P.Doc meta header contents) =
  Pandoc.Pandoc meta <$> zipWithM processBlock [1..] contents

 where
    ext :: String
    ext = "hs"

    readFile' :: FilePath -> Converter String
    readFile' f = do
      logMessage DEBUG $ T.append (T.pack f) " is to read..."
      liftIO $ readFile f

    system' :: String -> Converter ExitCode
    system' s = do
      logMessage DEBUG $ T.append "executing " (T.pack s)
      liftIO $ system s

    importsHiding :: Set.HashSet P.Name -> Converter String
    importsHiding funnames = case header of
        HSyn.Module loc (Just (HSyn.ModuleHead _ (HSyn.ModuleName _ modname) _ _)) directives imps _ ->
          return $ HPty.prettyPrint $ 
            HSyn.Module loc Nothing directives
              ([mkImport HLoc.noSrcSpan modname funnames, mkImport_ HLoc.noSrcSpan "X" modname] ++ imps) []
        HSyn.Module _ Nothing _ _ _ ->
          Except.throwError $ ConversionError
          { ceGeneralInfo = "Missing module header."
          , ceDetails     = T.unwords ["Source file ", T.pack filename, " must have module header like 'module ", T.pack filename, " where'."]
          }
        _ ->
          Except.throwError $ ConversionError
          { ceGeneralInfo = "Invalid source file."
          , ceDetails     = T.unwords ["File ", T.pack filename, " does not seem to be a Haskell source file." ]
          }
    mkCodeBlock :: [String] -> B.Html
    mkCodeBlock code = B.card (fromString $ intercalate "\n" code)

----------------------------

    inputDescToHtml :: P.InputDesc -> T.Text -> Converter Pandoc.Block
    inputDescToHtml input ident =
      P.withInputDesc input $ \expr visibility correctness -> do
        res <- eval expr correctness
        return $ rawHtml $
          P.inputVisibilityCata
            (evaluation res) -- evaluation
            folded           -- folded
            (answer res)     -- answer
            mempty           -- hidden
            visibility
      where
        eval :: P.Expression -> P.Correctness -> Converter R.Result
        eval expr correctness = do
          res <- liftIO $ GHCi.evaluate ghci expr i18n
          P.correctnessCata
            (either (Except.throwError . evalErrToConvErr) return res) -- Correct
            (either (return . evalErrToResult) (Except.throwError . resultToConvErr) res) -- HasError
            correctness
            where
              evalErrToConvErr :: GHCi.EvaluationError -> ConversionError
              evalErrToConvErr = GHCi.evaluationErrorCata ConversionError

              evalErrToResult :: GHCi.EvaluationError -> R.Result
              evalErrToResult = GHCi.evaluationErrorCata R.Error

              resultToConvErr :: R.Result -> ConversionError
              resultToConvErr _ = ConversionError
                { ceGeneralInfo = i18n $ E.msg_Converter_ErroneousEval "Erroneous evaluation."
                , ceDetails = i18n $ E.msg_Converter_ShouldBeErroneous "Expression should be erroneous but it is correct."
                }
        
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
      _ <- inputDescToHtml (P.InputDesc (P.Value code) P.Folded P.Correct) (T.pack $ show n)
      let fn = filename ++ "_" ++ show n <.> ext
      return $ rawHtml $ B.form "" $ do
        B.textInput (inputId (T.pack $ show n))
        L.with (B.card mempty) [L.id_ (resultId (T.pack $ show n))]
    processBlock n (P.DefinitionExercise _ visi hidden names tests) = do
        let (static_, inForm, rows) = if null hidden
              then ([], visi, length visi) 
              else (visi, [], 2 + length hidden)
        return . rawHtml $
          mkCodeBlock static_ <>
          B.textArea (inputId (T.pack $ show n))
    processBlock n (P.Raw (Pandoc.CodeBlock (_, [t], _) l))
        | t `elem` ["dot","neato","twopi","circo","fdp","dfdp","latex"] = do
            return . rawHtml $ mempty
{-
            tmpdir <- liftIO $ getTemporaryDirectory
            let fn = filename ++ show n
                imgname = fn <.> "png"
                outfile = (genDir config) </> fn <.> "png"
                tmpfile = tmpdir </> fn <.> if t=="latex" then "tex" else t
                l' = T.pack l

            writeFile' tmpfile $ T.unlines $ case t of
                "latex" -> 
                    [ "\\documentclass{article}"
                    , "\\usepackage{ucs}"
                    , "\\usepackage[utf8x]{inputenc}"
                    , "\\usepackage{amsmath}"
                    , "\\pagestyle{empty}"
                    -- , "\\newcommand{\\cfrac}[2]{\\displaystyle\\frac{#1}{#2}}"
                    , "\\begin{document}"
                    , "$$", l', "$$"
                    , "\\end{document}" ]
                _ ->
                    ["digraph G {", l', "}"]

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
-}
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

mkImport :: loc -> String -> Set.HashSet P.Name -> HSyn.ImportDecl loc
mkImport loc m d
    = HSyn.ImportDecl
        { HSyn.importAnn = loc
        , HSyn.importModule = HSyn.ModuleName loc m
        , HSyn.importQualified = False
        , HSyn.importSrc = False
        , HSyn.importPkg = Nothing
        , HSyn.importAs = Nothing
        , HSyn.importSpecs = Just (HSyn.ImportSpecList loc True (map (HSyn.IVar loc . mkName loc) (Set.toList d)))
        , HSyn.importSafe = False
        }

mkName :: loc -> String -> HSyn.Name loc
mkName loc n@(c:_)
  | isLetter c = HSyn.Ident loc n
mkName loc n   = HSyn.Symbol loc n

mkImport_ :: loc -> String -> String -> HSyn.ImportDecl loc
mkImport_ loc magic m 
    = (mkImport loc m Set.empty) { HSyn.importQualified = True, HSyn.importAs = Just $ HSyn.ModuleName loc magic }

------------------

isOutOfDate :: MonadIO m => FilePath -> FilePath -> m Bool
isOutOfDate x src = do
    a <- liftIO $ modTime x
    b <- liftIO $ modTime src
    return $ case (a, b) of
               (Nothing, Just _) -> True
               (Just t1, Just t2) -> t1 < t2
               _   -> False
 where
   modTime :: FilePath -> IO (Maybe UTCTime)
   modTime f = do
       a <- doesFileExist f
       if a
         then Just <$> getModificationTime f
         else return Nothing

