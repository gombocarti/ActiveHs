{-# LANGUAGE PatternGuards, ViewPatterns, NamedFieldPuns #-}

module ActiveHs.Converter (
    convert
  ) where

import qualified ActiveHs.Bootstrap as B
import qualified ActiveHs.Parser as P
import qualified ActiveHs.Result as R

import ActiveHs.GHCi (GHCi, runGHCi, EvaluationError)
--import qualified Smart as S
--import qualified Result as R
--import qualified Bootstrap as B
import ActiveHs.Args
--import Html (renderResult, delim, getOne)
import ActiveHs.Logger (logMessage)
import ActiveHs.Translation.I18N (I18N)
import qualified ActiveHs.Translation.Entries as E

import qualified Language.Haskell.Exts.Pretty as HPty
import qualified Language.Haskell.Exts.Syntax as HSyn

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.String as HR
import qualified Text.Blaze.Html5.Attributes as A
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
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Monoid ((<>))
import Data.List
import Data.Char hiding (Format)
import Data.String (fromString)

----------------------------------

type Converter a = ExceptT ConversionError IO a

data ConversionError
  = ParseError
    { generalInfo  :: String
    , parseDetails :: P.ParseError
    }
  | CompilationError
    { generalInfo :: String
    , file        :: FilePath
    , command     :: String
    , stdOutput   :: String
    , stdError    :: String
    }
  | EvaluationError
    { generalInfo :: String
    , expression  :: String
    , evalDetails :: Either R.Result EvaluationError
    }
  | DotError
    { generalInfo :: String
    , graph       :: String
    , dotDetails  :: String
    }

data ConverterConfig = ConverterConfig
  { sourceDir  :: FilePath
  , genDir     :: FilePath
  , compileCmd :: ProgramWithArgs
  , hoogleDb   :: Maybe FilePath
  }

convert :: I18N -> ConverterConfig -> Logger -> GhciService -> String -> IO (Either ConversionError ())
convert i18n config logger ghci filename =
    runExceptT $ whenOutOfDate () output input $ do
        whenOutOfDate () object input $ do
            logMessage DEBUG $ object ++ " is out of date, regenerating"
            let ProgramWithArgs (ghc, args) = recompileCmd config
            (exitCode, out, err) <- liftIO $ readProcessWithExitCode ghc (args ++ [input]) ""
            if exitCode == ExitSuccess
              then
                liftIO $ C.reload ghci filename
              else
                throwError $ CompilationError
                  { generalInfo = "Error during compilation"
                  , command = recompilecmd
                  , file = input
                  , stdOutput = out
                  , stdError = err
                  }
        logMessage DEBUG $ output ++ " is out of date, regenerating"
        case P.parse input of
          Right doc -> extract i18n ghci args what doc
          Left err -> throwError $ ParseError
            { generalInfo = "Error during parsing"
            , parseDetails = err
            }
 where
   input, output, object :: FilePath

   input  = (sourceDir dirs) </> filename <.> "lhs"
   output = (genDir dirs)    </> filename <.> "xml"
   object = (sourceDir dirs) </> filename <.> "o"


extract :: Bool -> I18N -> S.TaskChan -> Args -> String -> P.Doc -> Converter ()
extract verbose i18n ghci (Args {lang, templatedir, sourcedir, exercisedir, gendir, magicname, hoogledb}) what (P.Doc meta header contents) = do
    liftIO $ writeEx (what <.> ext) [showEnv $ importsHiding []]
    ss' <- zipWithM processBlock [1..] contents

    liftIO $ writeFile' (gendir </> what <.> "xml") $ flip Pandoc.writeHtmlString (Pandoc.Pandoc meta ss')
      $ Pandoc.def
        { Pandoc.writerTableOfContents = True
        , Pandoc.writerSectionDivs     = True
        }

 where
    ext :: String
    ext = "hs"
    
    lang' = case span (/= '_') . reverse $ what of
        (l, "")                -> lang
        (l, _) | length l > 2  -> lang
        (x, _)                 -> reverse x

    writeEx :: MonadIO m => FilePath -> [String] -> m ()
    writeEx f l =
        writeFile' (exercisedir </> f) $ intercalate delim l

    writeFile' :: MonadIO m => FilePath -> String -> m ()
    writeFile' f s = liftIO $ do
        when verbose $ putStrLn $ f ++ " is written."
        createDirectoryIfMissing True (dropFileName f)
        writeFile f s

    readFile' :: MonadIO m => FilePath -> m String
    readFile' f = liftIO $ do
        when verbose $ putStrLn $ f ++ " is to read..."
        readFile f

    system' :: MonadIO m => String -> m ExitCode
    system' s = liftIO $ do
        when verbose $ putStrLn $ "executing " ++ s
        system s

    importsHiding :: [P.Name] -> String
    importsHiding funnames = case header of
        HSyn.Module loc (HSyn.ModuleName modname) directives _ _ imps _ ->
            HPty.prettyPrint $ 
              HSyn.Module loc (HSyn.ModuleName "") directives Nothing Nothing
                ([mkImport modname funnames, mkImport_ ('X':magicname) modname] ++ imps) []

    mkCodeBlock :: [String] -> H.Html
    mkCodeBlock code = B.well (fromString $ intercalate "\n" code)

----------------------------

    eval :: P.Expression -> Converter R.Result
    eval expr = do
      result <- liftIO $ runGHCi (GHCi.evaluate expr) i18n hoogledb
      case result of
        Right evalResult ->
          case (correctness, R.hasError [evalResult]) of
            (P.Correct, False) -> return evalResult
            (P.HasError, True) -> return evalResult
            _                  -> throwError $
              EvaluationError
                { generalInfo = i18n $ E.msg_Converter_ErroneousEval "Erroneous evaluation"
                , expression = expr
                , evalDetails = Left evalResult
                }
        Left err -> throwError $
              EvaluationError
                { generalInfo = i18n $ E.msg_Converter_ErroneousEval "Erroneous evaluation"
                , expression = expr
                , evalDetails = Right err
                }

    showInput :: P.InputDesc -> R.Result -> String -> Pandoc.Block
    showInput (P.InputDesc _expr visiblity _correctness) res ident =
      rawHtml $ P.inputVisibilityCata
                evaluation -- evaluation
                folded     -- folded
                answer     -- answer
                mempty     -- hidden
                visibility
      where
        evaluation :: H.Html
        evaluation = B.form "" $
          B.textInput (inputId ident) <> (B.well (renderResult res)
                                            ! A.id (fromString $ resultId ident))

        folded :: H.Html
        folded = B.form "" $
          B.textInput (inputId ident) <> (B.well mempty
                                            ! A.id (fromString $ resultId ident))

        answer :: H.Html
        answer = B.well (renderResult res)

        exercise :: H.Html
        exercise = B.form "" $
          B.textInput (inputId ident) <> (B.well mempty
                                           ! A.id (fromString $ resultId ident))

    inputId :: String -> String
    inputId ident = ident ++ "_input"

    resultId :: String -> String
    resultId ident = ident ++ "_result"

    processBlock :: Int -> P.Block -> Converter Pandoc.Block
    processBlock n (P.Example expression) = do
      result <- eval expression 
      return $ showExpression expression result (show n)
    processBlock n (P.OneLineExercise expression) = do
      _ <- eval expression
      let code = P.expressionCata (\_ _ src -> src) expression
          m5 = mkHash $ show n ++ code
          i = show m5
          fn = what ++ "_" ++ i <.> ext
          act = getOne "eval" fn i i
      writeEx fn [showEnv $ importsHiding [], "\n" ++ magicname ++ " = " ++ code]
      return $ rawHtml $ B.form "" $
        B.textInput (inputId (show n))
        <> (B.well mempty ! A.id (fromString $ resultId (show n)))
    processBlock n (P.DefinitionExercise _ visi hidden names tests) = do
        let i = show $ mkHash $ unlines names
            j = "_j" ++ i
            fn = what ++ "_" ++ i <.> ext
            (static_, inForm, rows) = if null hidden
                then ([], visi, length visi) 
                else (visi, [], 2 + length hidden)

        writeEx fn  [ showEnv $ importsHiding names ++ "\n" ++ unlines static_
                    , unlines $ hidden, show tests, j, i
                    , show names
                    ]
        return . rawHtml $
          mkCodeBlock static_ <>
          B.textArea (inputId (show n))
    processBlock _ (P.Raw (Pandoc.CodeBlock ("",[t],[]) l)) 
        | t `elem` ["dot","neato","twopi","circo","fdp","dfdp","latex"] = do
            tmpdir <- liftIO $ getTemporaryDirectory
            let i = show $ mkHash $ t ++ l
                fn = what ++ i
                imgname = takeFileName fn <.> "png"
                outfile = gendir </> fn <.> "png"
                tmpfile = tmpdir </> takeFileName fn <.> if t=="latex" then "tex" else t

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
                then return . rawHtml $ H.img ! A.src (fromString imgname)
                else throwError $ DotError
                       { generalInfo = "processDot"
                       , graph = tmpfile
                       , dotDetails = show x
                       }
    processBlock _ (P.Raw content) = return content

------------------------------------

rawHtml :: H.Html -> Pandoc.Block
rawHtml h = Pandoc.RawBlock (Pandoc.Format "html") (HR.renderHtml h)

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

mkImport :: String -> [P.Name] -> HSyn.ImportDecl ()
mkImport m d 
    = HSyn.ImportDecl
        { HSyn.importAnn = ()
        , HSyn.importModule = HSyn.ModuleName m
        , HSyn.importQualified = False
        , HSyn.importSrc = False
        , HSyn.importPkg = Nothing
        , HSyn.importAs = Nothing
        , HSyn.importSpecs = Just (True, map (HSyn.IVar . mkName) d)
        , HSyn.importSafe = False
        }

mkName :: String -> HSyn.Name ()
mkName n@(c:_)
  | isLetter c = HSyn.Ident () n
mkName n       = HSyn.Symbol () n

mkImport_ :: String -> String -> HSyn.ImportDecl ()
mkImport_ magic m 
    = (mkImport m []) { HSyn.importQualified = True, HSyn.importAs = Just $ HSyn.ModuleName magic }

------------------

whenOutOfDate :: MonadIO m => b -> FilePath -> FilePath -> m b -> m b
whenOutOfDate def x src m = do
    a <- modTime x
    b <- modTime src
    case (a, b) of
        (Nothing, Just _) -> m
        (Just t1, Just t2) | t1 < t2 -> m
        _   -> return def
 where
   modTime :: MonadIO m => FilePath -> m (Maybe UTCTime)
   modTime f = do
       a <- liftIO $ doesFileExist f
       if a
         then liftIO (fmap Just $ getModificationTime f)
         else return Nothing


--------------------
