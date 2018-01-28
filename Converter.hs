{-# LANGUAGE PatternGuards, ViewPatterns, NamedFieldPuns #-}

module Converter
    ( convert
    ) where

import Parse

import Smart (TaskChan, restart, interp)
import Result (hasError)
import Html
import Lang
import Args
import Hash

import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile)

import qualified Language.Haskell.Exts.Pretty as HPty
import qualified Language.Haskell.Exts.SrcLoc as HLoc
import qualified Language.Haskell.Exts.Syntax as HSyn

import Text.XHtml.Strict hiding (lang)
import Text.Pandoc hiding (getModificationTime)

import System.Process (readProcessWithExitCode)
import System.Cmd
import System.FilePath
import System.Exit
import System.Directory as Dir (getTemporaryDirectory, getModificationTime, doesFileExist, getTemporaryDirectory, createDirectoryIfMissing)
--import Data.Time (diffUTCTime) 

import Control.Monad
import Data.List
import Data.Char hiding (Format)

----------------------------------

convert :: TaskChan -> Args -> String -> IO ()
convert ghci args@(Args {magicname, sourcedir, gendir, recompilecmd, verbose}) what = do
    whenOutOfDate () output input $ do
        whenOutOfDate () object input $ do
            when (verbose > 0) $ putStrLn $ object ++ " is out of date, regenerating"
--            x <- system $ recompilecmd ++ " " ++ input
            let (ghc:args) = words recompilecmd -- !!!
            (x, out, err) <- readProcessWithExitCode ghc (args ++ [input]) ""
            if x == ExitSuccess
                then do
                    restart ghci
                    return ()
                else fail $ unlines [unwords [recompilecmd, input], show x, out, err]
        when (verbose > 0) $ putStrLn $ output ++ " is out of date, regenerating"
        mainParse HaskellMode input  >>= extract HaskellMode (verbose > 0) ghci args what
 where
    input  = sourcedir  </> what <.> "lhs"
    output = gendir     </> what <.> "xml"
    object = sourcedir  </> what <.> "o"


extract :: ParseMode -> Bool -> TaskChan -> Args -> Language -> Doc -> IO ()
extract mode verbose ghci (Args {lang, templatedir, sourcedir, exercisedir, gendir, magicname}) what (Doc meta modu ss) = do

    writeEx (what <.> ext) [showEnv mode $ importsHiding []]
    ss' <- zipWithM processBlock [1..] $ preprocessForSlides ss
    ht <- readFile' $ templatedir </> lang' ++ ".template"
    putStrLn $ "Lang is:" ++ lang'

    let writerOptions = def
          { writerTableOfContents = True
          , writerSectionDivs     = True
          , writerTemplate        = Just ht
          }

    case runPure $ writeHtml5String writerOptions (Pandoc meta $ concat ss') of
      Right html -> writeFile' (gendir </> what <.> "xml") html
      Left err -> error $ show err

 where
    ext = case mode of
        HaskellMode -> "hs"
    
    lang' = case span (/= '_') . reverse $ what of
        (l, "")                -> lang
        (l, _) | length l > 2  -> lang
        (x, _)                 -> reverse x

    writeEx f l =
        writeFile' (exercisedir </> f) $ T.pack $ intercalate delim l

    writeFile' f s = do
        when verbose $ putStrLn $ f ++ " is written."
        createDirectoryIfMissing True (dropFileName f)
        T.writeFile f s

    readFile' f = do
        when verbose $ putStrLn $ f ++ " is to read..."
        readFile f

    system' s = do
        when verbose $ putStrLn $ "executing " ++ s
        system s

    importsHiding funnames = case modu of
        HaskellModule (HSyn.Module loc (Just (HSyn.ModuleHead _ modname _ _)) directives imps _) ->
            HPty.prettyPrint $ 
              HSyn.Module loc Nothing directives
                ([mkImport modname funnames, mkImport_ ('X':magicname) modname] ++ imps) []
--        _ -> error "error in Converter.extract"

    mkCodeBlock l =
        [ CodeBlock ("", ["haskell"], []) $ intercalate "\n" l | not $ null l ]

----------------------------

-- todo: processBlock :: Int -> BBlock -> IO (Either Html [Block])
--  hogy a hibákhoz lehessen rendes html oldalt generálni
--  vagy a Resulthoz jobb show-t írni

    processBlock :: Int -> BBlock -> IO [Block]

    processBlock _ (Exercise visihidden _ _ funnames is)
        | null funnames || null is
        = return $ mkCodeBlock $ visihidden

    processBlock _ (Exercise _ visi hidden funnames is) = do
        let i = show $ mkHash $ unlines funnames
            j = "_j" ++ i
            fn = what ++ "_" ++ i <.> ext
            (static_, inForm, rows) = if null hidden
                then ([], visi, length visi) 
                else (visi, [], 2 + length hidden)

        writeEx fn  [ showEnv mode $ importsHiding funnames ++ "\n" ++ unlines static_
                    , unlines $ hidden, show $ map parseQuickCheck is, j, i
                    , show funnames ]
        return
            $  mkCodeBlock static_
            ++ showBlockSimple lang' fn i rows (intercalate "\n" inForm)

    processBlock ii (OneLineExercise p correct exp) = do
        let m5 = mkHash $ show ii ++ exp
            i = show m5
            fn = what ++ (if p == 'R' then "_" ++ i else "") <.> ext
            act = getOne "eval" fn i i

        when (p == 'R') $ writeEx fn [showEnv mode $ importsHiding [], "\n" ++ magicname ++ " = " ++ exp]
        when verbose $ putStrLn $ "interpreting  " ++ exp
        result <- interp False m5 lang' ghci (exercisedir </> fn) exp Nothing
        when (correct == hasError [result])
          $ error $ translate lang' "Erroneous evaluation"  ++ ": " ++ exp ++ " ; " ++ showHtmlFragment (renderResult result)
        return $
          if p == 'H'
            then []
            else let toShow = if p `elem` ['F', 'R']
                                then Nothing
                                else Just result
                 in [rawHtml $ showHtmlFragment $ showInterpreter lang' 60 act i p exp toShow]

    processBlock _ (Text (CodeBlock ("",[t],[]) l)) 
        | t `elem` ["dot","neato","twopi","circo","fdp","dfdp","latex"] = do
            tmpdir <- getTemporaryDirectory
            let i = show $ mkHash $ t ++ l
                fn = what ++ i
                imgname = takeFileName fn <.> "png"
                outfile = gendir </> fn <.> "png"
                tmpfile = tmpdir </> takeFileName fn <.> if t=="latex" then "tex" else t

            writeFile' tmpfile $ T.pack $ unlines $ case t of
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

            createDirectoryIfMissing True (dropFileName outfile)

            x <- system' $ unwords $ case t of
                "latex" ->  [ "(", "cd", dropFileName tmpfile, "&&"
                            , "latex -halt-on-error", takeFileName tmpfile, "2>&1 >/dev/null", ")"
                            , "&&", "(", "dvipng -D 150 -T tight", "-o", outfile
                            , replaceExtension tmpfile "dvi", "2>&1 >/dev/null",")"]
                _       ->  [ t, "-Tpng", "-o", outfile, tmpfile, "2>&1 >/dev/null" ]

            if x == ExitSuccess 
                then return [Para [Image ("", [], []) [Str imgname] (imgname, "")]]
                else fail $ "processDot " ++ tmpfile ++ "; " ++ show x

    processBlock _ (Text l)
        = return [l]


---------------------------------

preprocessForSlides :: [BBlock] -> [BBlock]
preprocessForSlides x = case span (not . isLim) x of
    (a, []) -> a
    (a, b) -> a ++ case span (not . isHeader) b of
        (c, d) -> [Text $ rawHtml "<div class=\"handout\">"] ++ c 
               ++ [Text $ rawHtml "</div>"] ++ preprocessForSlides d
 where
    isLim (Text HorizontalRule) = True
    isLim _ = False

    isHeader (Text (Header {})) = True
    isHeader _ = False

------------------------------------

rawHtml :: String -> Block
rawHtml x = RawBlock (Format "html") x

showBlockSimple :: Language -> String -> String -> Int -> String -> [Block]

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

-----------------

showEnv :: ParseMode -> String -> String
showEnv HaskellMode prelude
    =  "{-# LINE 1 \"testenv\" #-}\n"
    ++ prelude
    ++ "\n{-# LINE 1 \"input\" #-}\n"

mkImport :: HSyn.ModuleName HLoc.SrcSpanInfo -> [Name] -> HSyn.ImportDecl HLoc.SrcSpanInfo
mkImport m d 
    = HSyn.ImportDecl
        { HSyn.importAnn = HLoc.noSrcSpan
        , HSyn.importModule = m
        , HSyn.importQualified = False
        , HSyn.importSrc = False
        , HSyn.importPkg = Nothing
        , HSyn.importAs = Nothing
        , HSyn.importSpecs = Just (HSyn.ImportSpecList HLoc.noSrcSpan True (map (HSyn.IVar HLoc.noSrcSpan . mkName) d))
        , HSyn.importSafe = False
        }

mkName :: String -> HSyn.Name HLoc.SrcSpanInfo
mkName n@(c:_)
  | isLetter c = HSyn.Ident HLoc.noSrcSpan n
mkName n       = HSyn.Symbol HLoc.noSrcSpan n

mkImport_ :: String -> HSyn.ModuleName HLoc.SrcSpanInfo -> HSyn.ImportDecl HLoc.SrcSpanInfo
mkImport_ magic m 
    = (mkImport m []) { HSyn.importQualified = True, HSyn.importAs = Just $ HSyn.ModuleName HLoc.noSrcSpan magic }

------------------

whenOutOfDate :: b -> FilePath -> FilePath -> IO b -> IO b
whenOutOfDate def x src m = do
    a <- modTime x
    b <- modTime src
    case (a, b) of
        (Nothing, Just _) -> m
        (Just t1, Just t2) | t1 < t2 -> m
        _   -> return def
 where
    modTime f = do
        a <- doesFileExist f
        if a then fmap Just $ getModificationTime f else return Nothing


--------------------



