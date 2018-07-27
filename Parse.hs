
{-# LANGUAGE ExistentialQuantification, RelaxedPolyRec, PatternGuards,
             ViewPatterns #-}

module Parse
    ( ParseMode (..)
    , Module (..)
    , Doc (..)
    , BBlock (..)
    , Name
    , Prompt
    , mainParse
    , getCommand
    , printName
    , parseQuickCheck
    ) where

import qualified Data.Text as T
import Text.Pandoc

import qualified Language.Haskell.Exts.Parser as HPar
import qualified Language.Haskell.Exts.SrcLoc as HLoc
import qualified Language.Haskell.Exts.Syntax as HSyn

import Data.List.Split (splitOn)
import Data.List (tails, partition, groupBy)
import Data.Function (on)
import Data.Char (isAlpha, isSpace, toUpper, isUpper)
import Control.Monad (zipWithM)

--------------------------------- data structures

data ParseMode = HaskellMode -- | AgdaMode
                 deriving (Show, Enum, Eq)

data Module
    = HaskellModule (HSyn.Module HLoc.SrcSpanInfo)
--    | AgdaModule ASyn.Module

data Doc
    = Doc
        Meta{-title, author, date-}
        Module{-module directives, module name, imports-}
        [BBlock]

data BBlock
    = Text Block{-pandoc block-}
    | OneLineExercise
        Prompt
        Bool{-intentional error-}
        String
    | Exercise
        [String]{-lines-}
        [String]{-visible lines-}
        [String]{-hidden lines-}
        [Name]{-defined names-}
        [String]{-test expressions-}

type Prompt = Char  -- see the separate documentation

type Name = String

-----------------------------------

commandList, testCommandList :: String
commandList = "AaRr" ++ testCommandList
testCommandList = "EeFfH"

-----------------------------------

mainParse :: ParseMode -> FilePath -> IO Doc
mainParse mode s = do
    c <- readFile s
    case runPure . readMarkdown pState . T.pack . unlines . concatMap preprocess . lines $ c of
        Right (Pandoc meta (CodeBlock ("",["sourceCode","literate","haskell"],[]) h: blocks)) -> do
            header <- parseModule mode $ h
            fmap (Doc meta header) $ collectTests mode $ map ({-highlight . -}interpreter . Text) blocks
        Right (Pandoc meta blocks) -> do
            header <- parseModule mode $ "module Unknown where"
            fmap (Doc meta header) $ collectTests mode $ map ({-highlight . -}interpreter . Text) blocks
        Left err -> fail $ "readMarkdown: " ++ show err
    where
        parseModule :: ParseMode -> String -> IO Module
        parseModule HaskellMode m = case HPar.parseModuleWithMode HPar.defaultParseMode m of
            (HPar.ParseOk m) -> return $ HaskellModule m
            parseError       -> fail $ "parseHeader: " ++ show parseError

        preprocess (c:'>':' ':l) | c `elem` commandList
            = ["~~~~~ {." ++ [c] ++ "}", dropWhile (==' ') l, "~~~~~", ""]
        preprocess ('|':l)
            = []
        -- drop lines ending with "--"
        preprocess l | take 3 (dropWhile (==' ') $ reverse l) == "-- " = []
                     | otherwise = [l]

        pState = def
            { readerStandalone = True
            , readerExtensions = enableExtension Ext_literate_haskell $ pandocExtensions
            }

        interpreter :: BBlock -> BBlock
        interpreter (Text (CodeBlock ("",[[x]],[]) e)) | x `elem` commandList
            = OneLineExercise (toUpper x) (isUpper x) e
        interpreter a = a

------------------------------

collectTests :: ParseMode -> [BBlock] -> IO [BBlock]
collectTests mode l = zipWithM f l $ tail $ tails l where

    f (Text (CodeBlock ("",["sourceCode","literate","haskell"],[]) h)) l = do
        let
            isExercise = True -- not $ null $ concatMap fst exps

        (visible, hidden, funnames) <- processLines mode isExercise h
        let
            exps = [snd $ getCommand e | (OneLineExercise _ _ e) <- takeWhile p l]

            p (OneLineExercise x _ e) = x `elem` testCommandList && fst (getCommand e) == ""
            p _ = False

        return $ Exercise (lines h) visible hidden funnames exps

    f x _ = return x

processLines :: ParseMode -> Bool -> String -> IO ([String], [String], [Name])
processLines HaskellMode = processHaskellLines

processHaskellLines :: Bool -> String -> IO ([String], [String], [Name])
processHaskellLines isExercise l_ = return (concatMap fst visible, concatMap fst hidden, names)
 where
    x = zip l $ map (HPar.parseDeclWithMode HPar.defaultParseMode . unlines) l

    l = parts l_

    names = concatMap (getFName . snd) x

    getFName (HPar.ParseOk x) = case x of
        HSyn.TypeSig _ a _                       -> map printName a
        HSyn.PatBind _ (HSyn.PVar _ a) _ _         -> [printName a]
        HSyn.FunBind _ (HSyn.Match _ a _ _ _ :_) -> [printName a]
        HSyn.FunBind _ (HSyn.InfixMatch _ _ a _ _ _ :_) -> [printName a]
        HSyn.TypeDecl _ dh _                    -> [printName (getFromDeclHead dh)]
        HSyn.DataDecl _ _ _ dh x _              -> printName (getFromDeclHead dh): [printName n | HSyn.QualConDecl _ _ _ y<-x, n <- getN y]
        _                                        -> []
    getFName _ = []

    getFromDeclHead (HSyn.DHead _ n) = n
    getFromDeclHead (HSyn.DHInfix _ _ n) = n
    getFromDeclHead (HSyn.DHParen _ dh) = getFromDeclHead dh
    getFromDeclHead (HSyn.DHApp _ dh _) = getFromDeclHead dh

    getN (HSyn.ConDecl _ n _) = [n]
    getN (HSyn.InfixConDecl _ _ n _) = [n]
    getN (HSyn.RecDecl _ n l) = n: concat [names | HSyn.FieldDecl _ names _ <- l]

    isVisible (HPar.ParseOk (HSyn.TypeSig _ _ _)) = True
    isVisible (HPar.ParseOk (HSyn.InfixDecl _ _ _ _)) = True
    isVisible _ = not isExercise

    (visible, hidden) = partition (isVisible . snd) x


parts :: String -> [[String]]
parts = groupBy (const id `on` isIndented) . lines  where
    isIndented s | all isSpace s = True
    isIndented (' ':_) = True
    isIndented _ = False

------------------------------

getCommand :: String -> (String, String)
getCommand (':':'?': (dropSpace -> Just x))
    = ("?", x)
getCommand (':': (span isAlpha -> (c@(_:_), dropSpace -> Just x)))
    = (c, x)
getCommand s
    = ("", s)

dropSpace :: String -> Maybe String
dropSpace (' ':y) = Just $ dropWhile (==' ') y
dropSpace "" = Just ""
dropSpace _ = Nothing

parseQuickCheck :: String -> ([String], String)
parseQuickCheck s = case splitOn ";;" s of
    l -> (init l, last l)

printName :: HSyn.Name a -> Name
printName (HSyn.Ident _ x) = x
printName (HSyn.Symbol _ x) = x
