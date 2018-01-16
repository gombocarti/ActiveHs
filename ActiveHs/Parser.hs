{-# LANGUAGE PatternGuards #-}

module ActiveHs.Parser (
    Doc (..)
  , Block (..)
  , Name
  , Correctness (..)
  , Expression (..)
  , expressionCata
  , InputDesc (..)
  , inputDescCata
  , InputVisibility (..)
  , inputVisibilityCata
  , ParseError (..)
  , parse
  , printName
  , TestCase (..)
  ) where

import qualified Text.Pandoc as P

import qualified Language.Haskell.Exts.Parser as HPar
import qualified Language.Haskell.Exts.Syntax as HSyn
import qualified Language.Haskell.Exts.SrcLoc as HLoc

import           Control.Arrow ((&&&))
import           Data.Bifunctor (bimap)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (isLower, toLower)
import qualified Data.DList as DL
import           Data.Generics (mkQ, extQ, something, everythingBut, GenericQ, Data)
import           Data.Maybe (catMaybes, isJust)
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet as Set
import qualified Data.Text as T
import qualified Text.Regex.PCRE.Light as R

-- | A webpage with metadata, directives, imports and contents
data Doc
    = Doc
        P.Meta           -- Metadata: title, author, date
        (HSyn.Module HLoc.SrcSpanInfo)  -- Module directives, module name and imports of the source file. 
        [Block]          -- Page contents

-- | A single building block of webpage content
data Block
    = Raw P.Block      -- ^ Plain text, bullet list, code block, etc.
    | Example          -- ^ A Haskell expression
        InputDesc
    | OneLineExercise  -- ^ An exercise which required an expression and no function definition is needed
        String
    | DefinitionExercise      -- ^ An exercise which requires a function
        [String]      {-lines-}
        [String]      {-visible lines-}
        [String]      {-hidden lines-}
        (Set.HashSet Name)        {-defined names-}
        [TestCase]    {-test expressions-}

data InputDesc = InputDesc Expression InputVisibility

-- | Indicates whether an example contains an intentional error, such as it throws an exception or uses `error`.
data Correctness
    = HasError
    | Correct

data InputVisibility
    = Evaluation      -- ^ Input and result are shown.
    | Folded          -- ^ Input is shown but result is not visible by default.
    | Answer          -- ^ Input is hidden, only result is shown.
    | Hidden          -- ^ No input or result is visible by default. It is used as test case for exercises.

inputVisibilityCata :: a -> a -> a -> a -> InputVisibility -> a
inputVisibilityCata
  evaluation
  folded
  answer
  hidden
  eType = case eType of
            Evaluation -> evaluation
            Folded -> folded
            Answer -> answer
            Hidden -> hidden

-- | A Haskell expression, possibly incorrect
data Expression
  = HoogleQuery String Correctness
  | HoogleQueryInfo String Correctness
  | Type String Correctness
  | Kind String Correctness
  | Value String Correctness

expressionCata :: (String -> a) -> (String -> a) -> (String -> a) -> (String -> a) -> (String -> a) -> Expression -> a
expressionCata
  hoogle
  hoogleInfo
  type_
  kind
  value
  e =
  case e of
    HoogleQuery s -> hoogle s
    HoogleQueryInfo s -> hoogleInfo s
    Type s -> type_ s
    Kind s -> kind s
    Value s -> value s

inputDescCata :: (Expression -> InputVisibility -> Correctness -> a) -> InputDesc -> a
inputDescCata f (InputDesc expr visibility correctness) = f expr visibility correctness

newtype TestCase = TestCase InputDesc

type Name = String

data Visibility
  = Show
  | Hide
  deriving Eq

-- | Represents a parsing error.
data ParseError
  = MarkdownError                -- ^ Error while parsing markdown
    { generalInfo    :: String
    , pandocDetails  :: P.PandocError
    }
  | HaskellError                 -- ^ Error while parsing Haskell code
    { generalInfo    :: String
    , haskellDetails :: (HLoc.SrcLoc, String)
    }

-----------------------------------

exercisePrompts :: [T.Text]
exercisePrompts = [T.singleton 'R', T.singleton 'r']

validPrompts :: [T.Text]
validPrompts = exercisePrompts ++ map T.singleton "AaEeFfHh"

-----------------------------------

-- | Converts the contents of an exercise file into a `Right Doc` when parsing and evaluation was successful,
--   otherwise to a `Left ParseError`.
parse :: T.Text -> Either ParseError Doc
parse contents = 
    case P.runPure (P.readMarkdown pandocOptions . preprocess $ contents) of
        Right (P.Pandoc meta (P.CodeBlock ("", ["sourceCode","literate","haskell"], []) h: blocks)) ->
            case parseModule h of
                Right header -> Right $ Doc meta header (fromPandocBlock blocks)
                Left err -> Left err
        Right (P.Pandoc meta blocks) ->
            case parseModule "module Unknown where" of
                Right header -> Right $ Doc meta header (fromPandocBlock blocks)
                Left err -> Left err
        Left err -> Left (MarkdownError "Error while parsing markdown" err)
    where
        parseModule :: String -> Either ParseError (HSyn.Module HLoc.SrcSpanInfo)
        parseModule code = case HPar.parseModuleWithMode HPar.defaultParseMode code of
            (HPar.ParseOk ast) -> Right ast
            (HPar.ParseFailed loc msg) -> Left (HaskellError "parseHeader: " (loc, msg))

        preprocess :: T.Text -> T.Text
        preprocess = T.unlines . map statementToCode . filter (not . commentLine) . T.lines
          where
            statementToCode :: T.Text -> T.Text
            statementToCode t
              | c `elem` validPrompts = T.unlines
                  [ (T.pack "~~~~~ {.") `T.append` c `T.append` (T.singleton '}')
                  , T.stripStart expr
                  , T.pack "~~~~~"
                  ]
              where
                c, expr :: T.Text
                (c, expr) = T.breakOn (T.pack "> ") t
            statementToCode t = t

            commentLine :: T.Text -> Bool
            commentLine t = (T.pack "|") `T.isPrefixOf` t

        pandocOptions :: P.ReaderOptions
        pandocOptions = P.def
            { P.readerStandalone = True
            , P.readerExtensions = P.enableExtension P.Ext_literate_haskell P.pandocExtensions
            }
        
        fromPandocBlock :: [P.Block] -> [Block]
        fromPandocBlock (code@(P.CodeBlock ("",[[c]],[]) expr) : bs)
            | T.singleton c `elem` exercisePrompts = OneLineExercise expr : rest
            | Just inputDesc <- blockToInputDesc code = Example inputDesc : rest
          where rest :: [Block]
                rest = fromPandocBlock bs

        fromPandocBlock (b@(P.CodeBlock ("",["sourceCode","literate","haskell"],[]) src) : bs) = block : fromPandocBlock rest
          where
            block :: Block
            block = if null tests
                    then Raw b 
                    else DefinitionExercise (lines src) visible invisible names tests

            tests :: [TestCase]
            rest :: [P.Block]
            (tests, rest) = collectTests bs

            visible, invisible :: [String]
            names :: Set.HashSet Name
            HPar.ParseOk ((visible, invisible), names) = hideSolution src
        fromPandocBlock (b : bs) = Raw b : fromPandocBlock bs
        fromPandocBlock []       = []

parsePrompt :: Char -> Maybe (InputVisibility, Correctness)
parsePrompt p = (,) <$> parseVisibility (toLower p) <*> Just (parseCorrectness p)
  where
    parseVisibility :: Char -> Maybe InputVisibility
    parseVisibility 'e' = Just Evaluation
    parseVisibility 'f' = Just Folded
    parseVisibility 'a' = Just Answer
    parseVisibility 'h' = Just Hidden
    parseVisibility _   = Nothing

    parseCorrectness :: Char -> Correctness
    parseCorrectness c = if isLower c then Correct else HasError

expressionSyntax :: R.Regex
expressionSyntax = R.compile (BC.pack "^[[:space:]]*(:[[:alpha:]][[:space:]])?[[:space:]]*(.+)") []

parseExpression :: String -> Maybe Expression
parseExpression s = do
  matches <- R.match expressionSyntax (BC.pack s) []
  case matches of
    [expr] -> return $ Value (BC.unpack expr)
    [prompt, expr] -> do
      f <- Map.lookup prompt prompts
      return (f (BC.unpack expr))
    _ -> fail ("Invalid expression parse: " ++ s)

    where
      prompts :: Map.HashMap B.ByteString (String -> Expression)
      prompts = Map.fromList
        [ (BC.pack ":h ", HoogleQuery)
        , (BC.pack ":i ", HoogleQueryInfo)
        , (BC.pack ":t ", Type)
        , (BC.pack ":k ", Kind)
        ]

blockToInputDesc :: P.Block -> Maybe InputDesc
blockToInputDesc (P.CodeBlock (_, [[c]], _) src) = do
  expr <- parseExpression src
  (visibility, correctness) <- parsePrompt c
  return $ InputDesc expr visibility correctness
blockToInputDesc _ = Nothing

------------------------------

-- | Cuts of the prefix of a list of blocks that consists of test cases and
--   returns them with the rest of the blocks.
collectTests :: [P.Block] -> ([TestCase], [P.Block])
collectTests bs =
  bimap (map TestCase . catMaybes . map fst) (map snd) $
    span (isJust . fst) (map (\ b -> (blockToInputDesc b, b)) bs)

hideSolution :: String -> HPar.ParseResult (([String], [String]), Set.HashSet Name)
hideSolution code = (\ds -> (partitionLines (spans ds) (lines code), names ds)) <$> decls
 where
   decls :: HPar.ParseResult [HSyn.Decl HLoc.SrcSpanInfo]
   decls = selectDecls <$> HPar.parseModule code
     where selectDecls :: HSyn.Module a -> [HSyn.Decl a]
           selectDecls (HSyn.Module _ _ _ _ ds) = ds
           selectDecls _                        = []

   names :: [HSyn.Decl HLoc.SrcSpanInfo] -> Set.HashSet Name
   names = Set.unions . map getFName

   -- Concentrates only on Names in DeclHead, QualConDecl, ConDecl,
   -- GadtDecl and Pat types.
   getFName :: HSyn.Decl HLoc.SrcSpanInfo -> Set.HashSet Name
   getFName decl = everythingBut Set.union whitelist decl
     where
       whitelist :: GenericQ (Set.HashSet Name, Bool)
       whitelist = def `extQ` getName
                       `extQ` getFromMatch
                       `extQ` getFromDeclHead
                       `extQ` getFromQualConDecl
                       `extQ` getFromConDecl
                       `extQ` getFromGadtDecl
                       `extQ` getFromQualConDeclList
                       `extQ` getFromConDeclList
         where
           def :: Data a => a -> (Set.HashSet Name, Bool)
           def _ = (Set.empty, False)
       
       -- Get a 'Name' and stop traversal
       getName :: HSyn.Name HLoc.SrcSpanInfo -> (Set.HashSet Name, Bool)
       getName n = (Set.singleton (printName n), False)

       -- Get 'Names' from lists
       getFromQualConDeclList :: [HSyn.QualConDecl HLoc.SrcSpanInfo] -> (Set.HashSet Name, Bool)
       getFromQualConDeclList _ = (Set.empty, True)

       -- Get 'Names' from lists
       getFromConDeclList :: [HSyn.ConDecl HLoc.SrcSpanInfo] -> (Set.HashSet Name, Bool)
       getFromConDeclList _ = (Set.empty, True)

       -- Traverse 'DeclHead's
       getFromDeclHead :: HSyn.DeclHead HLoc.SrcSpanInfo -> (Set.HashSet Name, Bool)
       getFromDeclHead _ = (Set.empty, True)

       -- Traverse 'QualConDecl's
       getFromQualConDecl :: HSyn.QualConDecl HLoc.SrcSpanInfo -> (Set.HashSet Name, Bool)
       getFromQualConDecl _ = (Set.empty, True)

       -- Extract 'Name's from a 'ConDecl' and stop traversal. Traverses arguments except 'Type's.
       getFromConDecl :: HSyn.ConDecl HLoc.SrcSpanInfo -> (Set.HashSet Name, Bool)
       getFromConDecl conDecl = (everythingBut Set.union (mkQ (Set.empty, True) getName `extQ` skipType) conDecl, False)
         where
           skipType :: HSyn.Type HLoc.SrcSpanInfo -> (Set.HashSet Name, Bool)
           skipType _ = (Set.empty, False)

       -- Extract 'Name's from a 'GadtDecl' and stop traversal
       getFromGadtDecl :: HSyn.GadtDecl HLoc.SrcSpanInfo -> (Set.HashSet Name, Bool)
       getFromGadtDecl gadtDecl = (everythingBut Set.union (mkQ (Set.empty, False) getName) gadtDecl, False)

       -- Traverse 'Match'es
       getFromMatch :: HSyn.Match HLoc.SrcSpanInfo -> (Set.HashSet Name, Bool)
       getFromMatch _ = (Set.empty, True)

   isVisible :: HSyn.Decl a -> Bool
   isVisible (HSyn.TypeSig _ _ _) = True
   isVisible (HSyn.InfixDecl _ _ _ _) = True
   isVisible _ = False

   srcSpan :: HSyn.Decl HLoc.SrcSpanInfo -> Maybe (HLoc.SrcSpan)
   srcSpan = something (mkQ Nothing getSpan)
     where
       getSpan :: HLoc.SrcSpanInfo -> Maybe HLoc.SrcSpan
       getSpan = Just . HLoc.srcInfoSpan

   partitionLines :: [(Visibility, HLoc.SrcSpan)] -> [String] -> ([String], [String])
   partitionLines lineInfo srcLines = let (visib, invisib) = partition lineInfo srcLines
                                      in (DL.toList visib, DL.toList invisib)
     where
       partition :: [(Visibility, HLoc.SrcSpan)] -> [String] -> (DL.DList String, DL.DList String)
       partition ((visibility, l):ls) codeLines
         | visibility == Show = (visible `DL.append` (DL.fromList decl), invisible)
         | otherwise          = (visible, invisible `DL.append` (DL.fromList decl))
         where
           decl, rest :: [String]
           (decl, rest) = splitAt (HLoc.srcSpanEndLine l) codeLines

           visible, invisible :: DL.DList String
           (visible, invisible) = partition ls rest
       partition [] _ = (DL.empty, DL.empty)

   spans :: [HSyn.Decl HLoc.SrcSpanInfo] -> [(Visibility, HLoc.SrcSpan)]
   spans ds = [ (visib, loc) | (visib, Just loc) <- map (categorize &&& srcSpan) ds ]
     where
       categorize :: HSyn.Decl a -> Visibility
       categorize d
         | isVisible d = Show
         | otherwise   = Hide

------------------------------

printName :: HSyn.Name a -> Name
printName (HSyn.Ident _ x) = x
printName (HSyn.Symbol _ x) = x
