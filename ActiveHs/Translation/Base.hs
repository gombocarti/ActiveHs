{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ActiveHs.Translation.Base (
    generateTranslationEntries
  , Language(..)
  , languageCata
  , parseLanguage
  , Translation
  , translationCata
  , translationText
  , translationId
  , (<|)
  , labels
  ) where

import           Control.Monad (sequence)
import           Control.Applicative ((<$>))
import           Data.Char (toLower)
import           Data.Data (Data)
import           Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH

------------

data Language = En | Hu
  deriving (Show, Data)

parseLanguage :: String -> Maybe Language
parseLanguage str = listToMaybe [l | (s, l) <- langs, s == str']
  where
    str' :: String
    str' = map toLower str

    langs :: [(String, Language)]
    langs = [("en", En), ("hu", Hu)]

languageCata :: a -> a -> Language -> a
languageCata en hu lang = case lang of
                            En -> en
                            Hu -> hu

newtype Translation = Translation (Int, T.Text)

translationCata :: (Int -> T.Text -> a) -> Translation -> a
translationCata f (Translation (n, text)) = f n text

translationText :: Translation -> T.Text
translationText = translationCata (\_ident text -> text)

translationId :: Translation -> Int
translationId = translationCata const

(<|) :: (String -> Translation) -> String -> Translation
(<|) = ($)

generateTranslationEntries :: [String] -> TH.Q [TH.Dec]
generateTranslationEntries entryLabels =
  concat <$> sequence [ entry tid l | (tid, l) <- [0..] `zip` entryLabels ]
  where
    entry :: Int -> String -> TH.Q [TH.Dec]
    entry n label = do
      typ <- TH.sigD (TH.mkName label) [t| String -> Translation |]
      body <- [| \text -> Translation (n, T.pack text) |]
      return $ [typ, TH.FunD (TH.mkName label) [TH.Clause [] (TH.NormalB body) []]]

labels :: [String]
labels =
  [ "msg_Converter_ErroneousEval"
  , "msg_Converter_ShouldBeErroneous"
  , "msg_Eval_CantCompareDiagrams"
  , "msg_Eval_CantDecide"
  , "msg_Eval_DontKnowHowToEvaluate"
  , "msg_Eval_ErroneousEval"
  , "msg_Eval_Error"
  , "msg_Eval_GhcException"
  , "msg_Eval_GoodSolution"
  , "msg_Eval_WrongSolution"
  , "msg_Eval_NoHoogleInfo"
  , "msg_Eval_NotAllowed"
  , "msg_Eval_NotSupported"
  , "msg_Eval_SpecializeError"
  , "msg_Eval_UnknownError"
  , "msg_Eval_WontCompile"
  , "msg_Hoogle_NoInfo"
  , "msg_Test_AllCompleted"
  , "msg_WebServer_Inconsistency"
  ]
