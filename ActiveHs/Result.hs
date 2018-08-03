{-# LANGUAGE OverloadedStrings #-}
module ActiveHs.Result (
      Result (..)
    , isError
    ) where

import           Data.Data.Compare (Answer)
import qualified Data.Text as T

import           Control.DeepSeq (NFData(rnf))

---------------------

data Result
    = ExprType String String
    | TypeKind String String
    | Comparison String Answer String
    | SearchResults [String]
    | GoodSolution
    | CantDecideSolution String String
    | WrongSolution String String
    | Message T.Text
    | Error
      { generalInfo :: T.Text
      , details     :: T.Text
      }
    | Dia String
    | TestsPassed
    | TestsFailed T.Text Result

instance NFData Result where
    rnf (ExprType expr type_) = rnf expr `seq` rnf type_
    rnf (TypeKind type_ kind) = rnf type_ `seq` rnf kind
    rnf (Comparison a x b) = rnf a `seq` rnf x `seq` rnf b
    rnf (SearchResults l) = rnf l

    rnf (Message msg) = rnf msg
    rnf (Error general details_) = rnf general `seq` rnf details_
    rnf (Dia html) = rnf html
    rnf TestsPassed = ()
    rnf (TestsFailed test result) = rnf test `seq` rnf result
    rnf GoodSolution              = ()
    rnf (CantDecideSolution a b)  = rnf a `seq` rnf b
    rnf (WrongSolution a b)       = rnf a `seq` rnf b

{-
errors :: Result -> Bool
errors (ExprType _ _ _ l) = not $ null l
errors (TypeKind _ _ l) = not $ null l
errors (Comparison _ x _ l) = x /= Yes || not (null l)
errors (Dia _ l) = not $ null l
errors (Error i _) = i
errors (Message _ x) = maybe False errors x
errors _ = False
-}
{-
filterResults :: [Result] -> [Result]
filterResults rs = case filter (not . weakOrHardError) rs of
    [] -> case [e | e@(Error True _) <- rs] of
            [] -> take 1 rs
            rs -> take 1 rs
    rs -> case filter (not . searchResult) rs of
        [] -> rs
        rs -> {- nubBy f -} rs
-}
{-
 where
    f (ExprType _ _ _ _) (ExprType _ _ _ _) = True
    f _ _ = False
-}

isError :: Result -> Bool
isError (Error {}) = True
isError _          = False
{-
hasError :: [Result] -> Bool
hasError rs = case filter (not . weakOrHardError) rs of
    [] -> True
    rs -> any errors rs

weakOrHardError :: Result -> Bool
weakOrHardError (Error _ _) = True
weakOrHardError (ExprType b _ _ _) = b
weakOrHardError _ = False

searchResult :: Result -> Bool
searchResult (SearchResults _ _) = True
searchResult _ = False

-}
