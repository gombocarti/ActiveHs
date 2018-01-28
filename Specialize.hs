{-# LANGUAGE FlexibleInstances #-}
module Specialize 
    ( specialize
    ) where

import Data.List
import Data.Function

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Extension

-----------------------------

specialize :: String -> Either String (String, String)
specialize a
    = case parseTypeWithMode (defaultParseMode {extensions = [EnableExtension FlexibleContexts]}) a of
        ParseFailed loc s -> Left $ show s
        ParseOk t -> let

                (t',t'') = convert (split t)

            in Right (prettyPrint t', prettyPrint t'')

split :: Type loc -> ([(String, [String])], Type loc)
split (TyForall _ Nothing (Just l) t) 
    = ( let assertions = case l of
                           CxSingle _ assert -> [assert]
                           CxTuple _ asserts -> asserts
                           CxEmpty _ -> []
        in map (\x -> (fst (head x), map snd x)) $ groupBy ((==) `on` fst) $ sort
          [(v,s) | ClassA _ (UnQual _ (Ident _ s)) [TyVar _ (Ident _ v)] <- assertions]
      , t
      )
split t 
    = ([], t)

convert :: ([(String, [String])], Type loc ) -> (Type loc, Type loc)
convert (m, t) = (app True mm t, app False mm t)  where mm = map resolve m

app :: Bool -> [(String, [[Char]])] -> Type loc -> Type loc
app b m t = f t where
    f (TyFun loc a b) = TyFun loc (f a) (f b)
    f (TyTuple loc bo l) = TyTuple loc bo $ map f l
    f (TyList loc t) = TyList loc (f t)
    f (TyParen loc t) = TyParen loc (f t)
    f (TyApp loc x t) = TyApp loc (f x) (f t)
    f (TyVar _ (Ident loc s)) = mkV loc $ head $ [y | (v,x)<-m, v==s, y<-ff  x] ++ ff allT
    f t = t

    ff = if b then id else reverse

mkV :: loc -> String -> Type loc
mkV loc v = TyVar loc $ Ident loc v

resolve :: (String, [String]) -> (String, [String])
resolve (v, l) = (v, foldl1 intersect $ map res l)

allT :: [String]
allT = ["Double","Integer","Char","Bool","()"]

res :: String -> [String]
res x | x `elem` ["RealFrac", "Real", "RealFloat", "Floating", "Fractional"] = ["Double"]
res x | x `elem` ["Num"] = ["Double","Integer"]
res x | x `elem` ["Integral"] = ["Integer"]
res x | x `elem` ["Monad"] = ["[]","Maybe"]
res x | x `elem` ["Ord","Show","Eq","Enum"] = allT
res x = []  -- !!!


