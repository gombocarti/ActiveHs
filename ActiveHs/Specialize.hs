{-# LANGUAGE FlexibleInstances #-}
module ActiveHs.Specialize
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

split :: Type a -> ([(String, [String])], Type a)
split (TyForall a Nothing (Just l) t)
    = ( map (\x -> (fst (head x), map snd x)) $ groupBy ((==) `on` fst) $ sort $
          let assertions = case l of
                          CxSingle _ (asst) -> [asst]
                          CxTuple _ assts -> assts
                          CxEmpty _ -> []
          in [(v,s) | ClassA _ (UnQual _ (Ident _ s)) [TyVar _ (Ident _ v)] <- assertions]
      , t
      )
split t
    = ([], t)

convert :: ([(String, [String])], Type a) -> (Type a, Type a)
convert (m, t) = (app True mm t, app False mm t)  where mm = map resolve m

app :: Bool -> [(String, [[Char]])] -> Type l -> Type l
app b m t = f t where
    f (TyFun l a b) = TyFun l (f a) (f b)
    f (TyTuple l_ bo l) = TyTuple l_ bo $ map f l
    f (TyList l t) = TyList l (f t)
    f (TyParen l t) = TyParen l (f t)
    f (TyApp l x t) = TyApp l (f x) (f t)
    f (TyVar l (Ident l' s)) = mkV (head $ [y | (v,x)<-m, v==s, y<-ff  x] ++ ff allT) l l'
    f t = t

    ff = if b then id else reverse

mkV :: String -> l -> l -> Type l
mkV v l l' = TyVar l $ Ident l' v

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
