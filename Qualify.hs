-- | Qualify given names in a Haskell expression
module Qualify
    ( qualify
    ) where

import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

import Data.List ((\\))
import Data.Generics
import Control.Monad.Reader

-------------------------------------

type R = Reader [String]

qualify 
    :: String   -- ^ qualifier to add
    -> [String] -- ^ names to qualifiy
    -> String   -- ^ Haskell expression
    -> Either String String     -- ^ either the modified expression or an error
qualify q ns x = case parseExpWithMode defaultParseMode x of
    ParseOk y -> Right $ prettyPrint $ runReader (trExp y) ns
    e         -> Left $ show e
 where 
    trQName :: QName SrcSpanInfo -> R (QName SrcSpanInfo)
    trQName y@(UnQual loc x) = do
        b <- asks (printName x `elem`)
        return $ if b then (Qual loc (ModuleName loc q) x) else y
    trQName y = return y

    trExp :: Exp SrcSpanInfo -> R (Exp SrcSpanInfo)
    trExp (Lambda loc pats e) = do
        pats' <- tr pats
        e' <- local (\\ vars pats) $ trExp e
        return $ Lambda loc pats' e'
    trExp (Let loc b e) = do
        (b', e') <- local (\\ vars b) $ tr (b, e)
        return $ Let loc b' e'
    trExp x = gmapM tr x

{-
Alt:
Alt SrcLoc Pat GuardedAlts Binds
-}

    tr :: Data x => x -> R x
    tr = everywhereM (mkM trQName) `extM` trExp

    vars :: Data a => a -> [String]
    vars = map printName . everything (++) (mkQ [] patVars_)

    patVars_ :: Pat SrcSpanInfo -> [Name SrcSpanInfo]
    patVars_ (PVar _ x) = [x]
    patVars_ (PAsPat _ x _) = [x]
    patVars_ (PNPlusK _ x _) = [x]
    patVars_ _ = []

    printName :: Name a -> String
    printName (Ident _ x) = x
    printName (Symbol _ x) = x

{- !!!
PatTypeSig SrcLoc Pat Type	
PViewPat Exp Pat
-}

