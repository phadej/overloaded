{-# LANGUAGE RecordWildCards #-}
module Overloaded.Plugin.IdiomBrackets where

import Data.List          (foldl')
import Data.List.NonEmpty (NonEmpty (..))

import GHC.Compat.Expr

import Overloaded.Plugin.Rewrite
import Overloaded.Plugin.Names

transformIdiomBrackets
    :: Names
    -> LHsExpr GhcRn
    -> Rewrite (LHsExpr GhcRn)
transformIdiomBrackets names (L _l (HsRnBracketOut _ (ExpBr _ e) _))
    = Rewrite (transformIdiomBrackets' names e)
transformIdiomBrackets _ _ = NoRewrite

transformIdiomBrackets'
    :: Names
    -> LHsExpr GhcRn
    -> LHsExpr GhcRn
transformIdiomBrackets' names expr@(L _e OpApp {}) = do
    let bt = matchOp expr
    let result = idiomBT names bt
    result
transformIdiomBrackets' names expr = do
    let (f :| args) = matchApp expr
    let f' = pureExpr names f
    let result = foldl' (applyExpr names) f' args
    result

-------------------------------------------------------------------------------
-- Function application maching
-------------------------------------------------------------------------------

-- | Match nested function applications, 'HsApp':
-- f x y z ~> f :| [x,y,z]
--
matchApp :: LHsExpr p -> NonEmpty (LHsExpr p)
matchApp (L _ (HsApp _ f x)) = neSnoc (matchApp f) x
matchApp e = pure e

neSnoc :: NonEmpty a -> a -> NonEmpty a
neSnoc (x :| xs) y = x :| xs ++ [y]

-------------------------------------------------------------------------------
-- Operator application matching
-------------------------------------------------------------------------------

-- | Match nested operator applications, 'OpApp'.
-- x + y * z ~>  Branch (+) (Leaf x) (Branch (*) (Leaf y) (Leaf z))
matchOp :: LHsExpr p -> BT (LHsExpr p)
matchOp (L _ (OpApp _  lhs op rhs)) = Branch (matchOp lhs) op (matchOp rhs)
matchOp x = Leaf x

-- | Non-empty binary tree, with elements at branches too.
data BT a = Leaf a | Branch (BT a) a (BT a)

-- flatten: note that leaf is returned as is.
idiomBT :: Names -> BT (LHsExpr GhcRn) -> LHsExpr GhcRn
idiomBT _     (Leaf x)            = x
idiomBT names (Branch lhs op rhs) = fmapExpr names op (idiomBT names lhs) `ap` idiomBT names rhs
  where
    ap = apExpr names

-------------------------------------------------------------------------------
-- Idioms related constructors
-------------------------------------------------------------------------------

applyExpr :: Names -> LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
applyExpr names f (L _ (HsPar _ (L _ (HsApp _ (L _ (HsVar _ (L _ voidName'))) x))))
    | voidName' == voidName names = birdExpr names f x
applyExpr names f x               = apExpr names f x

apExpr :: Names -> LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
apExpr Names {..} f x = hsApps l' (hsVar l' apName) [f, x] where
    l' = noSrcSpan

birdExpr :: Names -> LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
birdExpr Names {..} f x = hsApps l' (hsVar l' birdName) [f, x] where
    l' = noSrcSpan

fmapExpr :: Names -> LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
fmapExpr Names {..} f x = hsApps l' (hsVar l' fmapName) [f, x] where
    l' = noSrcSpan

pureExpr :: Names -> LHsExpr GhcRn -> LHsExpr GhcRn
pureExpr Names {..} x = hsApps l' (hsVar l' pureName) [x] where
    l' = noSrcSpan
