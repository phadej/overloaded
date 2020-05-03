{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Nat.PlusTree where

import Data.Kind     (Type)
import Data.Type.Nat (Nat (..), SNat (..), SNatI (..))

import qualified Data.Type.Nat as N

-------------------------------------------------------------------------------
-- PlusTree
-------------------------------------------------------------------------------

data PT
    = Leaf Nat
    | Plus PT PT

type family Eval (pt :: PT) :: Nat where
    Eval ('Leaf n)    = n
    Eval ('Plus n m) = N.Plus (Eval n) (Eval m)

data SPT (pt :: PT) where
    SLeaf :: SNatI n => SPT ('Leaf n)
    SPlus :: SPT x -> SPT y -> SPT ('Plus x y)

class SNatI (Eval pt) => SPTI (pt :: PT) where
    spt :: SPT pt

instance SNatI n => SPTI ('Leaf n) where
    spt = SLeaf

instance (SPTI x, SPTI y, SNatI (Eval ('Plus x y))) => SPTI ('Plus x y) where
    spt = SPlus spt spt

snatEval :: SPT pt -> SNat (Eval pt)
snatEval SLeaf       = snat
snatEval (SPlus x y) = snatPlus (snatEval x) (snatEval y)

-------------------------------------------------------------------------------
-- Data.Type.Nat Plus addons
-------------------------------------------------------------------------------

caseSNat
    :: SNat n
    -> ((n ~ 'Z) => r)
    -> (forall m. (n ~ 'S m) => SNat m -> r)
    -> r
caseSNat SZ z _ = z
caseSNat SS _ s = s snat

snatPlus :: SNat n -> SNat m -> SNat (N.Plus n m)
snatPlus n m = caseSNat n m
    $ \n' -> N.withSNat (snatPlus n' m) SS

-------------------------------------------------------------------------------
-- Wrapped
-------------------------------------------------------------------------------

data Wrapped (f :: Nat -> Type -> Type) (s :: Type) (t :: PT)
    = Wrap (SPT t) (f (Eval t) s)

rewrap :: (SPTI t', Eval t ~ Eval t') => Wrapped f s t -> Wrapped f s t'
rewrap (Wrap _ xs) = Wrap spt xs
