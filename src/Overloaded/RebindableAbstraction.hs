{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}

module Overloaded.RebindableAbstraction (
  Lam(..),
  lam2,
  lam3,
  lam4,
  lam5,
  lam6,
  lam7,
) where

import Overloaded.RebindableApplication (A, pureA)

class Lam f a b | f -> a b where
  lam :: (a -> b) -> f

instance Lam (a -> b) a b where
  lam :: (a -> b) -> a -> b
  lam = id

instance Applicative f => Lam (A f (a -> b)) a b where
  lam :: (a -> b) -> A f (a -> b)
  lam = pureA

lam2 ::
  (Lam f2 c f1, Lam f1 b a) =>
  (c -> b -> a) -> f2
lam2 f = lam $ lam . f

-- AKA
-- lam3 f = lam (\a -> lam (\b -> lam (f a b)))
lam3 ::
  (Lam f3 d f2, Lam f2 c f1, Lam f1 b a) =>
  (d -> c -> b -> a) -> f3
lam3 f = lam $ lam2 . f

lam4 ::
  (Lam f4 e f3, Lam f3 d f2, Lam f2 c f1, Lam f1 b a) =>
  (e -> d -> c -> b -> a) -> f4
lam4 f = lam $ lam3 . f

lam5 ::
  (Lam f5 f f4, Lam f4 e f3, Lam f3 d f2, Lam f2 c f1, Lam f1 b a) =>
  (f -> e -> d -> c -> b -> a) -> f5
lam5 f = lam $ lam4 . f

lam6 ::
  (Lam f6 g f5, Lam f5 f f4, Lam f4 e f3, Lam f3 d f2, Lam f2 c f1, Lam f1 b a) =>
  (g -> f -> e -> d -> c -> b -> a) -> f6
lam6 f = lam $ lam5 . f

lam7 ::
  (Lam f7 h f6, Lam f6 g f5, Lam f5 f f4, Lam f4 e f3, Lam f3 d f2, Lam f2 c f1, Lam f1 b a) =>
  (h -> g -> f -> e -> d -> c -> b -> a) -> f7
lam7 f = lam $ lam6 . f
