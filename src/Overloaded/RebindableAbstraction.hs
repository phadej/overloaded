{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE RankNTypes             #-}

module Overloaded.RebindableAbstraction (
  Lam(..),
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
