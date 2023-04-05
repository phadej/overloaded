{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE InstanceSigs              #-}

module Overloaded.RebindableApplication (
  Apply(..),
  ($),
  A(A),
  unA,
  pureA
) where

import Prelude hiding (($))

-- | Create instances for this to extend the overloading.
class Apply f a b | f -> a b where
  apply :: f -> a -> b

infixr 0 `apply`

-- | Import this for the actual overload.
($) :: Apply f a b => f -> a -> b
($) = apply
infixr 0 $

instance Apply (a -> b) a b where
  apply :: (a -> b) -> a -> b
  apply f = f

-- Applicative (boxing needed to avoid fundep conflict)

data A f a = (Applicative f) => A (f a)

unA :: Applicative f => A f a -> f a
unA (A a) = a

instance Applicative f => Apply (A f (a -> b)) (f a) (A f b) where
  apply :: A f (a -> b) -> f a -> A f b
  apply (A fun) a = A (fun <*> a)

pureA :: Applicative f => a -> A f a
pureA = A . pure
