{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Another way to desugar overloaded numeric literals. See 'FromNatural'.
--
-- An explicit list expression, e.g. @[1, True]@ is desugared to
--
-- @
-- cons 1 (cons True nil)
-- @
--
-- Enabled with:
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Lists #-}
-- @
--
module Overloaded.Lists (
    Nil (..),
    Cons (..),
  ) where

import Data.SOP.NP (NP (..), POP (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Coerce (coerce)

import qualified Data.Vec.Lazy as Vec
import qualified Data.Type.Nat as N

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Class for nil, @[]@
--
-- See test-suite for ways to define instances for 'Data.Map.Map'.
-- There are at-least two-ways.
-- 
class Nil a where
    nil :: a

-- | Class for Cons ':'.
class Cons x ys zs | zs -> x ys where
    cons :: x -> ys -> zs

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance Nil [a] where
    nil = []

instance Cons a [a] [a] where
    cons = (:)

instance Cons a [a] (NonEmpty a) where
    cons = (:|)

-------------------------------------------------------------------------------
-- vec
-------------------------------------------------------------------------------

instance n ~ 'N.Z => Nil (Vec.Vec n a) where
    nil = Vec.VNil

instance Cons a (Vec.Vec n a) (Vec.Vec ('N.S n) a) where
    cons = (Vec.:::)

-------------------------------------------------------------------------------
-- sop-core
-------------------------------------------------------------------------------

instance xs ~ '[] => Nil (NP f xs) where
    nil = Nil

instance Cons (f x) (NP f xs)  (NP f (x ': xs)) where
    cons = (:*)

instance xs ~ '[] => Nil (POP f xs) where
    nil =  POP Nil

instance Cons (NP f xs) (POP f xss) (POP f (xs ': xss)) where
    cons = coerce ((:*) :: NP f xs -> NP (NP f) xss -> NP (NP f) (xs ': xss))
