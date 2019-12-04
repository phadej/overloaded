{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Another way to desugar overloaded list literals. See 'Nil' and 'Cons'.
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
    fromList,
  ) where

import Data.Coerce        (coerce)
import Data.List.NonEmpty (NonEmpty (..))
import Data.SOP.NP        (NP (..), POP (..))

import qualified Data.IntSet   as IS
import qualified Data.Set      as S
import qualified Data.Type.Nat as N
import qualified Data.Vec.Lazy as Vec

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

infixr 5 `cons`

-- | @since 0.1.3
fromList :: (Nil xs, Cons x xs xs) => [x] -> xs
fromList = foldr cons nil

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
-- containers
-------------------------------------------------------------------------------

-- | @since 0.1.3
instance Nil (S.Set a) where
    nil = S.empty

-- | @since 0.1.3
instance Ord a => Cons a (S.Set a) (S.Set a) where
    cons = S.insert

-- | @since 0.1.3
instance Nil IS.IntSet where
    nil = IS.empty

-- | @since 0.1.3
instance Cons Int IS.IntSet IS.IntSet where
    cons = IS.insert

-------------------------------------------------------------------------------
-- vec
-------------------------------------------------------------------------------

instance n ~ 'N.Z => Nil (Vec.Vec n a) where
    nil = Vec.VNil

instance (a ~ b, b ~ c, m ~ 'N.S n) => Cons a (Vec.Vec n b) (Vec.Vec m c) where
    cons = (Vec.:::)

-------------------------------------------------------------------------------
-- sop-core
-------------------------------------------------------------------------------

instance xs ~ '[] => Nil (NP f xs) where
    nil = Nil

instance (f ~ g, g ~ h, xxs ~ (x ': xs)) => Cons (f x) (NP g xs)  (NP h xxs) where
    cons = (:*)

instance xs ~ '[] => Nil (POP f xs) where
    nil =  POP Nil

instance (f ~ g, g ~ h, xsxss ~ (xs ': xss)) => Cons (NP f xs) (POP g xss) (POP h xsxss) where
    cons = coerce (cons :: NP f xs -> NP (NP f) xss -> NP (NP f) (xs ': xss))
