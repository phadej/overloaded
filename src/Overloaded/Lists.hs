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

    null :: a -> Bool

-- | Class for Cons ':'.
class Cons x ys zs | zs -> x ys where
    cons :: x -> ys -> zs

    uncons :: zs -> Maybe (x, ys)

-- | @since 0.1.2
fromList :: (Nil xs, Cons x xs xs) => [x] -> xs
fromList = foldr cons nil

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance Nil [a] where
    nil = []
    null = Prelude.null

instance Cons a [a] [a] where
    cons = (:)

    uncons []     = Nothing
    uncons (x:xs) = Just (x, xs)

instance Cons a [a] (NonEmpty a) where
    cons = (:|)
    uncons (x :| xs) = Just (x, xs)

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

-- | @since 0.1.2
instance Nil (S.Set a) where
    nil = S.empty
    null = S.null

-- | @since 0.1.2
instance Ord a => Cons a (S.Set a) (S.Set a) where
    cons = S.insert
    uncons = S.minView

-- | @since 0.1.2
instance Nil IS.IntSet where
    nil = IS.empty
    null = IS.null

-- | @since 0.1.2
instance Cons Int IS.IntSet IS.IntSet where
    cons = IS.insert
    uncons = IS.minView

-------------------------------------------------------------------------------
-- vec
-------------------------------------------------------------------------------

instance n ~ 'N.Z => Nil (Vec.Vec n a) where
    nil = Vec.VNil
    null _ = True

instance Cons a (Vec.Vec n a) (Vec.Vec ('N.S n) a) where
    cons = (Vec.:::)
    uncons (x Vec.::: xs) = Just (x, xs)

-------------------------------------------------------------------------------
-- sop-core
-------------------------------------------------------------------------------

instance xs ~ '[] => Nil (NP f xs) where
    nil = Nil
    null _ = True

instance Cons (f x) (NP f xs)  (NP f (x ': xs)) where
    cons = (:*)
    uncons (x :* xs) = Just (x, xs)

instance xs ~ '[] => Nil (POP f xs) where
    nil =  POP Nil
    null _ = True

instance Cons (NP f xs) (POP f xss) (POP f (xs ': xss)) where
    cons = coerce (cons :: NP f xs -> NP (NP f) xss -> NP (NP f) (xs ': xss))
    uncons = coerce (uncons :: NP (NP f) (xs ': xss) -> Maybe (NP f xs, NP (NP f) xss))
