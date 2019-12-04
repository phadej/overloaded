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

-- | Another way to desugar list literals.
--
-- An explicit list expression, e.g. @[1, True]@ is desugared to
--
-- @
-- cons 1 (cons True nil)
-- @
--
-- This desugaring uses bidirectional functional dependencies to
-- make `cons` infer more. The trade-off is that we can have strictly
-- less instances.
--
-- Enabled with:
--
-- @
-- {-\# OPTIONS_GHC -fplugin=Overloaded
--                 -fplugin-opt=Overloaded:Lists=Overloaded.Lists.Bidi.nil=Overloaded.Lists.Bidi.cons
-- @
--
module Overloaded.Lists.Bidi (
    nil,
    Cons (..),
    Uni.fromList,
  ) where

import Data.SOP.NP (NP (..), POP (..))

import qualified Data.IntMap   as IM
import qualified Data.IntSet   as IS
import qualified Data.Sequence as Seq
import qualified Data.Map      as M
import qualified Data.Set      as S
import qualified Data.Type.Nat as N
import qualified Data.Vec.Lazy as Vec

import qualified Overloaded.Lists as Uni

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

nil :: Uni.Nil a => a
nil = Uni.nil
{-# INLINE nil #-}

-- | Bidirectional class for Cons ':'.
--
-- @since 0.2
class Uni.Cons x ys zs => Cons x ys zs | zs -> x ys, x ys -> zs where
    cons :: x -> ys -> zs
    cons = Uni.cons

infixr 5 `cons`

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance (a ~ b, b ~ c) => Cons a [b] [c]

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance (Ord a, a ~ b, b ~ c) => Cons a (S.Set a) (S.Set a)
instance Cons Int IS.IntSet IS.IntSet
instance (a ~ b, b ~ c) => Cons a (Seq.Seq b) (Seq.Seq c)
instance (Ord k, k ~ k1, k ~ k2, v ~ v1, v ~ v2) => Cons (k, v) (M.Map k1 v1) (M.Map k2 v2)
instance (i ~ Int, a ~ b, b ~ c) => Cons (i, a) (IM.IntMap b) (IM.IntMap c)

-------------------------------------------------------------------------------
-- vec
-------------------------------------------------------------------------------

instance (a ~ b, b ~ c, m ~ 'N.S n) => Cons a (Vec.Vec n b) (Vec.Vec m c)

-------------------------------------------------------------------------------
-- sop-core
-------------------------------------------------------------------------------

instance (f ~ g, g ~ h, xxs ~ (x ': xs)) => Cons (f x) (NP g xs)  (NP h xxs)
instance (f ~ g, g ~ h, xsxss ~ (xs ': xss)) => Cons (NP f xs) (POP g xss) (POP h xsxss)
