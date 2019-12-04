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

import qualified Data.Bin             as B
import qualified Data.IntMap          as IM
import qualified Data.IntSet          as IS
import qualified Data.Map             as M
import qualified Data.RAList          as RAL
import qualified Data.RAList.NonEmpty as NERAL
import qualified Data.RAVec           as RAV
import qualified Data.RAVec.NonEmpty  as NERAV
import qualified Data.Sequence        as Seq
import qualified Data.Set             as S
import qualified Data.Type.Bin        as B
import qualified Data.Type.Nat        as N
import qualified Data.Vec.Lazy        as Vec

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

instance (a ~ b, b ~ c) =>  Cons a [b] [c] where
    cons = (:)

instance (a ~ b, b ~ c) =>  Cons a [b] (NonEmpty c) where
    cons = (:|)

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

-- | @since 0.1.3
instance Nil (S.Set a) where
    nil = S.empty

-- | @since 0.1.3
instance (Ord a, a ~ b, b ~ c) => Cons a (S.Set b) (S.Set c) where
    cons = S.insert

-- | @since 0.1.3
instance Nil IS.IntSet where
    nil = IS.empty

-- | @since 0.1.3
instance Cons Int IS.IntSet IS.IntSet where
    cons = IS.insert

-- | @since 0.2
instance Nil (Seq.Seq a) where
    nil = Seq.empty

-- | @since 0.2
instance (a ~ b, b ~ c) => Cons a (Seq.Seq b) (Seq.Seq c) where
    cons = (Seq.<|)

-- | @since 0.2
instance Nil (M.Map k v) where
    nil = M.empty

-- | @since 0.2
instance (Ord k, k ~ k1, k ~ k2, v ~ v1, v ~ v2) => Cons (k, v) (M.Map k1 v1) (M.Map k2 v2) where
    cons ~(k, v) = M.insert k v

-- | @since 0.2
instance Nil (IM.IntMap v) where
    nil = IM.empty

-- | @since 0.2
instance (i ~ Int, a ~ b, b ~ c) => Cons (i, a) (IM.IntMap b) (IM.IntMap c) where
    cons ~(i, x) = IM.insert i x

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

-------------------------------------------------------------------------------
-- ral
-------------------------------------------------------------------------------

instance Nil (RAL.RAList a) where
    nil = RAL.empty

instance (a ~ b, a ~ c) => Cons a (RAL.RAList b) (RAL.RAList c) where
    cons = RAL.cons

instance (a ~ b, a ~ c) => Cons a (RAL.RAList b) (NERAL.NERAList c) where
    cons x RAL.Empty         = NERAL.singleton x
    cons x (RAL.NonEmpty xs) = NERAL.cons x xs

instance b ~ 'B.BZ => Nil (RAV.RAVec b a) where
    nil = RAV.empty

instance (b ~ 'B.BP bb, bp ~ B.Pred bb, bb ~ B.Succ' bp) => Cons a (RAV.RAVec bp a) (RAV.RAVec b a) where
    cons = RAV.cons

instance (bp ~ B.Pred b, b ~ B.Succ' bp) => Cons a (RAV.RAVec bp a) (NERAV.NERAVec b a) where
    cons x RAV.Empty         = NERAV.singleton x
    cons x (RAV.NonEmpty xs) = NERAV.cons x xs
