{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
-- | Overloaded type-level natural numbers.
module Overloaded.TypeNats where

import GHC.TypeNats (Nat)

import qualified Data.Type.Nat as N

-- | A way to overload type level 'Nat's.
--
-- A number type-literal @42@ is desugared to
--
-- @
-- 'FromNat' 42
-- @
--
-- Enabled with:
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:TypeNats #-}
-- @
--
class FromNatC a where
    type family FromNat (n :: Nat) :: a

instance FromNatC Nat where
    type FromNat n = n

instance FromNatC N.Nat where
    type FromNat n = N.FromGHC n
