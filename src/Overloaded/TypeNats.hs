{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Overloaded.TypeNats where

import GHC.TypeNats (Nat)

import qualified Data.Type.Nat as N

class FromNatC a where
    type family FromNat (n :: Nat) :: a

instance FromNatC Nat where
    type FromNat n = n

instance FromNatC N.Nat where
    type FromNat n = N.FromGHC n
