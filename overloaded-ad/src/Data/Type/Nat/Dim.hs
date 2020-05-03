{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Nat.Dim where

import Data.Type.Nat (Nat, SNatI)
import Data.Vec.Lazy (Vec)

import qualified Data.Type.Nat as N

class SNatI (Dim a) => HasDim a where
    type Dim a :: Nat

instance HasDim () where
    type Dim () = N.Nat0

instance HasDim Double where
    type Dim Double = N.Nat1

instance (HasDim a, Dim a ~ N.Nat1, SNatI n) => HasDim (Vec n a) where
    type Dim (Vec n a) = n

instance (HasDim a, HasDim b, SNatI (N.Plus (Dim a) (Dim b))) => HasDim (a, b) where
    type Dim (a, b) = N.Plus (Dim a) (Dim b)
