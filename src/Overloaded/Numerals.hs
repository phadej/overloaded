{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Another way to desugar overloaded numeric literals. See 'FromNatural'.
module Overloaded.Numerals where

import Data.Proxy      (Proxy (..))
import GHC.Exts        (Constraint)
import GHC.TypeLits    (ErrorMessage (..), TypeError)
import GHC.TypeNats    (KnownNat, Nat, natVal, type (<=?))
import Numeric.Natural (Natural)

import Data.Word (Word8)

-- | Another way to desugar numerals.
--
-- A numeric literal @123@ is desugared to
--
-- @
-- 'fromNatural' \@123
-- @
--
-- Enabled with:
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Numerals #-}
-- @
--
-- One can do type-level computations with this.
--
class FromNatural (n :: Nat) a where
    fromNatural :: a

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance KnownNat n => FromNatural n Natural where
    fromNatural = natVal (Proxy :: Proxy n)


type family IsWord8 (n :: Nat) (b :: Bool) :: Constraint where
    IsWord8 n 'True  = ()
    IsWord8 n 'False = TypeError ('Text "Overflowing Word8 " ':<>: 'ShowType n)

instance (KnownNat n, IsWord8 n (n <=? 255)) => FromNatural n Word8 where
    fromNatural = fromIntegral $ natVal (Proxy :: Proxy n)
