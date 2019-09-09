{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Another way to desugar overloaded numeric literals. See 'FromNatural'.
module Overloaded.Numerals (
    FromNatural (..),
    defaultFromNatural,
    ) where

import Data.Proxy      (Proxy (..))
import Data.Word       (Word16, Word32, Word64, Word8)
import GHC.Exts        (Constraint)
import GHC.TypeLits    (ErrorMessage (..), Symbol, TypeError)
import GHC.TypeNats    (type (<=?), KnownNat, Nat, natVal)
import Numeric.Natural (Natural)

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

-- | Default implementation of 'fromNatural'. 
--
-- Usage example:
--
-- @
-- instance (KnownNat n, ...) => FromNatural n MyType where
--     fromNatural = defaultFromNatural @n
-- @
--
defaultFromNatural :: forall n a. (KnownNat n, Integral a) => a
defaultFromNatural = fromIntegral $ natVal (Proxy :: Proxy n)

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance KnownNat n => FromNatural n Natural where
    fromNatural = natVal (Proxy :: Proxy n)

instance KnownNat n => FromNatural n Integer where
    fromNatural = defaultFromNatural @n

-- | TODO: currently no range check
instance KnownNat n => FromNatural n Int where
    fromNatural = defaultFromNatural @n

-------------------------------------------------------------------------------
-- Word
-------------------------------------------------------------------------------

type family OverflowCheck (n :: Nat) (t :: Symbol) (b :: Bool) :: Constraint where
    OverflowCheck n t 'True  = ()
    OverflowCheck n t 'False = TypeError
        ('Text "Overflowing " ':<>: 'Text t ':<>: 'Text ": " ':<>: 'ShowType n)

instance (KnownNat n, OverflowCheck n "Word8" (n <=? 0xff)) => FromNatural n Word8 where
    fromNatural = defaultFromNatural @n

instance (KnownNat n, OverflowCheck n "Word8" (n <=? 0xffff)) => FromNatural n Word16 where
    fromNatural = defaultFromNatural @n

instance (KnownNat n, OverflowCheck n "Word8" (n <=? 0xffffffff)) => FromNatural n Word32 where
    fromNatural = defaultFromNatural @n

instance (KnownNat n, OverflowCheck n "Word8" (n <=? 0xffffffffffffffff)) => FromNatural n Word64 where
    fromNatural = defaultFromNatural @n
