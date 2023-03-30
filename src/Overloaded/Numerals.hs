{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Another way to desugar overloaded numeric literals. See 'FromNumeral'.
module Overloaded.Numerals (
    FromNumeral (..),
    defaultFromNumeral,
    ) where

import Data.Bin.Pos    (Pos (..))
import Data.BinP.PosP  (PosP (..))
import Data.Fin        (Fin (..))
import Data.Proxy      (Proxy (..))
import Data.Word       (Word16, Word32, Word64, Word8)
import GHC.Exts        (Constraint)
import GHC.TypeLits    (ErrorMessage (..), Symbol, TypeError)
import GHC.TypeNats    (KnownNat, Nat, natVal, type (<=?))
import Numeric.Natural (Natural)

import qualified Data.Bin       as B
import qualified Data.BinP.PosP as PP
import qualified Data.Type.Bin  as B
import qualified Data.Type.BinP as BP
import qualified Data.Type.Nat  as N

-- | Another way to desugar numerals.
--
-- A numeric literal @123@ is desugared to
--
-- @
-- 'fromNumeral' \@123
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
class FromNumeral (n :: Nat) a where
    fromNumeral :: a

-- | Default implementation of 'fromNumeral'.
--
-- Usage example:
--
-- @
-- instance ('KnownNat' n, ...) => 'FromNumeral' n MyType where
--     'fromNumeral' = 'defaultFromNumeral' @n
-- @
--
defaultFromNumeral :: forall n a. (KnownNat n, Integral a) => a
defaultFromNumeral = fromIntegral $ natVal (Proxy :: Proxy n)

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance KnownNat n => FromNumeral n Natural where
    fromNumeral = natVal (Proxy :: Proxy n)

instance KnownNat n => FromNumeral n Integer where
    fromNumeral = defaultFromNumeral @n

-- | TODO: currently there is no range check
instance KnownNat n => FromNumeral n Int where
    fromNumeral = defaultFromNumeral @n

-------------------------------------------------------------------------------
-- Word
-------------------------------------------------------------------------------

type family OverflowCheck (n :: Nat) (t :: Symbol) (b :: Bool) :: Constraint where
    OverflowCheck n t 'True  = ()
    OverflowCheck n t 'False = TypeError
        ('Text "Overflowing " ':<>: 'Text t ':<>: 'Text ": " ':<>: 'ShowType n)

instance (KnownNat n, OverflowCheck n "Word8" (n <=? 0xff)) => FromNumeral n Word8 where
    fromNumeral = defaultFromNumeral @n

instance (KnownNat n, OverflowCheck n "Word8" (n <=? 0xffff)) => FromNumeral n Word16 where
    fromNumeral = defaultFromNumeral @n

instance (KnownNat n, OverflowCheck n "Word8" (n <=? 0xffffffff)) => FromNumeral n Word32 where
    fromNumeral = defaultFromNumeral @n

instance (KnownNat n, OverflowCheck n "Word8" (n <=? 0xffffffffffffffff)) => FromNumeral n Word64 where
    fromNumeral = defaultFromNumeral @n

-------------------------------------------------------------------------------
-- fin
-------------------------------------------------------------------------------

instance KnownNat n => FromNumeral n N.Nat where
    fromNumeral = defaultFromNumeral @n

type family IsLess (p :: Nat) (q :: Nat) :: Constraint where
    IsLess p q = IsLess' (q <=? p) p q

type family IsLess' (b :: Bool) (p :: Nat) (q :: Nat) :: Constraint where
    IsLess' 'False p q = ()
    IsLess' 'True  p q = TypeError ('ShowType p ':<>: 'Text " is not less than " ':<>: 'ShowType q)

class FinFromNumeral (n :: N.Nat) (m :: N.Nat) where
    finFromNumeral :: Proxy n -> Fin m

instance FinFromNumeral 'N.Z ('N.S m) where
    finFromNumeral _ = FZ

instance FinFromNumeral n m => FinFromNumeral ('N.S n) ('N.S m) where
    finFromNumeral _ = FS (finFromNumeral (Proxy :: Proxy n))

instance (FinFromNumeral (N.FromGHC n) m, IsLess n (N.ToGHC m)) => FromNumeral n (Fin m) where
    fromNumeral = finFromNumeral (Proxy :: Proxy (N.FromGHC n))

-------------------------------------------------------------------------------
-- bin
-------------------------------------------------------------------------------

instance KnownNat n => FromNumeral n B.Bin where
    fromNumeral = defaultFromNumeral @n

instance (KnownNat n, (1 <=? n) ~ 'True) => FromNumeral n B.BinP where
    fromNumeral = defaultFromNumeral @n

class PosFromNumeral (n :: N.Nat) (b :: B.BinP) where
    posFromNumeral:: Proxy n -> PosP b

instance B.SBinPI b => PosFromNumeral 'N.Z b where
    posFromNumeral _ = PP.top

instance (B.SBinPI bp, PosFromNumeral n bp, B.Pred b ~ 'B.BP bp, BP.Succ bp ~ b) => PosFromNumeral ('N.S n) b where
    posFromNumeral _ = PP.pop (posFromNumeral (Proxy :: Proxy n))

instance (PosFromNumeral (N.FromGHC n) b, IsLess n (BP.ToGHC b)) => FromNumeral n (PosP b) where
    fromNumeral = posFromNumeral (Proxy :: Proxy (N.FromGHC n))

instance (PosFromNumeral (N.FromGHC n) b, IsLess n (BP.ToGHC b)) => FromNumeral n (Pos ('B.BP b)) where
    fromNumeral = Pos (posFromNumeral (Proxy :: Proxy (N.FromGHC n)))
