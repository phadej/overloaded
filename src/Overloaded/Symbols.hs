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

-- | Another way to desugar overloaded string literals. See 'FromSymbol'.
module Overloaded.Symbols (
    FromSymbol (..),
  ) where

import Data.Proxy         (Proxy (..))
import Data.String        (fromString)
import Data.Symbol.Ascii  (ToList)
import Data.Time.Calendar (Day, fromGregorian)
import GHC.Exts           (Constraint)
import GHC.TypeLits
       (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)
import GHC.TypeNats

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL

-- | Another way to desugar overloaded string literals using this class.
--
-- A string literal @"example"@ is desugared to
--
-- @
-- 'fromSymbol' \@"example"
-- @
--
-- Enabled with:
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Symbols #-}
-- @
--
class FromSymbol (s :: Symbol) a where
    fromSymbol :: a

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance (KnownSymbol s, a ~ Char) => FromSymbol s [a] where
    fromSymbol = symbolVal (Proxy :: Proxy s)

-------------------------------------------------------------------------------
-- text
-------------------------------------------------------------------------------

instance KnownSymbol s => FromSymbol s T.Text where
    fromSymbol = fromString (symbolVal (Proxy :: Proxy s))

instance KnownSymbol s => FromSymbol s TL.Text where
    fromSymbol = fromString (symbolVal (Proxy :: Proxy s))

-------------------------------------------------------------------------------
-- bytestring
-------------------------------------------------------------------------------

type family SeqList (xs :: [Symbol]) :: Constraint where
    SeqList '[]       = ()
    SeqList (x ': xs) = SeqList xs
    SeqList xs        = TypeError ('Text "Cannot reduce list " ':$$: 'ShowType xs)

instance (KnownSymbol s, SeqList (ToList s)) => FromSymbol s BS.ByteString where
    fromSymbol = fromString (symbolVal (Proxy :: Proxy s))

instance (KnownSymbol s, SeqList (ToList s)) => FromSymbol s BSL.ByteString where
    fromSymbol = fromString (symbolVal (Proxy :: Proxy s))

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

instance (KnownNat y, KnownNat m, KnownNat d, ParseDay s ~ '(y, m, d)) => FromSymbol s Day where
    fromSymbol = fromGregorian (integralVal @y) (integralVal @m) (integralVal @d)

integralVal :: forall n m. (KnownNat n, Num m) => m
integralVal = fromIntegral (natVal (Proxy :: Proxy n))

type family ParseDay (sym :: Symbol) :: (Nat, Nat, Nat) where
    ParseDay sym = ParseDay1 sym (ToList sym)

type a ** b = a GHC.TypeNats.* b
infixl 7 **

type family ParseDay1 (sym :: Symbol) (cs :: [Symbol]) :: (Nat, Nat, Nat) where
    ParseDay1 sym (y1 ': y2 ': y3 ': y4 ': "-" ': m1 ': m2 ': "-" ': d1 ': d2 :' [])
        = ParseDay2 sym (1000 ** TD y1 + 100 ** TD y2 + 10 ** TD y3 + TD y4) (10 ** TD m1 + TD m2)  (10 ** TD d1 + TD d2)
    ParseDay1 sym cs = TypeError ('ShowType sym ':<>: 'Text " doesn't look like a date (yyyy-mm-dd)")

-- To Digit
type family TD (c :: Symbol) :: Nat where
    TD "0" = 0
    TD "1" = 1
    TD "2" = 2
    TD "3" = 3
    TD "4" = 4
    TD "5" = 5
    TD "6" = 6
    TD "7" = 7
    TD "8" = 8
    TD "9" = 9
    TD c   = TypeError ('ShowType c ':<>: 'Text " is not a digit")

type family ParseDay2 (sym :: Symbol) (y :: Nat) (m :: Nat) (d :: Nat) :: (Nat, Nat, Nat) where
    ParseDay2 sym y m 0 = TypeError ('Text "Zero-day in " ':<>: 'ShowType sym)
    ParseDay2 sym y 0 d = TypeError ('Text "Zero-month in " ':<>: 'ShowType sym)
    ParseDay2 sym y m d = ParseDay3 sym y m d (d <=? DaysIn y m)

type family ParseDay3 (sym :: Symbol) (y :: Nat) (m :: Nat) (d :: Nat) (check :: Bool) :: (Nat, Nat, Nat) where
    ParseDay3 sym y m d 'True  = '(y, m , d)
    ParseDay3 sym y m d 'False = TypeError ('Text "There are only " ':<>: 'ShowType (DaysIn y m) ':<>: 'Text " days in year-of-month of " ':<>: 'ShowType sym)

type family DaysIn (y :: Nat) (m :: Nat) where
    DaysIn y  1 = 31
    DaysIn y  2 = Leap (Mod y 4) (Mod y 100) (Mod y 400)
    DaysIn y  3 = 31
    DaysIn y  4 = 30
    DaysIn y  5 = 31
    DaysIn y  6 = 30
    DaysIn y  7 = 31
    DaysIn y  8 = 31
    DaysIn y  9 = 30
    DaysIn y 10 = 31
    DaysIn y 11 = 30
    DaysIn y 12 = 31
    DaysIn y m = TypeError ('Text "Overflowed month " ':<>: 'ShowType m)

type family Leap (a :: Nat) (b :: Nat) (c :: Nat) :: Nat where
    Leap a b 0 = 29
    Leap a 0 c = 28
    Leap 0 b c = 29
    Leap a b c = 28
