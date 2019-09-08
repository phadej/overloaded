-- {-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Symbols -fplugin-opt=Overloaded:Numerals -fplugin-opt=Overloaded:Lists #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Main (main) where

import qualified Data.ByteString    as BS
import           Data.Fin           (Fin (..))
import           Data.Proxy         (Proxy (..))
import           Data.String        (IsString (..))
import           Data.Symbol.Ascii  (ReadNat)
import           Data.Type.Equality
import qualified Data.Type.Nat      as N
import           GHC.Exts           (Constraint)
import           GHC.TypeLits       (ErrorMessage (..), TypeError)
import           GHC.TypeLits
import qualified GHC.TypeNats       as Nat
import           Numeric.Natural
import           Test.HUnit         ((@?=))

import Overloaded.Numerals
import Overloaded.Symbols

-- non-empty string
newtype NES = NES String deriving (Eq, Show)

instance IsString NES where
    fromString = NES

instance (KnownSymbol s, a ~ Char, (s == "") ~ 'False) => FromSymbol s NES where
    fromSymbol = NES $ symbolVal (Proxy :: Proxy s)

-- | Orphan instance for natural
instance (KnownNat (ReadNat s)) => FromSymbol s Natural where
    fromSymbol = Nat.natVal (Proxy :: Proxy (ReadNat s))

type family IsLess (n :: N.Nat) (m :: N.Nat) (p :: Nat) (q :: Nat) :: Constraint where
    IsLess 'N.Z     ('N.S m) p q = ()
    IsLess ('N.S n) ('N.S m) p q = IsLess n m p q
    IsLess ('N.S n) 'N.Z     p q = TypeError ('ShowType p ':<>: 'Text " is not less than " ':<>: 'ShowType q)

class FinFromNatural (n :: N.Nat) (m :: N.Nat) where
    finFromNatural :: Proxy n -> Fin m

instance FinFromNatural 'N.Z ('N.S m) where
    finFromNatural _ = FZ

instance FinFromNatural n m => FinFromNatural ('N.S n) ('N.S m) where
    finFromNatural _ = FS (finFromNatural (Proxy :: Proxy n))

instance (FinFromNatural (N.FromGHC n) m, IsLess (N.FromGHC n) m n (N.ToGHC m)) => FromNatural n (Fin m) where
    fromNatural = finFromNatural (Proxy :: Proxy (N.FromGHC n))

main :: IO ()
main = do
    --Non-Empty String
    let lhs = "foo"
    print lhs
    lhs @?= NES "foo"

    -- ByteString
    let bs = "foo" -- try non-ASCII
    bs @?= BS.pack [102, 111, 111]

    -- Numbers
    let n = "2" -- try non digit
    print n
    1 + n @?= (3 :: Natural)

    let f :: Fin (N.FromGHC 5)
        f = 2

    print f
