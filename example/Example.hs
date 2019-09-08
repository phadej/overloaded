-- {-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Symbols -fplugin-opt=Overloaded:Numerals #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Main (main) where

import           Data.Proxy         (Proxy (..))
import           Data.String        (IsString (..))
import           Data.Symbol.Ascii (ReadNat)
import           Data.Type.Equality
import           GHC.TypeLits
import qualified GHC.TypeNats       as Nat
import           Numeric.Natural
import           Test.HUnit         ((@?=))
import qualified Data.ByteString as BS

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
