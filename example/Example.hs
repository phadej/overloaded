-- {-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Symbols #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Main (main) where

import           Data.Proxy         (Proxy (..))
import           Data.String        (IsString (..))
import           Data.Symbol.Ascii
import           Data.Type.Equality
import           GHC.TypeLits
import qualified GHC.TypeNats       as Nat
import           Numeric.Natural
import           Test.HUnit         ((@?=))

import Overloaded.Symbols

-- non-empty string
newtype S = S String deriving (Eq, Show)

instance IsString S where
    fromString = S

instance (KnownSymbol s, a ~ Char, (s == "") ~ 'False) => FromSymbol s S where
    fromSymbol = S $ symbolVal (Proxy :: Proxy s)

-- | Orphan instance for natural
instance (KnownNat (ReadNat s)) => FromSymbol s Natural where
    fromSymbol = Nat.natVal (Proxy :: Proxy (ReadNat s))

main :: IO ()
main = do
    -- Basics
    let lhs = "foo"
    print lhs
    lhs @?= S "foo"

    -- numbers
    let n = "2"
    print n
    1 + n @?= (3 :: Natural)
