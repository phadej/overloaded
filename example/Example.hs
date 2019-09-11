{-# LANGUAGE OverloadedLabels      #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Symbols:Numerals:Lists:If:Labels=Overloaded.Symbols.fromSymbol:TypeNats #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}
module Main (main) where

import qualified Data.ByteString        as BS
import           Data.Fin               (Fin (..))
import           Data.Proxy             (Proxy (..))
import           Data.SOP.BasicFunctors (I (..))
import           Data.SOP.NP            (NP (..), POP (..))
import           Data.String            (IsString (..))
import           Data.Symbol.Ascii      (ReadNat)
import           Data.Type.Equality
import qualified Data.Type.Nat          as N
import           Data.Vec.Lazy          (Vec (..))
import           GHC.Exts               (Constraint)
import           GHC.OverloadedLabels   (IsLabel (..))
import           GHC.TypeLits           (ErrorMessage (..), TypeError, KnownSymbol, KnownNat, symbolVal, Symbol)
import qualified GHC.TypeNats           as Nat
import           Numeric.Natural
import           Test.HUnit             ((@?=))

import Overloaded

-- non-empty string
newtype NES = NES String deriving (Eq, Show)

instance IsString NES where
    fromString = NES

type family NotEmptySymbol (s :: Symbol) :: Constraint where
    NotEmptySymbol "" = TypeError ('ShowType NES ':<>: 'Text " should not be empty")
    NotEmptySymbol s  = ()

instance (KnownSymbol s, NotEmptySymbol s) => FromSymbol s NES where
    fromSymbol = NES $ symbolVal (Proxy :: Proxy s)

-- We don't need this instance, as we have configured the plugin
-- instance (KnownSymbol s, NotEmptySymbol s) => IsLabel s NES where
--     fromLabel = fromSymbol @s

-- | Orphan instance for natural
instance (KnownNat (ReadNat s)) => FromSymbol s Natural where
    fromSymbol = Nat.natVal (Proxy :: Proxy (ReadNat s))



main :: IO ()
main = do
    --Non-Empty String
    let lhs = "foo"
    print lhs
    lhs @?= NES "foo"

    let lhs' = #foo
    print lhs
    lhs' @?= NES "foo"

    -- ByteString
    let bs = "foo" -- try non-ASCII
    bs @?= BS.pack [102, 111, 111]

    -- Numbers
    let n = "2" -- try non digit
    print n
    1 + n @?= (3 :: Natural)

    -- fin: Fin
    let f :: Fin 5
        f = 2

    print f
    f @?= FS (FS FZ)

    -- vec: Vec
    let v :: Vec N.Nat3 Int
        v = [1, 2, 3]

    print v
    v @?= 1 ::: 2 ::: 3 ::: VNil

    -- sop-core: NP and POP
    let np :: NP I '[Int, Bool, String]
        np = [I 1, I True, I "YES"]

    print np
    np @?= I 1 :* I True :* I "YES" :* Nil

    let pop :: POP I '[ '[Int, Bool], '[String] ]
        pop = [[I 0, I False], [I "NO"]]

    print pop
    pop @?= POP ((I 0 :* I False :* Nil) :* (I "NO" :* Nil) :* Nil)

    -- Overloaded:If
    (if True then 'x' else 'y') @?= ('x' :: Char)
    (if Just 'b' then 'x' else 'y') @?= ('x' :: Char)


