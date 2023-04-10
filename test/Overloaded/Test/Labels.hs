{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Labels=Overloaded.Symbols.fromSymbol #-}
module Overloaded.Test.Labels where

import Data.Proxy       (Proxy (..))
import GHC.Exts         (Constraint)
import GHC.OverloadedLabels   (IsLabel (..))
import GHC.TypeLits
       (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Overloaded

newtype NES = NES String deriving (Eq, Show)

type family NotEmptySymbol (s :: Symbol) :: Constraint where
    NotEmptySymbol "" = TypeError ('ShowType NES ':<>: 'Text " should not be empty")
    NotEmptySymbol s  = ()

instance (KnownSymbol s, NotEmptySymbol s) => FromSymbol s NES where
    fromSymbol = NES $ symbolVal (Proxy :: Proxy s)

-- TODO: We should not need this instance, as we have configured the plugin
instance (KnownSymbol s, NotEmptySymbol s) => IsLabel s NES where
    fromLabel = fromSymbol @s

tests :: TestTree
tests = testGroup "Labels"
    [ testCase "NES" $
        #foo @?= NES "foo"
    ]
