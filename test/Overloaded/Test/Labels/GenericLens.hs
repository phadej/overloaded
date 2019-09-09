{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Labels=Data.Generics.Product.Fields.field #-}
module Overloaded.Test.Labels.GenericLens where

import Control.Lens     (over, view)
import GHC.Generics     (Generic)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

data Person = Person
    { name :: String
    , age  :: Int
    , pet  :: Pet
    }
  deriving (Eq, Show, Generic)

data Pet = Pet
    { petName :: String
    , petAge  :: Int
    }
  deriving (Eq, Show, Generic)

sally :: Person
sally = Person
    { name = "Sally"
    , age  = 21
    , pet  = Pet
        { petName = "Bob"
        , petAge  = 3
        }
    }

sally' :: Person
sally' = Person
    { name = "Sally"
    , age  = 21
    , pet  = Pet
        { petName = "Bob"
        , petAge  = 4
        }
    }

newtype NES = NES String deriving (Eq, Show)
tests :: TestTree
tests = testGroup "Labels.GenericLens"
    [ testCase "view" $
        view (#pet . #petName) sally @?= "Bob"
    , testCase "over" $
        over (#pet . #petAge) succ sally @?= sally'
    ]
