{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS
    -fplugin=Overloaded
    -fplugin-opt=Overloaded:RecordFields:Labels=Optics.HasField.field
  #-}
module Overloaded.Test.RecordFields where

import Optics.Core      (over, view)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "RecordFields"
    []
    -- TODO broken
    -- [ testCase "view" $
    --     view #petName bob @?= "bob"
    -- , testCase "over" $
    --     over #petAge succ bob @?= bob { petAge = 4 }
    -- ]

data Pet u = MkPet
    { petName  :: String
    , petAge   :: Int
    , petExtra :: u
    }
  deriving (Eq, Show)

bob :: Pet ()
bob = MkPet "bob" 3 ()
