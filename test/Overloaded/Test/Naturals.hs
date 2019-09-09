{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Naturals #-}
module Overloaded.Test.Naturals where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Numeric.Natural (Natural)

tests :: TestTree
tests = testGroup "Naturals"
    [ testCase "Natural" $
        2 @?= (2 :: Natural)
    , testCase "Integer" $
        3 @?= (3 :: Integer)
    ]
