{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:If #-}
module Overloaded.Test.If where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "If"
    [ testCase "Bool" $
        (if True then 'x' else 'y')  @?= 'x'
    , testCase "Either" $
        (if Left () then 'x' else 'y')  @?= 'y'
    ]
