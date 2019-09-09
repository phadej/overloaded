{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Chars #-}
module Overloaded.Test.Chars where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "Chars"
    [ testCase "Char" $
        'x' @?= ('x' :: Char)
    ]
