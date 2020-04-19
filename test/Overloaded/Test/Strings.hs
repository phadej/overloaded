{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Strings #-}
module Overloaded.Test.Strings where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Text (pack)

tests :: TestTree
tests = testGroup "Strings"
    [ testCase "String" $
        "Example" @?= ("Example" :: String)
    , testCase "Text" $
        "Example" @?= pack "Example"
    ]
