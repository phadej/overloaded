{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:CodeLabels #-}
module Overloaded.Test.CodeLabels where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Overloaded
import Overloaded.Test.CodeLabels.String

tests :: TestTree
tests = testGroup "Labels"
    [ testCase "NES" $ #foo @?= "FOO"
    ]
