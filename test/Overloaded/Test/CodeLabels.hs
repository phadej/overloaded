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
    [
    -- TODO: can't find instance: IsLabel "foo" String
    -- testCase "String" $ do
        -- we need type annotations :(
        -- https://gitlab.haskell.org/ghc/ghc/issues/18211
        -- (#foo :: String) @?= ("FOO" :: String)
        -- (#bar :: String) @?= ("BAR" :: String)
    ]
