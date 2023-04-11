{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:CodeStrings #-}
module Overloaded.Test.CodeStrings where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Data.ByteString as BS

import Overloaded ()

tests :: TestTree
tests = testGroup "Labels"
    [ testCase "String" $ do
        -- we seem to need type annotations :(
        -- https://gitlab.haskell.org/ghc/ghc/issues/18211
        ("FOO" :: String) @?= ("FOO" :: String)
        ("BAR" :: String) @?= ("BAR" :: String)

    , testCase "ByteString" $ do
        ("FOO"     :: BS.ByteString) @?= BS.pack [70,79,79]
        ("FOO\313" :: BS.ByteString) @?= BS.pack [70,79,79,57]
    ]
