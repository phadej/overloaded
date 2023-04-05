{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fplugin=Overloaded
            -fplugin-opt=Overloaded:RebindableApplication=$ #-}
module Overloaded.Test.RebindableApplications where

import Data.Char        (toUpper)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Prelude hiding (($))
import Overloaded.RebindableApplication (($), pureA, unA)

tests :: TestTree
tests = testGroup "RebindableApplication"
    [ testCase "Normal functional application" $ do
        map toUpper "foo" @?= "FOO"

    , testCase "Applicative via shadowed ($)" $ do
        let f = pure ((+) :: Int -> Int -> Int)
            x = Just 1
            y = Just 2

            z = let ($) = (<*>) in f x y

        z @?= Just 3
    , testCase "Applicative via A and Apply" $ do
        let f = pureA ((+) :: Int -> Int -> Int)
            x = Just 1
            y = Just 2

            -- this calls (<*>) implicitly thanks to ($) using the Apply typeclass
            z = unA $ f x y

        z @?= Just 3
    ]
