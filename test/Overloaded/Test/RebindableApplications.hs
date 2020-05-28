{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fplugin=Overloaded
            -fplugin-opt=Overloaded:RebindableApplication #-}
module Overloaded.Test.RebindableApplications where

import Data.Char        (toUpper)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Overloaded

tests :: TestTree
tests = testGroup "RebindableApplication"
    [ testCase "Normal functional application" $ do
        map toUpper "foo" @?= "FOO"

    , testCase "Applicative" $ do
        let f = pure ((+) :: Int -> Int -> Int)
            x = Just 1
            y = Just 2
        
            z = let ($) = (<*>) in f x y

        z @?= Just 3
    ]
