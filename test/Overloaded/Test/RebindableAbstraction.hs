{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -fplugin=Overloaded
            -fplugin-opt=Overloaded:RebindableAbstraction=lam #-}
module Overloaded.Test.RebindableAbstraction where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Overloaded.RebindableAbstraction (Lam(lam))
import Overloaded.RebindableApplication (unA, A, apply)

genericInc :: Lam f Int Int => f
genericInc = \x -> x + 1

tests :: TestTree
tests = testGroup "RebindableAbstraction"
    [ testCase "genericInc for (->)" $ do
        genericInc 1 @?= 2
    , testCase "genericInc for A Maybe" $
        -- `apply` here could be omitted with RebindableApplication.
        -- Being polymorphic in the Lam instance can require giving the type
        -- manually later on, unless passing it to a higher-order function.
        unA (genericInc @(A Maybe (Int -> Int)) `apply` Just 1) @?= Just 2
    ]