{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fplugin=Overloaded
                -fplugin-opt=Overloaded:Lists=Overloaded.Lists.Bidi.nil=Overloaded.Lists.Bidi.cons
  #-}
module Overloaded.Test.Lists.Bidi where

import Data.SOP.BasicFunctors (I (..))
import Data.SOP.NP            (NP (..), POP (..))
import Data.Vec.Lazy          (Vec (..))
import Test.Tasty             (TestTree, testGroup)
import Test.Tasty.HUnit       (testCase, (@?=))

import qualified Data.Set      as Set
import qualified Data.Type.Nat as N

import Overloaded.Lists.Bidi

int :: Int
int = 1

tests :: TestTree
tests = testGroup "Lists"
    [ testCase "[]" $
        [1,2,3] @?= ([1,2,3] :: [Int])

    , testCase "Vec" $
        [1,2,3] @?= int ::: 2 ::: 3 ::: VNil

-- Patterns not supported
--    , testCase "Vec pattern-match" $do
--        let res = case [1,2,3] :: Vec N.Nat3 Int of
--                [x,y,z] -> x + y + z
--
--        res @?= 6

    , testCase "NP" $ do
        let np :: NP I '[Int, Bool, String]
            np = [I 1, I True, I "YES"]

        np @?= I 1 :* I True :* I "YES" :* Nil

    , testCase "POP" $ do
        let pop :: POP I '[ '[Int, Bool], '[String] ]
            pop = [[I 0, I False], [I "NO"]]

        pop @?= POP ((I 0 :* I False :* Nil) :* (I "NO" :* Nil) :* Nil)

    , testCase "Set" $ do
        let s :: Set.Set Char
            s = ['f', 'o', 'o']

        s @?= Set.fromList ['o', 'f']

        s @?= fromList ['o', 'f']
    ]

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

_vecTest00 :: Vec 'N.Z Int
_vecTest00 = nil

-- check bidi-inference
_vecTest01 = 1 `cons` 2 `cons` _vecTest00
