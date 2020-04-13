{-# LANGUAGE Arrows #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Categories #-}
module Overloaded.Test.Categories where

import Data.Bifunctor.Assoc  (assoc)
import Test.QuickCheck       ((===))
import Test.QuickCheck.Poly  (A, B, C)
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.HUnit      (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import Overloaded.Categories
import AD
import STLC

tests :: TestTree
tests = testGroup "Categories"
    [ testGroup "Basic tests"
        [ testCase "Category" $ do
            let lhs = proc x -> do
                    y <- identity -< x
                    identity -< y
                rhs = id
            lhs 'x' @?= rhs 'x'

        , testCase "Product expession" $ do
            let lhs = proc x -> do
                    y <- identity -< (x, x)
                    identity -< (y, x)
                rhs = \x -> ((x,x),x)
            lhs 'x' @?= rhs 'x'

        , testCase "Wild pattern" $ do
            let lhs = proc x -> do
                  _ <- identity -< x
                  identity -< x
                rhs = id
            lhs 'x' @?= rhs 'x'

        , testCase "Product pattern" $ do
            let lhs = proc x -> do
                    (y, _) <- identity -< x
                    (z, _) <- identity -< y
                    identity -< z
                rhs = fst . fst
                test = (('x', 'y'), 'z')
            lhs test @?= rhs test
        ]
    , testProperty "assoc (->)" $ \a b c ->
        let abc = ((a, b), c) :: ((A, B), C)
        in assoc abc === catAssoc abc

    , testCase "assoc Mapping" $ do
        let M rhs = catAssoc
            lhs = "Lam (Pair (Fst (Fst (Var Here))) (Pair (Snd (Fst (Var Here))) (Snd (Var Here))))"
        -- writing Eq instance for Term is not nice :)
        show rhs @?= lhs

    , testCase "AD" $ do
        evaluateAD quad (0, 0) [(1,0), (0,1), (1, 1)] @?= (0 :: Int, [0,0,0])
        evaluateAD quad (1, 2) [(1,0), (0,1), (1, 1)] @?= (5 :: Int, [2,4,6])
    ]

catAssoc
    :: CategoryProduct cat
    => cat (Product cat (Product cat a b) c) (Product cat a (Product cat b c))
catAssoc = proc ((x, y), z) -> identity -< (x, (y, z))

quad :: Num a => AD (a, a) a
quad = proc (x, y) -> do
    x2 <- mult -< (x, x)
    y2 <- mult -< (y, y)
    plus -< (x2, y2)
