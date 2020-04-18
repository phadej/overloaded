{-# LANGUAGE Arrows #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Categories #-}
module Overloaded.Test.Categories where

import Data.Bifunctor.Assoc  (assoc)
import Data.Bifunctor.Swap (swap)
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

        , testCase "Coproduct expression" $ do
            let lhs = proc x -> identity -< Left x
                rhs :: a -> Either a ()
                rhs = Left
                test = 'x'
            lhs test @?= rhs test

        ]
    , testProperty "assoc (->)" $ \abc ->
        assoc abc === catAssoc (abc :: ((A, B), C))

    , testCase "assoc Mapping" $ do
        let M rhs = catAssoc
            lhs = "Lam (Pair (Fst (Fst (Var Here))) (Pair (Snd (Fst (Var Here))) (Snd (Var Here))))"
        -- writing Eq instance for Term is not nice :)
        show rhs @?= lhs

    , testProperty "assocCo (->)" $ \abc ->
        assoc abc === catAssocCo (abc :: Either (Either A B) C)

    , testCase "assocCo Mapping" $ do
        let M rhs = catAssocCo
            lhs = "Lam (Case (Case (InL (Var Here)) (InR (InL (Var Here))) (Var Here)) (InR (InR (Var Here))) (Var Here))"
        show rhs @?= lhs

    , testCase "uncurry Mapping" $ do
        let M rhs = catUncurry
            lhs = "Lam (Lam (App (App (Var (There Here)) (Fst (Var Here))) (Snd (Var Here))))"
        show rhs @?= lhs

    , testCase "AD" $ do
        evaluateAD quad (0, 0) [(1,0), (0,1), (1, 1)] @?= (0 :: Int, [0,0,0])
        evaluateAD quad (1, 2) [(1,0), (0,1), (1, 1)] @?= (5 :: Int, [2,4,6])
    ]

catAssoc
    :: CartesianCategory cat
    => cat (Product cat (Product cat a b) c) (Product cat a (Product cat b c))
catAssoc = proc ((x, y), z) -> identity -< (x, (y, z))

catSwapCo
    :: BicartesianCategory cat
    => cat (Coproduct cat a b) (Coproduct cat b a)
-- catSwapCo =
--     fanin (inr ## proj1) (inl ## proj1) ## (distr ## fanout identity identity)
catSwapCo = proc xy -> case xy of
    Left x  -> identity -< Right x
    Right y -> identity -< Left y

catAssocCo
     :: BicartesianCategory cat
     => cat (Coproduct cat (Coproduct cat a b) c) (Coproduct cat a (Coproduct cat b c))
catAssocCo = proc xyz -> case xyz of
    Left xy     -> case xy of
        Left x  -> identity -< Left x
        Right y -> identity -< Right (Left y)
    Right z     -> identity -< Right (Right z)

catUncurry
    :: CCC cat
    => cat (Exponential cat a (Exponential cat b c))
           (Exponential cat (Product cat a b) c)
catUncurry = transpose $ proc (f, (a, b)) -> do
    bc <- f -<< a
    bc -<< b
    
quad :: Num a => AD (a, a) a
quad = proc (x, y) -> do
    x2 <- mult -< (x, x)
    y2 <- mult -< (y, y)
    plus -< (x2, y2)

-------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------

-- err01 = proc x -> case x of
--     Left z -> identity -< z

err01 :: BicartesianCategory cat => cat (Coproduct cat a a) a
err01 = proc z -> case z of
    Right x -> identity -< x
    Left y -> identity -< y
