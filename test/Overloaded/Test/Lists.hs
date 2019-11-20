{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Lists #-}
module Overloaded.Test.Lists where

import Data.Coerce (coerce)
import Data.List.NonEmpty     (NonEmpty (..))
import Data.SOP.BasicFunctors (I (..))
import Data.SOP.NP            (NP (..), POP (..))
import Data.Vec.Lazy          (Vec (..))
import Test.Tasty             (TestTree, testGroup)
import Test.Tasty.HUnit       (testCase, (@?=))

import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Type.Nat as N

import Overloaded.Lists

int :: Int
int = 1

tests :: TestTree
tests = testGroup "Lists"
    [ testCase "[]" $
        [1,2,3] @?= ([1,2,3] :: [Int])

    , testCase "NonEmpty" $
        [1,2,3] @?= int :| [2,3]

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

    , testCase "Map inline" $ do
        let m :: Map.Map Int Char
            m = unM [1, 'x', 3, 'y', 2, 'z']

        m @?= Map.fromList [(1,'x'),(2,'z'),(3,'y')]

    , testCase "Map pairs" $ do
        let m :: Map.Map Int Char
            m = unN [(1, 'x'), (3, 'y'), (2, 'z')]

        m @?= Map.fromList [(1,'x'),(2,'z'),(3,'y')]

    , testCase "Set" $ do
        let s :: Set.Set Char
            s = ['f', 'o', 'o']

        s @?= Set.fromList ['o', 'f']

        s @?= fromList ['o', 'f']
    ]

-------------------------------------------------------------------------------
-- Map inline
-------------------------------------------------------------------------------

newtype M k v = M { unM :: Map.Map k v }
  deriving (Eq, Show)

data M' k v = M' v (Map.Map k v)

instance Nil (M k v) where
    nil = M Map.empty
    null = Map.null . unM

instance Cons v (M k v) (M' k v) where
    cons v (M m) = M' v m

    uncons (M' v m) = Just (v, M m)

instance Ord k => Cons k (M' k v) (M k v) where
    cons k (M' v m) = M (Map.insert k v m)

    uncons (M m) = case Map.minViewWithKey m of
        Nothing           -> Nothing
        Just ((k, v), m') -> Just (k, M' v m')

-------------------------------------------------------------------------------
-- Map pairs
-------------------------------------------------------------------------------

newtype N k v = N { unN :: Map.Map k v }
  deriving (Eq, Show)

instance Nil (N k v) where
    nil = N Map.empty
    null = Map.null . unN

instance Ord k => Cons (k,v) (N k v) (N k v) where
    cons (k,v) (N m) = N (Map.insert k v m)
    uncons (N m) = fmap (fmap N) (Map.minViewWithKey m)
