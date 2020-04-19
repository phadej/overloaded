{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS -fplugin=Overloaded
            -fplugin-opt=Overloaded:Lists
            -Wno-type-defaults
  #-}
module Overloaded.Test.Lists where

import Data.List.NonEmpty     (NonEmpty (..))
import Data.SOP.BasicFunctors (I (..))
import Data.SOP.NP            (NP (..), POP (..))
import Data.Vec.Lazy          (Vec (..))
import Test.Tasty             (TestTree, testGroup)
import Test.Tasty.HUnit       (testCase, (@?=))

import qualified Data.Map            as Map
import qualified Data.RAVec          as RAV
import qualified Data.RAVec.NonEmpty as NERAV
import qualified Data.Set            as Set
import qualified Data.Type.Nat       as N

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

    , testCase "RAVec" $
        RAV.toList ['x','y','z'] @?= "xyz"

    , testCase "RAVec.NonEmpty" $
        NERAV.toNonEmpty ['x','y','z'] @?= ('x' :| "yz")

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
            m = [(1, 'x'), (3, 'y'), (2, 'z')]

        m @?= Map.fromList [(1,'x'),(2,'z'),(3,'y')]

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
_vecTest01 = as2 @Vec $ 1 `cons` 2 `cons` _vecTest00

-- GHC doesn't infer this type, though it could?
-- On the other hand
--
--    inferenceTestStr = fromString "foo"
--
-- doesn't work out in the source files either: MonomorphismRestriction
-- In GHCi things work, 
--
-- @
-- *Overloaded> :t True `cons` 'a' `cons` nil
-- True `cons` 'a' `cons` nil
--   :: (Cons Bool ys1 zs, Cons Char ys2 ys1, Nil ys2) => zs
-- @
--
inferenceTest :: (Nil xs, Cons x xs ys, Cons y ys zs, Num x, Num y) => zs
inferenceTest = [1, 2]

_inferenceList = as1 @[] inferenceTest
_inferenceVec  = as2 @Vec inferenceTest

-------------------------------------------------------------------------------
-- Map inline: weird thing to do
-------------------------------------------------------------------------------

newtype M k v = M { unM :: Map.Map k v }
  deriving (Eq, Show)

newtype M' k v = M' (k -> Map.Map k v)

instance Nil (M k v) where
    nil = M Map.empty

instance Ord k => Cons v (M k v) (M' k v) where
    cons v (M m) = M' (\k -> Map.insert k v m)

instance Cons k (M' k v) (M k v) where
    cons k (M' km) = M (km k)

-------------------------------------------------------------------------------
-- As
-------------------------------------------------------------------------------

as1 :: tycon a -> tycon a
as1 = id

as2 :: tycon a b -> tycon a b
as2 = id
