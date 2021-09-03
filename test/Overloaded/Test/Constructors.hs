{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS
    -fplugin=Overloaded
    -fplugin-opt=Overloaded:Constructors=build
    -dcore-lint
  #-}
module Overloaded.Test.Constructors where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Overloaded.Constructors

tests :: TestTree
tests = testGroup "Constructors"
    [ testGroup "build"
        [ testCase "Left" $
            build @"Left" ('x') @?= (Left 'x' :: Either Char Int)

        , testCase "Just" $
            build @"Just" ('x') @?= Just 'x'

        , testCase "Foobar Foo" $
            build @"Foo" () @?= Foo

        , testCase "Foobar Bar" $
            build @"Bar" ("here", 4.2) @?= Bar "here" 4.2
        ]

    , testGroup "match"
        [ testCase "Just" $ do
            match @"Just" (Nothing :: Maybe Char) @?= Nothing
            match @"Just" (Just 'x':: Maybe Char) @?= Just 'x'

        , testCase "Foobar Foo" $ do
            match @"Foo" Foo              @?= Just ()
            match @"Foo" (Bar "here" 4.2) @?= Nothing

        , testCase "Foobar Bar" $ do
            match @"Bar" Foo              @?= Nothing
            match @"Bar" (Bar "here" 4.2) @?= Just ("here", 4.2)
        ]

    -- Source plugin rewrites (:name arg1 arg2 arg3)
    -- into build @"name" (arg1, arg2, arg3).
    , testGroup "Overloaded build"
        [ testCase "Left" $
            (:Left 'x') @?= (Left 'x' :: Either Char Int)

        , testCase "Foobar" $ do
            (:Foo)            @?= Foo
            (:Bar "here" 4.2) @?= Bar "here" 4.2
        ]
    ]

data Foobar
    = Foo
    | Bar String Rational
  deriving (Eq, Show)
