{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Do #-}
module Overloaded.Test.Do where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Functor.Identity (Identity (..))

import Overloaded.Do
import IxMonad

tests :: TestTree
tests = testGroup "Do"
    [ testCase "Maybe" $ do
        ex1a @?= Just "xy"
        ex1b @?= Just "xy"
        ex1c @?= Just "xy"
        ex1d @?= Just "xy"
    , testCase "IxState" $ do
        execIxState ex2a 123 @?= "321"
        execIxState ex2b 123 @?= "321"
        execIxState ex2c 123 @?= "321"
        execIxState ex2d 123 @?= "321"
    ]

-------------------------------------------------------------------------------
-- Example 1
-------------------------------------------------------------------------------

ex1a :: Maybe String
ex1a = do
    x <- Just 'x'
    y <- Just 'y'
    pure [x, y]

ex1b :: Maybe String
ex1b =
    Just 'x' >>= \x ->
    Just 'y' >>= \y ->
    pure [x, y]

ex1c :: Maybe String
ex1c =
    monad @Bind (Just 'x') $ \x ->
    monad @Bind (Just 'y') $ \y ->
    monad @Pure [x, y]

ex1d :: Maybe String
ex1d = monad.do
    x <- Just 'x'
    y <- Just 'y'
    monad @Pure [x, y]

-------------------------------------------------------------------------------
-- Example 2
-------------------------------------------------------------------------------

ex2a :: IxStateT Identity Int String ()
ex2a =
    ixmodify show >>>= \_ ->
    ixmodify reverse

ex2b :: IxStateT Identity Int String ()
ex2b =
    ixmonad @Then (ixmodify show) $
    ixmodify reverse
    
ex2c :: IxStateT Identity Int String ()
ex2c = ixmonad.do
    ixmodify show
    ixmodify reverse
    
ex2d :: IxStateT Identity Int String ()
ex2d = ixmonad.do
    _unused <- ixmodify show
    ixmodify reverse
