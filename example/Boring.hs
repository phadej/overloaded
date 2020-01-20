{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS -fplugin=Overloaded
            -fplugin-opt=Overloaded:Unit=Data.Boring.boring #-}
module Main (main) where

import Data.Constraint    (Dict (..))
import Data.Type.Equality ((:~:) (..))
import Data.Void          (Void)
import Test.HUnit         ((@?=))

main :: IO ()
main = do
    -- vanilla unit
    let ex1 :: ()
        ex1 = ()
    () @?= ex1

    -- Boring instances
    let ex2 :: [Void]
        ex2 = ()

    [] @?= ex2

    let ex3 :: Dict (Ord [(Int, Char)])
        ex3 = ()

    let ex4 :: (Int :~: Int, ())
        ex4 = ()
    (Refl, ()) @?= ex4
