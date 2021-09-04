{-# LANGUAGE Arrows    #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Categories=Overloaded.Categories.identity #-}
module Main where

import Overloaded.Categories

main :: IO ()
main = do
    print $ fromTriple $ Triple 'x' 'y' 'z'
    print $ toTriple (('u', 'v'), 'w')

data Triple a b c = Triple a b c
  deriving (Eq, Show)

fromTriple :: Triple a b c -> (a, (b, c))
fromTriple = proc (Triple a b c) -> identity -< (a, (b, c))

toTriple :: ((a, b), c) -> Triple a b c
toTriple = proc ((a, b), c) -> identity -< Triple a b c
