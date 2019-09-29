{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -fplugin=Overloaded -fplugin-opt=Overloaded:IdiomBrackets #-}
module Main (main) where

import Data.Traversable (foldMapDefault)

-------------------------------------------------------------------------------
-- Traversable looks like Functor
-------------------------------------------------------------------------------

data Tree a
    = Leaf a
    | Branch (Tree a) (Tree a)
  deriving (Show)

instance Functor Tree where
    fmap f (Leaf x)     = Leaf (f x)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Traversable Tree where
    traverse f (Leaf x)     = [| Leaf (f x) |]
    traverse f (Branch l r) = [| Branch (traverse f l) (traverse f r) |]

instance Foldable Tree where
    foldMap = foldMapDefault

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    -- traverse a tree
    let tree = Branch (Branch (Leaf 'x') (Leaf 'y')) (Leaf 'z')
    tree' <- traverse print tree
    print tree'

    -- operators
    let xs :: [Int]
        xs = [| [1..4] * [1..4] |]

    print xs
