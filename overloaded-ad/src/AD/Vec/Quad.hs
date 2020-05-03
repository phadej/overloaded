{-# LANGUAGE Arrows           #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Categories=Overloaded.Categories.Constrained.identity #-}
module AD.Vec.Quad where

import Data.Vec.Lazy (Vec (..))

import qualified Data.Type.Nat as N
import qualified Data.Vec.Lazy as V

import AD.Vec.Types
import Data.Type.Nat.PlusTree

-------------------------------------------------------------------------------
-- Quad
-------------------------------------------------------------------------------

quad :: AD ('Leaf N.Nat2) ('Leaf N.Nat1)
quad = proc x -> do
    y    <- dot                   -< (x, x)
    bias <- konst (V.singleton 5) -< x
    plus                          -< (y, bias)

quadTest :: IO ()
quadTest = do
    putStrLn $ "quad          (2,3) = " ++ show (evaluateAD quad (2 ::: 3 ::: VNil))
    putStrLn $ "gradDesc quad (2,3) = " ++ show (gradDesc quad (2 ::: 3 ::: VNil) !! 30)
