{-# LANGUAGE Arrows           #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Categories=Overloaded.Categories.Constrained.identity #-}
module AD.Staged.Quad.Network where

import qualified Data.Type.Nat as N
import qualified Data.Vec.Lazy as V

import AD.Staged.Types
import Data.Type.Nat.PlusTree

-------------------------------------------------------------------------------
-- Quad
-------------------------------------------------------------------------------

quad :: AD ('Leaf N.Nat2) ('Leaf N.Nat1)
quad = proc x -> do
    y    <- dot                   -< (x, x)
    bias <- konst (V.singleton 5) -< x
    plus                          -< (y, bias)
