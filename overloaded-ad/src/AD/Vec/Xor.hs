{-# LANGUAGE Arrows           #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Categories=Overloaded.Categories.Constrained.identity #-}
module AD.Vec.Xor where

import Data.Vec.Lazy (Vec (..))

import qualified Data.Type.Nat as N
import qualified Data.Vec.Lazy as V

import AD.Vec.Types
import Data.Type.Nat.PlusTree

-------------------------------------------------------------------------------
-- Xor
-------------------------------------------------------------------------------
