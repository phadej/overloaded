{-# LANGUAGE GADTs             #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -ddump-splices #-}
module AD.Staged.Quad where

import Data.Vec.Lazy (Vec (..))

import AD.Staged.Types
import AD.Staged.Quad.Network (quad)
import Data.Type.Nat.PlusTree

import qualified Data.Type.Nat    as N

-------------------------------------------------------------------------------
-- Quad
-------------------------------------------------------------------------------

evalQuad :: Double -> Double -> IO ()
evalQuad x y =
    $$(evaluateAD quad (Dyn [|| x ||] ::: Dyn [|| y ||] ::: VNil) $ \(v ::: VNil) ((dx ::: dy ::: VNil) ::: VNil) -> [||
        putStrLn $ unwords
          [ "quad"
          , show (x, y)
          , "="
          , show $$(stoCode v)
          , show ($$(stoCode dx), $$(stoCode dy))
          ]
        ||])

evalTanh :: Double -> IO ()
evalTanh x =
    $$(evaluateAD (tanhAD @('Leaf N.Nat1)) (Dyn [|| x ||] ::: VNil) $ \(v ::: VNil) ((dx ::: VNil) ::: VNil) -> [||
        putStrLn $ unwords
          [ "tanh"
          , show x
          , "="
          , show $$(stoCode v)
          , show $$(stoCode dx)
          ]
        ||])

quadTest :: IO ()
quadTest = do
    -- this is evaluated compile time.
    $$(evaluateAD quad (1 ::: 2 ::: VNil) $ \(v ::: VNil) ((dx ::: dy ::: VNil) ::: VNil) -> [||
        putStrLn $ unwords
          [ "quad"
          , "(1.0, 2.0)"
          , "="
          , show $$(stoCode v)
          , show ($$(stoCode dx), $$(stoCode dy))
          ]
        ||])

    evalQuad 2 3
    evalTanh 1
