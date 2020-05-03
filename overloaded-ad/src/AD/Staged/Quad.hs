{-# LANGUAGE GADTs             #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -ddump-splices #-}
module AD.Staged.Quad where

import Data.Vec.Lazy (Vec (..))

import AD.Staged.Types
import AD.Staged.Quad.Network (quad)

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
