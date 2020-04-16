{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Do #-}
module Main (main) where

import Data.Kind (Type)
import Overloaded
import System.Timeout (timeout)
import Data.Maybe (fromMaybe)

-- Idea / example by Vladislav Zavialov (int-inded) from:
-- https://github.com/ghc-proposals/ghc-proposals/pull/216#issuecomment-614771416

main :: IO ()
main = do
    putStrLn "Enter string, you have 10 seconds..."
    str <- fromMaybe "timed out..." <$> timeout 10000000 getLine
    let customIO :: forall (method :: DoMethod) ty. CustomIO method ty => ty 
        customIO = makeCustomIO @method @ty str
    customIO.do
        putStrLn "Hello"
        putStrLn "World"

-------------------------------------------------------------------------------
-- CustomDo
-------------------------------------------------------------------------------

class CustomIO (method :: DoMethod) (ty :: Type) where
    makeCustomIO :: String -> ty

instance (ty ~ (a -> IO a)                  ) => CustomIO 'Pure ty where
    makeCustomIO _   = pure
instance (ty ~ (IO a -> IO b -> IO b)       ) => CustomIO 'Then ty where
    makeCustomIO str x y = do
        _ <- x
        putStrLn $ "--- " ++ str ++ " ---"
        y
instance (ty ~ (IO a -> (a -> IO b) -> IO b)) => CustomIO 'Bind ty where
    makeCustomIO str m k = do
        x <- m
        putStrLn $ "--- " ++ str ++ " ---"
        k x
