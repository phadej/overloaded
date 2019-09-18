{-# OPTIONS_GHC -fplugin=Overloaded -fplugin-opt=Overloaded:RecordFields #-}
{-# OPTIONS_GHC -dcore-lint #-}
-- {-# OPTIONS_GHC -ddump-simpl #-}
{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Test.HUnit ((@?=))

import GHC.Records.Compat
import GHC.Records.Extra

main :: IO ()
main = do
    -- Overloaded:RecordFields
    let bob = MkCat "Bob" (3 :: Int) ""
    getField @"petName" bob @?= "Bob"
    setField @"petExtra" bob "foo" @?= bob { petExtra = "foo" }
    modifyField @"petAge" bob succ @?= bob { petAge = 4 }

    print $ getField @"petName" bob

data Pet u = MkCat
    { petName  :: String
    , petAge   :: Int
    , petExtra :: u
    }
  deriving (Eq, Show)
