{-# LANGUAGE DataKinds #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:TypeSymbols #-}
module Overloaded.Test.TypeSymbols where

import Data.Singletons.Bool (SBool (..))
import Regexp.Type          (Matches)

example :: SBool (Matches "(foo|bar*)*q[u-w]x" "fooqux")
example = STrue
