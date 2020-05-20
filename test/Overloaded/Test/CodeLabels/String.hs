{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses, TemplateHaskellQuotes #-}
module Overloaded.Test.CodeLabels.String where

import Overloaded
import GHC.TypeLits (KnownSymbol)

instance (a ~ Char, KnownSymbol str) => IsCodeLabel str [a] where
    codeFromLabel = [|| "FOO" ||]
