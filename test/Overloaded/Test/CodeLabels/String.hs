{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Overloaded.Test.CodeLabels.String where

import Data.Char                  (toUpper)
import Data.Proxy                 (Proxy (..))
import GHC.TypeLits               (KnownSymbol, symbolVal)
import Language.Haskell.TH.Syntax
import Overloaded

instance (a ~ Char, KnownSymbol sym) => IsCodeLabel sym [a] where
    codeFromLabel = unsafeTExpCoerce (lift (map toUpper (symbolVal (Proxy :: Proxy sym))))
