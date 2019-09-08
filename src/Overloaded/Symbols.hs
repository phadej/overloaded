{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Another way to desugar overloaded string literals. See 'FromSymbol'.
module Overloaded.Symbols where

import Data.Proxy   (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- | Another way to desugar overloaded string literals using this class.
--
-- A string literal @"example"@ is desugared to
--
-- @
-- 'fromSymbol' \@"example"
-- @
-- 
-- Enabled with:
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Symbols #-}
-- @
--
-- One can do type-level computations with this.
--
class FromSymbol (s :: Symbol) a where
    fromSymbol :: a

instance (KnownSymbol s, a ~ Char) => FromSymbol s [a] where
    fromSymbol = symbolVal (Proxy :: Proxy s)
