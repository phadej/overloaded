{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
-- | Overloaded type-level symbols.
module Overloaded.TypeSymbols where

import GHC.TypeLits (Symbol)

-- | A way to overload type level 'Symbol's.
--
-- A symbol type-literal @"example"@ is desugared to
--
-- @
-- 'FromTypeSymbol' "example"
-- @
--
-- Enabled with:
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:TypeSymbols #-}
-- @
--
class FromTypeSymbolC a where
    type family FromTypeSymbol (s :: Symbol) :: a

instance FromTypeSymbolC Symbol where
    type FromTypeSymbol s = s
