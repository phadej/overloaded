{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Overloaded.TypeSymbols where

import GHC.TypeLits (Symbol)

class FromSymbolC a where
    type family FromSymbol (s :: Symbol) :: a

instance FromSymbolC Symbol where
    type FromSymbol s = s
