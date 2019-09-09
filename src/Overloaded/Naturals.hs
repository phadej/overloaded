-- | Overloaded natural numbers.
module Overloaded.Naturals (
    FromNatural (..),
    ) where

import Numeric.Natural (Natural)
--
-- | Class for 'Natural'-like datastructures
--
-- A numeric literal @42@ is desugared to
--
-- @
-- 'fromNatural' 42
-- @
--
---- Enabled with:
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Naturals #-}
-- @
--
class FromNatural a where
    fromNatural :: Natural -> a

instance FromNatural Natural where
    fromNatural = id

instance FromNatural Integer where
    fromNatural = fromIntegral
