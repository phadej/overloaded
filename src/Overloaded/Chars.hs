-- | Overloaded characters.
module Overloaded.Chars (
    FromChar (..),
    ) where

-- | Class for 'Char'-like datastructures
--
-- A character literal @'x'@ is desugared to
--
-- @
-- 'fromChar' \'x\'
-- @
--
---- Enabled with:
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Chars #-}
-- @
--
class FromChar a where
    fromChar :: Char -> a

instance FromChar Char where
    fromChar = id
