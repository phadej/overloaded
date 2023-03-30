-- | Overloaded @if@-expression.
module Overloaded.If (
    ToBool (..),
    ifte,
    ) where

import Data.Either (isRight)
import Data.Maybe  (isJust)

-- | Class for 'Bool'-like datastrucutres
--
-- An @if-@-expression @if b then t else e@ is desugared to
--
-- @
-- ifte ('toBool' b) t e
-- @
--
-- Enabled with:
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:If #-}
-- @
--
class ToBool b where
    toBool :: b -> Bool

instance ToBool Bool where
    toBool = id

-- | 'Just' is 'True'
instance ToBool (Maybe a) where
    toBool = isJust

-- | 'Right' is 'True'
instance ToBool (Either b a) where
    toBool = isRight

-- | 'ToBool' overloaded @if@-expression.
ifte :: ToBool b => b -> a -> a -> a
ifte b t e = if toBool b then t else e
