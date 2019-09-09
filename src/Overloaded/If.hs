module Overloaded.If (
    ToBool (..),
    ifte,
    ) where

import Data.Maybe (isJust)
import Data.Either (isRight)

class ToBool b where
    toBool :: b -> Bool

instance ToBool Bool where
    toBool = id

instance ToBool (Maybe a) where
    toBool = isJust

instance ToBool (Either b a) where
    toBool = isRight

ifte :: ToBool b => b -> a -> a -> a
ifte b t e = if toBool b then t else e
