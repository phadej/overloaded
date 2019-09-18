{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Optics.HasField (field) where

import GHC.Records.Compat (HasField (..))
import GHC.TypeLits       (Symbol)
import Optics.Core        (Lens', lens)

field :: forall (name :: Symbol) r a. HasField name r a => Lens' r a
field = lens (snd . f) (fst . f)
  where
    f :: r -> (a -> r, a)
    f = hasField @name
