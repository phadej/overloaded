{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Overloaded.CodeLabels where

import GHC.TypeLits        (Symbol)
import Language.Haskell.TH (TExpQ)

class IsCodeLabel (x :: Symbol) a where
    codeFromLabel :: TExpQ a
