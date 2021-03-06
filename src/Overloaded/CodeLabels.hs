{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Overloaded.CodeLabels where

import GHC.TypeLits                      (Symbol)
import Language.Haskell.TH.Syntax.Compat (SpliceQ)

-- | Class for auto-spliced labels
--
-- The labels @#lbl@ is desugared into @$$(codeFromlabel \@"lbl")@ splice.
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:CodeLabels #-}
-- @
--
-- This feature is not very usable, see https://gitlab.haskell.org/ghc/ghc/-/issues/18211
class IsCodeLabel (sym :: Symbol) a where
    codeFromLabel :: SpliceQ a
