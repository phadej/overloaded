module Overloaded.Plugin.Rewrite where

import qualified GHC.Compat.All  as GHC

-------------------------------------------------------------------------------
-- Rewrite
-------------------------------------------------------------------------------

data Rewrite a
    = NoRewrite
    | Rewrite a -- TODO: add warnings
    | Error (GHC.DynFlags -> IO ())

instance Semigroup (Rewrite a) where
    NoRewrite <> x = x
    x         <> _ = x
