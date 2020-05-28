{-# LANGUAGE DeriveFunctor #-}
module Overloaded.Plugin.Rewrite where

import Control.Monad (ap)

import qualified GHC.Compat.All  as GHC

-------------------------------------------------------------------------------
-- Rewrite
-------------------------------------------------------------------------------

data Rewrite a
    = NoRewrite
    | Rewrite a -- TODO: add warnings
    | WithName (GHC.Name -> Rewrite a)
    | Error (GHC.DynFlags -> IO ())
  deriving (Functor)

instance Semigroup (Rewrite a) where
    NoRewrite <> x = x
    x         <> _ = x

instance Monoid (Rewrite a) where
    mempty  = NoRewrite
    mappend = (<>)

instance Applicative Rewrite where
    pure = Rewrite
    (<*>) = ap

instance Monad Rewrite where
    return = Rewrite
    NoRewrite >>= _ = NoRewrite
    Rewrite a >>= k = k a
    WithName f >>= k = WithName (\n -> f n >>= k)
    Error err >>= _ = Error err
