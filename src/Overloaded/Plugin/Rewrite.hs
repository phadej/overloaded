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
    | Error (GHC.DynFlags -> IO ())
  deriving (Functor)

instance Semigroup (Rewrite a) where
    NoRewrite <> x = x
    x         <> _ = x

instance Applicative Rewrite where
    pure = Rewrite
    (<*>) = ap

instance Monad Rewrite where
    return = Rewrite
    NoRewrite >>= _ = NoRewrite
    Rewrite a >>= k = k a
    Error err >>= _ = Error err
