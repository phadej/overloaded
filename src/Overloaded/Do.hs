{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Overloaded "local" @do@-blocks.
--
-- Inspired by [Local Do GHC-proposal](https://github.com/ghc-proposals/ghc-proposals/pull/216).
-- Yet because we do desugaring in reader phase, we must have
-- a bit more complicated setup.
--
-- The expressions like
--
-- @
-- ex2d :: IxStateT Identity Int String ()
-- ex2d = ixmonad.do
--     _unused <- ixmodify show
--     ixmodify reverse
-- @
--
-- are desugared into
--
-- @
-- ex2b :: IxStateT Identity Int String ()
-- ex2b =
--     ixmonad \@Bind (ixmodify show) $ \\_unused ->
--     ixmodify reverse
-- @
--
-- Allowing to locally overload what @do@ desugars to.
--
-- The 'monad' in this module is an example how to define a desugaring.
-- We need to it this way, so the names are easily accessible in renamer phase.
-- (I.e. constant, then transformation is pure, as we don't need to lookup them for each do-block).
--
-- Enabled with:
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Do #-}
-- @
--
module Overloaded.Do (
-- * Do desugaring methods
DoMethod (..),
-- * Type aliases
Pure, Then, Bind,
-- * Default Monad desugaring
Monad' (..),
) where

import Data.Kind (Type)

-------------------------------------------------------------------------------
-- Definitions
-------------------------------------------------------------------------------

data DoMethod
    = Pure  -- ^ 'return'
    | Then  -- ^ '>>'
    | Bind  -- ^ '>>='

type Pure = 'Pure
type Then   = 'Then
type Bind   = 'Bind

-------------------------------------------------------------------------------
-- Default Monad
-------------------------------------------------------------------------------

class Monad' (method :: DoMethod) (ty :: Type) where
    monad :: ty

instance (ty ~ (a -> m a),                 Applicative m) => Monad' 'Pure ty where monad = pure
instance (ty ~ (m a -> m b -> m b),        Applicative m) => Monad' 'Then ty where monad = (*>)
instance (ty ~ (m a -> (a -> m b) -> m b), Monad m)       => Monad' 'Bind ty where monad = (>>=)
