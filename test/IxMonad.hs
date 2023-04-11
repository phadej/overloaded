{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module IxMonad where

import Data.Functor.Identity (Identity (..))
import Data.Kind             (Type)
import Overloaded.Do

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

class IxMonad m where
    ipure :: a -> m i i a
    (>>>=) :: m i j a -> (a -> m j k b) -> m i k b

infixl 4 >>>=

-------------------------------------------------------------------------------
-- Indexed State
-------------------------------------------------------------------------------

newtype IxStateT m i j a = IxStateT { runIxStateT :: i -> m (a, j) }

instance Monad m => IxMonad (IxStateT m) where
    ipure x = IxStateT $ \i -> pure (x, i)

    m >>>= k = IxStateT $ \s0 -> do
        (x, s1) <- runIxStateT m s0
        runIxStateT (k x) s1

ixmodify :: Applicative m => (i -> j) -> IxStateT m i j ()
ixmodify f = IxStateT $ \i -> pure ((), f i)

execIxState :: IxStateT Identity i j a -> i -> j
execIxState m i = snd (runIdentity (runIxStateT m i))

-------------------------------------------------------------------------------
-- Overloading
-------------------------------------------------------------------------------

class IxMonad' (method :: DoMethod) (ty :: Type) where
    ixmonad :: ty

instance (ty ~ (a -> m i i a),                         IxMonad m) => IxMonad' 'Pure ty where ixmonad = ipure
instance (ty ~ (m i j a -> m j k b -> m i k b),        IxMonad m) => IxMonad' 'Then ty where ixmonad = \x y -> x >>>= \_ -> y
instance (ty ~ (m i j a -> (a -> m j k b) -> m i k b), IxMonad m) => IxMonad' 'Bind ty where ixmonad = (>>>=)
