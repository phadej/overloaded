{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Overloaded.Categories.Nat where

import Data.Kind        (Type)
import Data.Type.Nat    (Nat)
import Data.Type.Nat.LE (LE)

import qualified Control.Category as C
import qualified Data.Type.Nat    as N

-------------------------------------------------------------------------------
-- Category
-------------------------------------------------------------------------------

-- | A non-clashing name for 'C.id'.
identity :: forall (cat :: Nat -> Nat -> Type) a. C.Category cat => cat a a
identity = C.id
{-# INLINE identity #-}

-- | A non-clashing name for @('C..')@.
(%%) :: forall (cat :: Nat -> Nat -> Type) a b c. C.Category cat => cat b c -> cat a b -> cat a c
(%%) = (C..)
{-# INLINE (%%) #-}
infixr 9 %%

-------------------------------------------------------------------------------
-- Category Classes
-------------------------------------------------------------------------------

class C.Category cat => CartesianCategory cat where
    proj1 :: (LE a ab, N.SNatI ab) => cat ab a
    proj2 :: (LE b ab, N.SNatI ab) => cat ab b

    -- | @'fanout' f g@ is written as \(\langle f, g \rangle\) in category theory literature.
    fanout :: cat a b -> cat a c -> cat a (N.Plus b c)

class CartesianCategory cat => CategoryWith1 cat where
    terminal :: cat a N.Nat0


-------------------------------------------------------------------------------
-- Unused classes
-------------------------------------------------------------------------------
