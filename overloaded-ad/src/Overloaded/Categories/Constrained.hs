{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
module Overloaded.Categories.Constrained where

import Data.Kind (Type)

import qualified Control.Category as C

-------------------------------------------------------------------------------
-- Category
-------------------------------------------------------------------------------

-- | A non-clashing name for 'C.id'.
identity :: C.Category cat => cat a a
identity = C.id
{-# INLINE identity #-}

-- | A non-clashing name for @('C..')@.
(%%) :: C.Category cat => cat b c -> cat a b -> cat a c
(%%) = (C..)
{-# INLINE (%%) #-}
infixr 9 %%

-------------------------------------------------------------------------------
-- Category Classes
-------------------------------------------------------------------------------

class C.Category cat => CartesianCategory c (cat :: k -> k -> Type) | cat -> c where
    type Product cat :: k -> k -> k

    proj1 :: (c a, c b) => cat (Product cat a b) a
    proj2 :: (c a, c b) => cat (Product cat a b) b

    fanout :: (c a, c x, c y) => cat a x -> cat a y -> cat a (Product cat x y)

class CartesianCategory c cat => CategoryWith1 c (cat :: k -> k -> Type) | cat -> c where
    type Terminal cat :: k

    terminal :: cat a (Terminal cat)

-------------------------------------------------------------------------------
-- Unused classes
-------------------------------------------------------------------------------
