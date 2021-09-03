{-# LANGUAGE CPP               #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Overloaded Categories, desugar @Arrow@ into classes in this module.
--
-- == Enabled with
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Categories #-}
-- @
--
-- == Description
--
-- @Arrows@ notation - [GHC manual chapter](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#arrow-notation) -
-- is cool, but it desugars into /"wrong"/ classes.
-- The 'arr' combinator is used for plumbing. We should desugar to proper
-- type-classes:
--
-- * 'CartesianCategory', not 'A.Arrow'
-- * 'CocartesianCategory', not 'A.ArrowChoice' (implementation relies on 'BicartesianCategory')
-- * 'CCC', not 'A.ArrowApply' (not implemented yet)
--
-- == Examples
--
-- Expression like
--
-- @
-- catAssoc
--     :: 'CartesianCategory' cat
--     => cat ('Product' cat ('Product' cat a b) c) ('Product' cat a ('Product' cat b c))
-- catAssoc = proc ((x, y), z) -> 'identity' -< (x, (y, z))
-- @
--
-- are desugared to (a mess which is)
--
-- @
-- 'fanout' ('proj1' '%%' 'proj1') ('fanout' ('proj2' '%%' 'proj1') 'proj2')
-- @
--
-- If you are familiar with arrows-operators, this is similar to
--
-- @
-- ('fst' . 'fst') '&&&' ('snd' . 'fst' '&&&' 'snd')
-- @
--
-- expression.
--
-- The @catAssoc@ could be instantiated to @cat = (->)@,
-- or more interestingly for example instantiate it to STLC morphisms to get an expression
-- like:
--
-- @
-- Lam (Pair (Fst (Fst (Var Here))) (Pair (Snd (Fst (Var Here))) (Snd (Var Here))))
-- @
--
-- @proc@ notation is nicer than writing de Bruijn indices.
--
-- This is very similar idea to Conal Elliott's [Compiling to Categories](http://conal.net/papers/compiling-to-categories/) work.
-- This approach is syntactically more heavy, but works in more correct
-- stage of compiler, before actual desugarer.
--
-- As one more example, we implement the automatic differentiation,
-- as in Conal's paper(s).
-- To keep things simple we use
--
-- @
-- newtype AD a b = AD (a -> (b, a -> b))
-- @
--
-- representation, i.e. use ordinary maps to represent linear maps.
-- We then define a function
--
-- @
-- evaluateAD :: Functor f => AD a b -> a -> f a -> (b, f b)
-- evaluateAD (AD f) x xs = let (y, f') = f x in (y, fmap f' xs)
-- @
--
-- which would allow to calculuate function value and
-- derivatives in given directions. Then we can define
-- simple quadratic function:
--
-- @
-- quad :: AD (Double, Double) Double
-- quad = proc (x, y) -> do
--     x2 <- mult -< (x, x)
--     y2 <- mult -< (y, y)
--     plus -< (x2, y2)
-- @
--
-- It's not as simple as writing @quad x y = x * x + y * y@,
-- but not /too far/.
--
-- Then we can play with it. At origo everything is zero:
--
-- @
-- let sqrthf = 1 / sqrt 2
-- in evaluateAD quad (0, 0) [(1,0), (0,1), (sqrthf, sqrthf)] = (0.0,[0.0,0.0,0.0])
-- @
--
-- If we evaluate at some other point, we see things working:
--
-- @
-- evaluateAD quad (1, 2) [(1,0), (0,1), (sqrthf, sqrthf)] = (5.0,[2.0,4.0,4.242640687119285])
-- @
--
-- Obviously, if we would use inspectable representation for linear maps,
-- as Conal describe, we'd get more benefits. And then 'arr' wouldn't
-- be definable!
--
module Overloaded.Categories (
    -- * Category
    C.Category,
    identity,
    (%%),
    -- * Monoidial
    SemigroupalCategory (..),
    defaultAssoc, defaultUnassoc,
    MonoidalCategory (..),
    defaultLunit, defaultRunit, defaultUnrunit, defaultUnlunit,
    CommutativeCategory (..),
    defaultSwap,
    -- * Product and Terminal
    CartesianCategory (..),
    CategoryWith1 (..),
    -- * Coproduct and initial
    CategoryWith0 (..),
    CocartesianCategory (..),
    -- * Bicartesian
    BicartesianCategory (..),
    -- * Closed cartesian category
    CCC (..),
    -- * Generalized element
    GeneralizedElement (..),
    -- * OPF
    OPF (..),
    -- * WrappedArrow and WrappedCategory
    WrappedArrow (..),
    WrappedCategory (..),
    ) where

import qualified Control.Arrow    as A
import qualified Control.Category as C

import Control.Applicative        (liftA2)
import Control.Arrow              (Kleisli (..))
import Data.Functor.Contravariant (Op (..))
import Data.Kind                  (Type)
import Data.Profunctor            (Star (..))
import Data.Semigroupoid.Dual     (Dual (..))
import Data.Void                  (Void, absurd)

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
-- Monoidal
-------------------------------------------------------------------------------

class C.Category cat => SemigroupalCategory (cat :: k -> k -> Type) where
    type Tensor cat :: k -> k -> k

    assoc :: cat (Tensor cat (Tensor cat a b) c)
                 (Tensor cat a (Tensor cat b c))

    unassoc :: cat (Tensor cat a (Tensor cat b c))
                   (Tensor cat (Tensor cat a b) c)

defaultAssoc :: (CartesianCategory cat, Tensor cat ~ Product cat) => cat (Tensor cat (Tensor cat a b) c) (Tensor cat a (Tensor cat b c))
defaultAssoc = fanout (proj1 %% proj1) (fanout (proj2 %% proj1) proj2)

defaultUnassoc :: (CartesianCategory cat, Tensor cat ~ Product cat) => cat (Tensor cat a (Tensor cat b c)) (Tensor cat (Tensor cat a b) c)
defaultUnassoc = fanout (fanout proj1 (proj1 %% proj2)) (proj2 %% proj2)

class SemigroupalCategory cat => MonoidalCategory (cat :: k -> k -> Type) where
    type Unit cat :: k

    lunit :: cat (Tensor cat (Unit cat) a) a
    runit :: cat (Tensor cat a (Unit cat)) a

    unlunit :: cat a (Tensor cat (Unit cat) a)
    unrunit :: cat a (Tensor cat a (Unit cat))

defaultLunit :: (CartesianCategory cat, Tensor cat ~ Product cat) => cat (Tensor cat (Unit cat) a) a
defaultLunit = proj2

defaultRunit :: (CartesianCategory cat, Tensor cat ~ Product cat) => cat (Tensor cat a (Unit cat)) a
defaultRunit = proj1

defaultUnlunit :: (CategoryWith1 cat, Tensor cat ~ Product cat, Unit cat ~ Terminal cat) => cat a (Tensor cat (Unit cat) a)
defaultUnlunit = fanout terminal identity

defaultUnrunit :: (CategoryWith1 cat, Tensor cat ~ Product cat, Unit cat ~ Terminal cat) => cat a (Tensor cat a (Unit cat))
defaultUnrunit = fanout identity terminal

class SemigroupalCategory cat => CommutativeCategory cat where
    swap :: cat (Tensor cat a b) (Tensor cat b a)

defaultSwap :: (CartesianCategory cat, Tensor cat ~ Product cat) => cat (Tensor cat a b) (Tensor cat b a)
defaultSwap = fanout proj2 proj1

-------------------------------------------------------------------------------
-- Product
-------------------------------------------------------------------------------

-- | Category with terminal object.
class CartesianCategory cat => CategoryWith1 (cat :: k -> k -> Type) where
    type Terminal cat :: k

    terminal :: cat a (Terminal cat)

-- | Cartesian category is a monoidal category
-- where monoidal product is the categorical product.
--
class C.Category cat => CartesianCategory (cat :: k -> k -> Type) where
    type Product cat :: k -> k -> k

    proj1 :: cat (Product cat a b) a
    proj2 :: cat (Product cat a b) b

    -- | @'fanout' f g@ is written as \(\langle f, g \rangle\) in category theory literature.
    fanout :: cat a b -> cat a c -> cat a (Product cat b c)

instance CategoryWith1 (->) where
    type Terminal (->) = ()

    terminal _ = ()

instance CartesianCategory (->) where
    type Product (->) = (,)

    proj1 = fst
    proj2 = snd
    fanout f g x = (f x , g x)

instance CategoryWith1 Op where
    type Terminal Op = Void

    terminal = Op absurd

instance CartesianCategory Op where
    type Product Op = Either

    proj1 = Op inl
    proj2 = Op inr
    fanout (Op f) (Op g) = Op (fanin f g)

-------------------------------------------------------------------------------
-- Coproduct
-------------------------------------------------------------------------------

-- | Category with initial object.
class CocartesianCategory cat => CategoryWith0 (cat :: k -> k -> Type) where
    type Initial cat :: k

    initial :: cat (Initial cat) a

-- | Cocartesian category is a monoidal category
-- where monoidal product is the categorical coproduct.
--
class C.Category cat => CocartesianCategory (cat :: k -> k -> Type) where
    type Coproduct cat :: k -> k -> k

    inl :: cat a (Coproduct cat a b)
    inr :: cat b (Coproduct cat a b)

    -- | @'fanin' f g@ is written as \([f, g]\) in category theory literature.
    fanin :: cat a c -> cat b c -> cat (Coproduct cat a b) c

instance CategoryWith0 (->) where
    type Initial (->) = Void

    initial = absurd

instance CocartesianCategory (->) where
    type Coproduct (->) = Either

    inl = Left
    inr = Right
    fanin = either

instance CategoryWith0 Op where
    type Initial Op = ()

    initial = Op (const ())

instance CocartesianCategory Op where
    type Coproduct Op = (,)

    inl = Op proj1
    inr = Op proj2
    fanin (Op f) (Op g) = Op (fanout f g)

-- | Bicartesian category is category which is
-- both cartesian and cocartesian.
--
-- We also require distributive morpism.
class (CartesianCategory cat, CocartesianCategory cat) => BicartesianCategory cat where
    distr :: cat (Product cat (Coproduct cat a b) c)
                 (Coproduct cat (Product cat a c) (Product cat b c))

instance BicartesianCategory (->) where
    distr (Left x,  z) = Left (x, z)
    distr (Right y, z) = Right (y, z)

-------------------------------------------------------------------------------
-- Dual
-------------------------------------------------------------------------------

instance CategoryWith1 cat => CategoryWith0 (Dual cat) where
    type Initial (Dual cat) = Terminal cat
    initial = Dual terminal

instance CategoryWith0 cat => CategoryWith1 (Dual cat) where
    type Terminal (Dual cat) = Initial cat
    terminal = Dual initial

instance CartesianCategory cat => CocartesianCategory (Dual cat) where
    type Coproduct (Dual cat) = Product cat

    inl = Dual proj1
    inr = Dual proj2

    fanin (Dual f) (Dual g) = Dual (fanout f g)

instance CocartesianCategory cat => CartesianCategory (Dual cat) where
    type Product (Dual cat) = Coproduct cat

    proj1 = Dual inl
    proj2 = Dual inr

    fanout (Dual f) (Dual g) = Dual (fanin f g)

-------------------------------------------------------------------------------
-- Exponential
-------------------------------------------------------------------------------

-- | Closed cartesian category.
--
class CartesianCategory cat => CCC (cat :: k -> k -> Type) where
    -- | @'Exponential' cat a b@ represents \(B^A\). This is due how (->) works.
    type Exponential cat :: k -> k -> k

    eval :: cat (Product cat (Exponential cat a b) a) b

    transpose :: cat (Product cat a b) c -> cat a (Exponential cat b c)

instance CCC (->) where
    type Exponential (->) = (->)

    eval      = uncurry ($)
    transpose = curry

-------------------------------------------------------------------------------
-- Generalized Element
-------------------------------------------------------------------------------

class C.Category cat => GeneralizedElement (cat :: k -> k -> Type) where
    type Object cat (a :: k) :: Type

    konst :: Object cat a -> cat x a

instance GeneralizedElement (->) where
    type Object (->) a = a

    konst = const

-------------------------------------------------------------------------------
-- Object-Preserving Functor
-------------------------------------------------------------------------------

-- | Object-Preserving Functor (from Hask).
-- Categories with combinator similar to arrows 'arr'.
class C.Category cat => OPF cat where
    -- We could have @type OPFObj cat a :: k@
    -- but it's hard to work with when we want object preserving functor.
    opf :: (a -> b) -> cat a b

instance OPF (->) where
    opf = id

-------------------------------------------------------------------------------
-- Star
-------------------------------------------------------------------------------

instance Monad m => CartesianCategory (Star m) where
    type Product (Star m) = (,)

    proj1 = Star (pure . proj1)
    proj2 = Star (pure . proj2)

    fanout (Star f) (Star g) = Star $ \a -> liftA2 (,) (f a) (g a)

instance Monad m => CategoryWith1 (Star m) where
    type Terminal (Star m) = ()

    terminal = Star (pure . terminal)

instance Monad m => CocartesianCategory (Star m) where
    type Coproduct (Star m) = Either

    inl = Star (pure . inl)
    inr = Star (pure . inr)

    fanin (Star f) (Star g) = Star (fanin f g)

instance Monad m => CategoryWith0 (Star m) where
    type Initial (Star m) = Void

    initial = Star (pure . initial)

instance Monad m => BicartesianCategory (Star m) where
    distr = Star (pure . distr)

instance Monad m => CCC (Star m) where
    type Exponential (Star m) = Star m

    eval = Star $ uncurry runStar
    transpose (Star f) = Star $ \a -> pure $ Star $ \b -> f (a, b)

instance Monad m => OPF (Star m) where
    opf f = Star (pure . f)

-------------------------------------------------------------------------------
-- Kleisli
-------------------------------------------------------------------------------

instance Monad m => CartesianCategory (Kleisli m) where
    type Product (Kleisli m) = (,)

    proj1 = Kleisli (pure . proj1)
    proj2 = Kleisli (pure . proj2)

    fanout (Kleisli f) (Kleisli g) = Kleisli $ \a -> liftA2 (,) (f a) (g a)

instance Monad m => CategoryWith1 (Kleisli m) where
    type Terminal (Kleisli m) = ()

    terminal = Kleisli (pure . terminal)

instance Monad m => CocartesianCategory (Kleisli m) where
    type Coproduct (Kleisli m) = Either

    inl = Kleisli (pure . inl)
    inr = Kleisli (pure . inr)

    fanin (Kleisli f) (Kleisli g) = Kleisli (fanin f g)

instance Monad m => CategoryWith0 (Kleisli m) where
    type Initial (Kleisli m) = Void

    initial = Kleisli (pure . initial)

instance Monad m => BicartesianCategory (Kleisli m) where
    distr = Kleisli (pure . distr)

instance Monad m => CCC (Kleisli m) where
    type Exponential (Kleisli m) = Kleisli m

    eval = Kleisli $ uncurry runKleisli
    transpose (Kleisli f) = Kleisli $ \a -> pure $ Kleisli $ \b -> f (a, b)

instance Monad m => OPF (Kleisli m) where
    opf f = Kleisli (pure . f)

-------------------------------------------------------------------------------
-- WrappedArrow
-------------------------------------------------------------------------------

-- | 'Arrow' correspond to various classes
-- in this hierarachy.
newtype WrappedArrow arr a b = WrapArrow { unwrapArrow :: arr a b }

instance C.Category arr => C.Category (WrappedArrow arr) where
    id = WrapArrow identity
    WrapArrow f . WrapArrow g = WrapArrow (f %% g)

instance A.Arrow arr => CategoryWith1 (WrappedArrow arr) where
    type Terminal (WrappedArrow arr) = ()
    terminal = WrapArrow (A.arr terminal)

instance A.Arrow arr => CartesianCategory (WrappedArrow arr) where
    type Product (WrappedArrow arr) = (,)
    proj1 = WrapArrow (A.arr proj1)
    proj2 = WrapArrow (A.arr proj2)
    fanout (WrapArrow f) (WrapArrow g) = WrapArrow (f A.&&& g)

instance A.ArrowChoice arr => CategoryWith0 (WrappedArrow arr) where
    type Initial (WrappedArrow arr) = Void
    initial = WrapArrow (A.arr absurd)

instance A.ArrowChoice arr => CocartesianCategory (WrappedArrow arr) where
    type Coproduct (WrappedArrow arr) = Either
    inl = WrapArrow (A.arr inl)
    inr = WrapArrow (A.arr inr)
    fanin (WrapArrow f) (WrapArrow g) = WrapArrow (f A.||| g)

instance A.ArrowChoice arr => BicartesianCategory (WrappedArrow arr) where
    distr = WrapArrow (A.arr distr)

instance A.ArrowApply arr => CCC (WrappedArrow arr) where
    type Exponential (WrappedArrow arr) = arr

    eval = WrapArrow A.app
    transpose = error "ArrowApply @(WrappedArrow arr) is not implemented"

instance A.Arrow arr => GeneralizedElement (WrappedArrow arr) where
    type Object (WrappedArrow arr) a = a
    konst = WrapArrow . A.arr . const

instance A.Arrow arr => OPF (WrappedArrow arr) where
    opf = WrapArrow . A.arr

-------------------------------------------------------------------------------
-- WrappedCategory
-------------------------------------------------------------------------------

newtype WrappedCategory cat a b = WrapCat { unwrapCat :: cat a b }

instance C.Category cat => C.Category (WrappedCategory cat) where
    id = WrapCat identity
    WrapCat f . WrapCat g = WrapCat (f %% g)

instance (OPF cat, CartesianCategory cat, Product cat ~ (,))
    => A.Arrow (WrappedCategory cat)
  where
    arr = WrapCat . opf
    WrapCat f *** WrapCat g = WrapCat (fanout (f %% proj1) (g %% proj2))

instance (OPF cat, CartesianCategory cat, Product cat ~ (,)
    , CocartesianCategory cat, Coproduct cat ~ Either)
    => A.ArrowChoice (WrappedCategory cat)
  where
    WrapCat f +++ WrapCat g = WrapCat (fanin (inl %% f) (inr %% g))

instance (OPF cat, CartesianCategory cat, Product cat ~ (,)
    , CCC cat, Exponential cat ~ cat)
    => A.ArrowApply (WrappedCategory cat)
  where
    app = WrapCat eval %% A.first (WrapCat (opf unwrapCat))
