{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module STLC where

import Data.Kind             (Type)
import Data.Proxy            (Proxy (..))
import Overloaded.Categories

import qualified Control.Category

data Ty
    = TyUnit
    | TyPair Ty Ty
    | TyFun Ty Ty
    | TyCoproduct Ty Ty
  deriving (Show)

data Elem :: [Ty] -> Ty -> Type where
    Here  :: Elem (x ': xs) x
    There :: Elem xs x -> Elem (y ': xs) x

deriving instance Show (Elem xs x)

data Term :: [Ty] -> Ty -> Type where
    Var :: Elem ctx ty -> Term ctx ty

    Lam :: Term (a ': ctx) b -> Term ctx ('TyFun a b)
    App :: Term ctx ('TyFun a b) -> Term ctx a -> Term ctx b

    Fst :: Term ctx ('TyPair a b) -> Term ctx a
    Snd :: Term ctx ('TyPair a b) -> Term ctx b
    Pair :: Term ctx a -> Term ctx b -> Term ctx ('TyPair a b)

    InL :: Term ctx a -> Term ctx ('TyCoproduct a b)
    InR :: Term ctx b -> Term ctx ('TyCoproduct a b)
    Case :: Term (a ': ctx) c -> Term (b ':  ctx) c -> Term ctx ('TyCoproduct a b) -> Term ctx c

deriving instance Show (Term xs x)

-------------------------------------------------------------------------------
-- Variables
-------------------------------------------------------------------------------

var0 :: Term (a ': ctx) a
var0 = Var Here

var1 :: Term (b ': a ': ctx) a
var1 = Var (There Here)

-------------------------------------------------------------------------------
-- Weakening
-------------------------------------------------------------------------------

weakenTerm :: Term ctx b -> Term (a ': ctx) b
weakenTerm = weakenTerm' SNil Proxy Proxy

weakenTerm1 :: Term (b ': ctx) c -> Term (b ': a ': ctx) c
weakenTerm1 = weakenTerm' (SCons SNil) Proxy Proxy

weakenTerm2 :: Term ctx b -> Term (a ': a' ': ctx) b
weakenTerm2 = weakenTerm . weakenTerm

weakenTerm' :: SList pfx -> Proxy sfx -> Proxy a
            -> Term (Append pfx sfx) b -> Term (Append pfx (a ': sfx)) b
weakenTerm' pfx sfx a (Var el)     = Var (weakenElem pfx sfx a el)
weakenTerm' pfx sfx a (Lam t)      = Lam (weakenTerm' (SCons pfx) sfx a t)
weakenTerm' pfx sfx a (App u v)    = App (weakenTerm' pfx sfx a u) (weakenTerm' pfx sfx a v)
weakenTerm' pfx sfx a (Fst t)      = Fst (weakenTerm' pfx sfx a t)
weakenTerm' pfx sfx a (Snd t)      = Snd (weakenTerm' pfx sfx a t)
weakenTerm' pfx sfx a (Pair u v)   = Pair (weakenTerm' pfx sfx a u) (weakenTerm' pfx sfx a v)
weakenTerm' pfx sfx a (InL t)      = InL (weakenTerm' pfx sfx a t)
weakenTerm' pfx sfx a (InR t)      = InR (weakenTerm' pfx sfx a t)
weakenTerm' pfx sfx a (Case u v w) = Case
    (weakenTerm' (SCons pfx) sfx a u)
    (weakenTerm' (SCons pfx) sfx a v)
    (weakenTerm' pfx         sfx a w)

weakenElem
    :: SList pfx
    -> Proxy sfx
    -> Proxy a
    -> Elem (Append pfx sfx) b
    -> Elem (Append pfx (a : sfx)) b
weakenElem SNil         _sfx _a el         = There el
weakenElem (SCons  pfx)  sfx  a (There el) = There (weakenElem pfx sfx a el)
weakenElem (SCons _pfx) _sfx _a Here       = Here

-------------------------------------------------------------------------------
-- Append...
-------------------------------------------------------------------------------

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append '[]       ys = ys
    Append (x ': xs) ys = x ': Append xs ys

data SList (xs :: [k]) where
    SNil  :: SList '[]
    SCons :: SList xs -> SList (x ': xs)

-------------------------------------------------------------------------------
-- Smart constructors
-------------------------------------------------------------------------------

app :: Term ctx ('TyFun a b) -> Term ctx a -> Term ctx b
app (Lam b) x = subst SNil Proxy b x
app f       x = App f x

tfst :: Term ctx ('TyPair a b) -> Term ctx a
tfst (Pair x _) = x
tfst p          = Fst p

tsnd :: Term ctx ('TyPair a b) -> Term ctx b
tsnd (Pair _ y) = y
tsnd p          = Snd p

tcase :: Term (a ': ctx) c -> Term (b ':  ctx) c -> Term ctx ('TyCoproduct a b) -> Term ctx c
tcase l _ (InL x) = subst SNil Proxy l x
tcase _ r (InR x) = subst SNil Proxy r x

-- case-of-case
tcase l r (Case l' r' p) = tcase
    (tcase (weakenTerm1 l) (weakenTerm1 r) l')
    (tcase (weakenTerm1 l) (weakenTerm1 r) r')
    p

tcase l r p       = Case l r p

-------------------------------------------------------------------------------
-- Substitution
-------------------------------------------------------------------------------

subst
    :: SList pfx -> Proxy sfx
    -> Term (Append pfx (a ': sfx)) b -> Term sfx a -> Term (Append pfx sfx) b
subst pfx sfx (Var el)     t = substElem pfx sfx el t
subst pfx sfx (Lam x)      t = Lam (subst (SCons pfx) sfx x t)
subst pfx sfx (Fst x)      t = tfst (subst pfx sfx x t)
subst pfx sfx (Snd x)      t = tsnd (subst pfx sfx x t)
subst pfx sfx (InL x)      t = InL (subst pfx sfx x t)
subst pfx sfx (InR x)      t = InR (subst pfx sfx x t)
subst pfx sfx (App u v)    t = app (subst pfx sfx u t) (subst pfx sfx v t)
subst pfx sfx (Pair u v)   t = Pair (subst pfx sfx u t) (subst pfx sfx v t)
subst pfx sfx (Case u v w) t = tcase
    (subst (SCons pfx) sfx u t)
    (subst (SCons pfx) sfx v t)
    (subst pfx         sfx w t)

substElem
    :: SList pfx -> Proxy sfx
    -> Elem (Append pfx (a : sfx)) b
    -> Term sfx a
    -> Term (Append pfx sfx) b
substElem SNil         _sfx Here       t = t
substElem SNil         _sfx (There el) _ = Var el
substElem (SCons _pfx) _sfx Here       _ = Var Here
substElem (SCons  pfx)  sfx (There el) t = weakenTerm (substElem pfx sfx el t)


-------------------------------------------------------------------------------
-- Mapping closed terms of type (a -> b)
-------------------------------------------------------------------------------

newtype Mapping (ctx :: [Ty]) (a :: Ty) (b :: Ty) = M (Term ctx ('TyFun a b))
  deriving (Show)

unMapping :: Mapping ctx a b -> Term ctx ('TyFun a b)
unMapping (M t) = t

-------------------------------------------------------------------------------
-- Category: Mapping
-------------------------------------------------------------------------------

instance Category (Mapping ctx) where
    id        = M $ Lam var0
    M f . M g = M $ Lam $ app (weakenTerm f) (app (weakenTerm g) (Var Here))

-------------------------------------------------------------------------------
-- Product: Mapping
-------------------------------------------------------------------------------

instance CartesianCategory (Mapping ctx) where
    type Product (Mapping ctx) = 'TyPair

    proj1 = M $ Lam $ Fst var0
    proj2 = M $ Lam $ Snd var0
    fanout (M f) (M g) = M $ Lam $ Pair
        (app (weakenTerm f) (Var Here))
        (app (weakenTerm g) (Var Here))

-- | Thanks to 'app' this simplifies!
--
-- >>> ex01mapping
-- M (Lam (Fst (Fst (Var Here))))
ex01 :: CartesianCategory cat => cat (Product cat (Product cat a b) c) a
ex01 = proj1 ## proj1

ex01mapping :: Mapping ctx ('TyPair ('TyPair a b) c) a
ex01mapping = ex01

-- |
--
-- >>> ex0mapping
-- M (Lam (Var Here))
ex02 :: CartesianCategory cat => cat a a
ex02 = proj1 ## fanout identity identity

ex02mapping :: Mapping ctx a a
ex02mapping = ex02

-------------------------------------------------------------------------------
-- Coproduct: Mapping
-------------------------------------------------------------------------------

instance CocartesianCategory (Mapping ctx) where
    type Coproduct (Mapping ctx) = 'TyCoproduct

    inl = M $ Lam $ InL var0
    inr = M $ Lam $ InR var0
    fanin (M f) (M g) = M $ Lam $ tcase
        (app (weakenTerm2 f) var0)
        (app (weakenTerm2 g) var0)
        var0

instance BicartesianCategory (Mapping ctx) where
    distr = M $ Lam $ tcase
        (InL (Pair var0 (Snd var1)))
        (InR (Pair var0 (Snd var1)))
        (Fst var0)

-- |
--
-- >>> ex03mapping
-- M (Lam (Var Here))
ex03 :: CocartesianCategory cat => cat a a
ex03 = fanin identity identity ## inl

ex03mapping :: Mapping ctx a a
ex03mapping = ex03

-------------------------------------------------------------------------------
-- Exponent: Mapping
-------------------------------------------------------------------------------

instance CCC (Mapping ctx) where
    type Exponential (Mapping ctx) = 'TyFun

    eval = M $ Lam $ app (Fst var0) (Snd var0)

    transpose (M f) = M $ Lam $ Lam $ app (weakenTerm2 f) (Pair var1 var0)

-- |
--
-- >>> ex04mapping
-- M (Lam (Pair (Var Here) (Var Here)))
ex04 :: CCC cat => cat a (Product cat a a)
ex04 = eval ## fanout (transpose identity) identity

ex04mapping :: Mapping ctx a ('TyPair a a)
ex04mapping = ex04
