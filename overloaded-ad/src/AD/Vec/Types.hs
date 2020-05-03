{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module AD.Vec.Types where

import Data.Proxy    (Proxy (..))
import Data.Type.Nat (SNat (..), SNatI (..))
import Data.Vec.Lazy (Vec (..))

import Data.Type.Nat.PlusTree
import Overloaded.Categories.Constrained

import qualified Control.Category as C
import qualified Data.Type.Nat    as N
import qualified Data.Vec.Lazy    as V

-------------------------------------------------------------------------------
-- Scalar
-------------------------------------------------------------------------------

type S = Double

szero :: S
szero = 0

sone :: S
sone = 1

splus :: S -> S -> S
splus = (+)

smult :: S -> S -> S
smult = (*)

-------------------------------------------------------------------------------
-- Vec tools
-------------------------------------------------------------------------------

type V n = Vec n S

vdot :: V n -> V n -> S
vdot x y = V.foldl' splus szero (V.zipWith smult x y)

vplus :: V n -> V n -> V n
vplus = V.zipWith splus

vscale :: S -> V n -> V n
vscale k = V.map (smult k)

vfst :: Proxy b -> SNat a -> Vec (N.Plus a b) s -> Vec a s
vfst p a = caseSNat a (\_ -> VNil) $ \a' (x ::: xs) -> x ::: vfst p a' xs

vsnd :: Proxy b -> SNat a -> Vec (N.Plus a b) s -> Vec b s
vsnd p a = caseSNat a id $ \a' (_ ::: xs) -> vsnd p a' xs

-------------------------------------------------------------------------------
-- Matrix
-------------------------------------------------------------------------------

type M n m = Vec m (Vec n S)

eye :: forall n. SNatI n => M n n
eye = case N.snat :: N.SNat n of
    N.SZ -> VNil
    N.SS -> (sone ::: V.repeat szero) ::: V.map (szero :::) eye

mplus :: M n m -> M n m -> M n m
mplus = V.zipWith (V.zipWith splus)

multR :: SNatI n => M n m -> V m -> V n
multR m = multL (mtranspose m)

multL :: M n m -> V n -> V m
multL m x = V.map (vdot x) m

mtranspose :: SNatI n => M n m -> M m n
mtranspose VNil         = V.repeat VNil
mtranspose (xs ::: xss) = V.zipWith (:::) xs (mtranspose xss)

splitV :: forall m n p. SNatI n => M (N.Plus n m) p -> (M n p, M m p)
splitV VNil       = (VNil, VNil)
splitV (x ::: xs) =
    let (y, z)   = V.split x
        (ys, zs) = splitV xs
    in (y ::: ys, z ::: zs)

splitH :: forall m p n. SNatI m => M n (N.Plus m p) -> (M n m, M n p)
splitH = V.split

-------------------------------------------------------------------------------
-- L
-------------------------------------------------------------------------------

newtype L n m = L { unL :: forall p. SNatI p => M p n -> M p m }

instance C.Category L where
    id = L id

    L f . L g = L (f . g)

lterminal :: L n N.Nat0
lterminal = L $ \_ -> VNil

lfanout :: L a b -> L a c -> L a (N.Plus b c)
lfanout (L f) (L g) = L $ \x -> f x V.++ g x

lproj1 :: forall a b. SNatI a => Proxy b -> L (N.Plus a b) a
lproj1 _ = L (vfst (Proxy :: Proxy b) (snat :: N.SNat a))

lproj2 :: forall a b. SNatI a => Proxy a -> L (N.Plus a b) b
lproj2 _ = L (vsnd (Proxy :: Proxy b) (snat :: N.SNat a))

bilinearLS
    :: forall n m. (SNatI n, SNatI m)
    => M n m -> Vec n S -> Vec m S -> L (N.Plus n m) N.Nat1
bilinearLS t n m = L $ \d ->
    let (dn, dm) = splitH @n @m d
    in mplus (V.singleton $ multR dn $ multR t m)
             (V.singleton $ multR dm $ multL t n)

lplus :: forall n. SNatI n => Proxy n -> L (N.Plus n n) n
lplus _ = L $ \xs ->
    let (x, y) = splitH @n @n xs
    in mplus x y

lscale :: S -> L n n
lscale k = L $ V.map (V.map (smult k))

lscaleV :: V n -> L n n
lscaleV xs = L $ V.zipWith vscale xs

-------------------------------------------------------------------------------
-- AD
-------------------------------------------------------------------------------

newtype AD (a :: PT) (b :: PT) = AD
    { unAD :: forall r. V (Eval a) -> (V (Eval b) -> L (Eval a) (Eval b) -> r) -> r }

instance C.Category AD where
    id = AD $ \x k -> k x identity
    AD f . AD g = AD $ \a k ->
        g a $ \b g' ->
        f b $ \c f' ->
        k c (f' %% g')

instance CartesianCategory SPTI AD where
    type Product AD = 'Plus

    fanout (AD f) (AD g) = AD $ \a k ->
        f a $ \b f' ->
        g a $ \c g' ->
        k (b V.++ c) (lfanout f' g')

    proj1 = adProj1
    proj2 = adProj2

instance CategoryWith1 SPTI AD where
    type Terminal AD = 'Leaf N.Nat0

    terminal = adTerminal

adProj1 :: forall a b. SNatI (Eval a) => AD ('Plus a b) a
adProj1 = AD $ \xs k -> k (vfst b snat xs) (lproj1 b) where
    b :: Proxy (Eval b)
    b = Proxy

adProj2 :: forall a b. SNatI (Eval a) => AD ('Plus a b) b
adProj2 = AD $ \xs k -> k (vsnd b (snat :: SNat (Eval a)) xs) (lproj2 a) where
    a :: Proxy (Eval a)
    a = Proxy
    b :: Proxy (Eval b)
    b = Proxy

adTerminal :: AD a ('Leaf N.Nat0)
adTerminal = AD $ \_ k -> k VNil lterminal

-------------------------------------------------------------------------------
-- AD functions
-------------------------------------------------------------------------------

konst :: (SNatI n, Eval b ~ n) => V n -> AD a b
konst x = AD $ \_ k -> k x $ L $ \_ -> V.repeat (V.repeat szero)

bilinearS
    :: forall a b n m. (SNatI n, SNatI m, Eval a ~ n, Eval b ~ m)
    => M n m -> AD ('Plus a b) ('Leaf N.Nat1)
bilinearS t = AD $ \xs k ->
    let (n, m) = V.split @n @m xs
    in k (V.singleton $ vdot n $ multR t m) (bilinearLS t n m)

plus :: forall a n. (SNatI n, Eval a ~ n) => AD ('Plus a a) a
plus = AD $ \xs k ->
    let (x, y) = V.split @n @n xs
    in k (V.zipWith splus x y) (lplus (Proxy :: Proxy n))

scale :: S -> AD n n
scale k = AD $ \xs kont ->
    kont (V.map (smult k) xs) (lscale k)

dot :: forall a n. (SNatI n, Eval a ~ n) => AD ('Plus a a) ('Leaf N.Nat1)
dot = bilinearS eye

-------------------------------------------------------------------------------
-- Fancy
-------------------------------------------------------------------------------

tanhAD :: AD a a
tanhAD = AD $ \x k ->
    let y = V.map tanh x
    in k y (lscaleV (V.map (\x' -> 1 - x' * x') y))

-------------------------------------------------------------------------------
-- Evaluation
-------------------------------------------------------------------------------

evaluateAD
    :: (SNatI n, SNatI m, Eval a ~ n, Eval b ~ m)
    => AD a b -> V n -> (V m, M n m)
evaluateAD (AD f) x = f x $ \y (L f') ->
    (y, f' eye)

-------------------------------------------------------------------------------
-- Gradient descent
-------------------------------------------------------------------------------

gradDesc
    :: forall a n. (N.SNatI n, Eval a ~ n)
    => AD a ('Leaf N.Nat1) -> V n -> [V n]
gradDesc f = iterate go where
    go :: V n -> V n
    go x = case evaluateAD f x of
        (_, m) -> let m' = V.head m in vplus x (vscale (-0.1) m')
