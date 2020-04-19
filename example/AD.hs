{-# LANGUAGE GADTs, TypeFamilies, Arrows, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Categories #-}
module Main where

import Overloaded.Categories

import qualified Control.Category

import qualified Numeric.LinearAlgebra as LA

import VectorSpace

evalL :: (HasDim a, HasDim b) => L a b -> LA.Matrix Double
evalL (L f) = toRawMatrix (f LI)

-- | A Function which computes value and derivative at the point.
newtype AD a b = AD (a -> (b, L a b))

instance Category AD where
    id = AD (\x -> (x, L id))

    AD g . AD f = AD $ \a ->
        let (b, L f') = f a
            (c, L g') = g b
        in (c, L (g' . f'))

instance CategoryWith1 AD where
    type Terminal AD = ()

    terminal = AD (const ((), terminal))

instance CartesianCategory AD where
    type Product AD = (,) 

    proj1 = AD (\x -> (fst x, proj1))
    proj2 = AD (\x -> (snd x, proj2))

    fanout (AD f) (AD g) = AD $ \a ->
        let (b, f') = f a
            (c, g') = g a
        in ((b, c), fanout f' g')

instance GeneralizedElement AD where
    type Object AD a = a

    konst x = AD (\_ -> (x, L $ \_ -> LZ))

ladd :: LinMap r (a, a) -> LinMap r a
ladd (LH f g) = LA f g
ladd (LV f g) = LV (ladd f) (ladd g)
ladd (LA a b) = LA (ladd a) (ladd b)
ladd (LK k f) = LK k (ladd f)
ladd LZ       = LZ
ladd LI       = LV LI LI

lmult :: Double -> Double -> LinMap r (a, a) -> LinMap r a
lmult x y (LH f g) = LA (LK y f) (LK x g)
lmult x y (LV f g) = LV (lmult x y f) (lmult x y g)
lmult x y (LA f g) = LA (lmult x y f) (lmult x y g)
lmult x y (LK k f) = LK k (lmult x y f)
lmult _ _ LZ       = LZ
lmult x y LI       = LV (LK y LI) (LK x LI)

plus :: AD (Double, Double) Double
plus = AD $ \(x,y) -> (x + y, L ladd)

mult :: AD (Double, Double) Double
mult = AD $ \(x,y) -> (x * y, L $ lmult x y)

evaluateAD :: (HasDim a, HasDim b) => AD a b -> a -> (b, LA.Matrix Double)
evaluateAD (AD f) x = let (y, f') = f x in (y, evalL f')

ex1 :: AD Double Double
ex1 = plus ## fanout identity identity 

ex2 :: AD Double Double
ex2 = mult ## fanout identity identity

quad :: AD (Double, Double) Double
quad = proc (x, y) -> do
    x2  <- mult    -< (x, x)
    y2  <- mult    -< (y, y)
    tmp <- plus    -< (x2, y2)
    z   <- konst 5 -< ()
    plus -< (tmp, z)

main :: IO ()
main = print $ evaluateAD quad (2,3)
