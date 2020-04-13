{-# LANGUAGE TypeFamilies #-}
module AD where

import Overloaded.Categories

import qualified Control.Category

-- | A Function which computes value and derivative at the point.
newtype AD a b = AD (a -> (b, a -> b))

instance Category AD where
    id = AD (\x -> (x, id))

    AD g . AD f = AD $ \a ->
        let (b, f') = f a
            (c, g') = g b
        in (c, g' . f')

linearD :: (a -> b) -> AD a b
linearD f = AD $ \x -> (f x, f)

instance CategoryProduct AD where
    type Product AD = (,) 

    proj1 = linearD fst
    proj2 = linearD snd

    fanout (AD f) (AD g) = AD $ \a ->
        let (b, f') = f a
            (c, g') = g a
        in ((b, c), fanout f' g')

plus :: Num a => AD (a, a) a
plus = linearD (uncurry (+))

mult :: Num a => AD (a, a) a
mult = AD $ \(x,y) -> (x * y, \(dx, dy) -> dx * y + dy * x)

ex1 :: AD Double Double
ex1 = plus ## fanout identity identity 

ex2 :: AD Double Double
ex2 = mult ## fanout identity identity

evaluateAD :: Functor f => AD a b -> a -> f a -> (b, f b)
evaluateAD (AD f) x xs = let (y, f') = f x in (y, fmap f' xs)
