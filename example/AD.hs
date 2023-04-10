{-# LANGUAGE Arrows              #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Categories #-}
module Main where

import Control.Monad      (when)
import Data.List          (intercalate)
import Data.Word          (Word64)
import Numeric            (showFFloat)
import System.Environment (getArgs)

import qualified Control.Category
import qualified Numeric.LinearAlgebra  as LA
import qualified System.Random.SplitMix as SM

import Overloaded.Categories
import VectorSpace

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
ladd LZ       = LZ
ladd (LD k)   = LV (LD k) (LD k)

lmult :: Double -> Double -> LinMap r (a, a) -> LinMap r a
lmult x y (LH f g) = LA (lmul y f) (lmul x g)
lmult x y (LV f g) = LV (lmult x y f) (lmult x y g)
lmult x y (LA f g) = LA (lmult x y f) (lmult x y g)
lmult _ _ LZ       = LZ
lmult x y (LD k)   = LV (LD (k * y)) (LD (k * x))

plus :: AD (Double, Double) Double
plus = AD $ \(x,y) -> (x + y, L ladd)

minus :: AD (Double, Double) Double
minus = AD $ \(x,y) -> (x - y, L $ lmult (-1) 1)

mult :: AD (Double, Double) Double
mult = AD $ \(x,y) -> (x * y, L $ lmult x y)

scale :: Double -> AD Double Double
scale k = AD $ \x -> (k * x, linear k)

evaluateAD :: (HasDim a, HasDim b) => AD a b -> a -> (b, LA.Matrix Double)
evaluateAD (AD f) x = let (y, f') = f x in (y, evalL f')

-------------------------------------------------------------------------------
-- Simple examples
-------------------------------------------------------------------------------

ex1 :: AD Double Double
ex1 = plus %% fanout identity identity

ex2 :: AD Double Double
ex2 = mult %% fanout identity identity

-------------------------------------------------------------------------------
-- Quadratic function
-------------------------------------------------------------------------------

quad :: AD (Double, Double) Double
quad = proc (x, y) -> do
    x2  <- mult    -< (x, x)
    y2  <- mult    -< (y, y)
    tmp <- plus    -< (x2, y2)
    z   <- konst 5 -< ()
    plus -< (tmp, z)

-------------------------------------------------------------------------------
-- Newton
-------------------------------------------------------------------------------

findZero :: AD Double Double -> Double -> [Double]
findZero f x0 = take 10 results
  where
    results = iterate go x0

    go :: Double -> Double
    go x =
        let (y, m) = evaluateAD f x
            [[y']] = LA.toLists m
        in x - gamma * (y / y')

    gamma = 0.1

-------------------------------------------------------------------------------
-- Gradient descent
-------------------------------------------------------------------------------

gradDesc :: forall a. VectorSpace a => AD a Double -> a -> [a]
gradDesc f = iterate go where
    go :: a -> a
    go x =
        let (_, m) = evaluateAD f x
            [grad] = LA.toLists $ LA.tr $ LA.scale gamma m

        in fromVector $ zipWith (-) (toVector x) grad

    gamma = 0.1

-------------------------------------------------------------------------------
-- Random
-------------------------------------------------------------------------------

randomDoubles :: Word64 -> [Double]
randomDoubles seed = go (SM.mkSMGen seed) where
    go g = let (d, g') = SM.nextDouble g in d : go g'

-------------------------------------------------------------------------------
-- Dot
-------------------------------------------------------------------------------

class VectorSpace' a where
    sumN :: AD a Double
    multN :: AD (a, a) a

instance (VectorSpace' a, VectorSpace' b) => VectorSpace' (a, b) where
    sumN = proc (x, y) -> do
        x' <- sumN -< x
        y' <- sumN -< y
        plus -< (x', y')

    multN = proc ((x1, x2), (y1, y2)) -> do
        z1 <- multN -< (x1, y1)
        z2 <- multN -< (x2, y2)
        identity -< (z1, z2)

instance VectorSpace' Double where
    sumN  = identity
    multN = mult

dot :: VectorSpace' a => AD (a, a) Double
dot = sumN %% multN

-------------------------------------------------------------------------------
-- ML stuff
-------------------------------------------------------------------------------

tanhAD :: AD Double Double
tanhAD = AD $ \x ->
    let y = tanh x
    in (y, linear (1 - y * y))

sigmoidAD :: AD Double Double
sigmoidAD = AD $ \x ->
    let y = 1 / (1 + exp (- x))
    in (x, linear (y * (1 - y)))

-- | weights for 2x1 connection. Two weights and bias.
type Weights' = ((Double, Double), Double)

-- | Two internal neurons, and final output
type Weights = ((Weights', Weights'), Weights')

startWeights :: Weights
startWeights = fromVector $ randomDoubles 1337

neuron :: AD (Weights', (Double, Double)) Double
neuron = proc ((ws, bias), i) -> do
    o <- dot -< (ws, i)
    tanhAD %% plus -< (o, bias)

network :: AD (Weights, (Double, Double)) Double
network = proc (((w1, w2), w3), xy) -> do
    u <- neuron  -< (w1, xy)
    v <- neuron  -< (w2, xy)
    neuron -< (w3, (u, v))

networkError :: AD Weights Double
networkError = proc ws -> do
    -- xor!
    s1 <- ex 1 1 0 -< ws
    s2 <- ex 0 0 0 -< ws
    s3 <- ex 1 0 1 -< ws
    s4 <- ex 0 1 1 -< ws

    sumN -< ((s1,s2), (s3, s4))
  where
    ex :: Double -> Double -> Double -> AD Weights Double
    ex x y z = proc ws -> do
         x1 <- konst x -< ()
         y1 <- konst y -< ()
         e1 <- konst z -< ()
         a1 <- network -< (ws, (x1, y1))
         r1 <- minus   -< (e1, a1)
         mult -< (r1, r1)

train :: Weights
train = gradDesc networkError startWeights !! 500

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn $ "quad (2,3) = " ++ show (evaluateAD quad (2,3))
    putStrLn $ "gradDesc quad (2,3) = " ++ show (gradDesc quad (2,3) !! 30)

    print $ evaluateAD tanhAD 1
    print $ evaluateAD sigmoidAD 1

    putStrLn "Training the net (for xor)"
    let ws = train
    putStrLn $ "Parameters = " ++ show (toVector ws)
    putStrLn $ "Error = " ++ show (fst $ evaluateAD networkError ws)
    let example xy =
          putStrLn $ "eval " ++ show xy ++ " = " ++ showFFloat (Just 2) (fst $ evaluateAD network (ws, xy)) ""

    example (0, 0)
    example (0, 1)
    example (1, 0)
    example (1, 1)

    args <- getArgs
    when ("plot" `elem` args) $ do
        putStrLn "Outputting plot data: datafile.dat"

        let n = 20 :: Int
        let points = [ fromIntegral x / fromIntegral n | x <- [0..n] ] :: [Double]

        let output :: String
            output = unlines
                [ intercalate "\t"
                    [ show x
                    , show y
                    , show (fst (evaluateAD network (ws, (x, y))))
                    ]
                | x <- points
                , y <- points
                ]

        writeFile "datafile.dat" output
