{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS -Wno-unused-top-binds -Wno-name-shadowing -Wno-missing-signatures
            -fplugin=Overloaded
            -fplugin-opt=Overloaded:RebindableApplication
            -fplugin-opt=Overloaded:RebindableAbstraction
 #-}


module Main (main) where

import Data.Kind                        (Type)
import Overloaded.RebindableAbstraction (Lam (lam))
import Overloaded.RebindableApplication (Apply (apply), ($))
import Prelude                          hiding (($))

main :: IO ()
main = do
  -- print (foo :: Integer)
-- We don't support rebindable function-bindings on 9.2
#if MIN_VERSION_ghc(9,4,0)
  print $ pdouble2 2
#endif
  print $ pdouble 2
  return ()

----- Mockup imitation of Plutarch 1 surface syntax

data S

type PType = S -> Type

newtype Term (s :: S) (t :: PType) = Term (t s) deriving stock Show

newtype PInt (s :: S) = PInt Int deriving stock Show

instance Num (Term s PInt) where
  fromInteger i = Term $ PInt $ fromInteger i
  (Term (PInt a)) + (Term (PInt b)) =  Term (PInt (a + b))
  (Term (PInt a)) - (Term (PInt b)) =  Term (PInt (a - b))
  (Term (PInt a)) * (Term (PInt b)) =  Term (PInt (a * b))
  abs (Term (PInt a)) = Term (PInt (abs a))
  signum (Term (PInt a)) = Term (PInt (signum a))

instance Eq (Term s PInt) where
  (Term (PInt a)) == (Term (PInt b)) = a == b

newtype ((a :: PType) :--> (b :: PType)) (s :: S) = PFun (a s -> b s)
infixr 0 :-->

plam' :: (Term s a -> Term s b) -> Term s (a :--> b)
plam' f = Term $ PFun \a -> let Term b = f (Term a) in b

----- Term instances

instance Lam (Term s (a :--> b)) (Term s a) (Term s b) where
  lam :: (Term s a -> Term s b) -> Term s (a :--> b)
  lam = plam'

instance Apply (Term s (a :--> b)) (Term s a) (Term s b) where
  apply (Term (PFun f)) (Term a) = Term $ f a

----- Usage

-- Can work as both, see next two bindings
genericId :: Lam f b b => f
genericId = \x -> x

hsId :: a -> a
hsId = genericId

termId :: Term s (a :--> a)
termId = genericId

-- Overloaded apply for Terms
bar :: Term s PInt
bar = (Term (PFun (\(PInt a) -> PInt (a + 1)))) 1

-- not giving type signature to show that bottom-up inference works
bar2 = (+ 1) (1 :: Term s PInt)

-- not giving type signature to show that bottom-up inference works
derp = (+ 1) (1 :: Int)

herp = ((\a b -> a + b) :: Int -> Int -> Int) 1 1

herp2 = let (+) = ((\a -> \b -> a Prelude.+ b) :: Term s (PInt :--> PInt :--> PInt)) in 1 + 1

map' :: (Apply f x y) => f -> [x] -> [y]
map' f = map (apply f)

-- | Force the 'Lam' instance to 'Term'.
--
-- The type checker has no way to figure this out from the argument or result
-- types when using a generic 'Lam' with 'apply'. Inconvenient, but the type
-- checker is right. In some cases the generic 'Lam' could mean either a Haskell
-- function or a Plutarch function when the argument or result is a 'Term', and
-- both would typecheck.
hintPFun :: Term s (a :--> b) -> Term s (a :--> b)
hintPFun = id

herpa :: forall s. Term s PInt
herpa = (hintPFun genericId) (1 :: Term s PInt)

-- Can work as both of those types:
pconst :: Term s (a :--> b :--> a)
-- pconst :: a -> b -> a
-- and as both of those definitions:
pconst = \a _ -> a
-- pconst a _ = a

pdouble :: Term s (PInt :--> PInt)
pdouble = \x -> x * 2

-- We don't support rebindable function-bindings on 9.2
#if MIN_VERSION_ghc(9,4,0)
-- demonstrates that internal conversion to lambda works even with multiple
-- alternative patterns and where-bindings
pdouble2 :: Term s (PInt :--> PInt)
pdouble2 (Term (PInt 2)) = Term (PInt 7)
pdouble2 x = bla x
  where bla = (* 2)
#endif

-- demonstrates lambda-case support
pnot :: Term s (PInt :--> PInt)
pnot = \case
  0 -> 1
  _ -> 0

-- just confirming that normal haskell functions still work
hsStrConcat :: String -> String -> String
hsStrConcat a b = a <> b
