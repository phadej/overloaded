{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Another way to desugar overloaded string literals. See 'FromSymbol'.
module Overloaded.Symbols where

import Data.Proxy        (Proxy (..))
import Data.String       (fromString)
import Data.Symbol.Ascii (ToList)
import GHC.Exts          (Constraint)
import GHC.TypeLits
       (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL

-- | Another way to desugar overloaded string literals using this class.
--
-- A string literal @"example"@ is desugared to
--
-- @
-- 'fromSymbol' \@"example"
-- @
--
-- Enabled with:
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Symbols #-}
-- @
--
-- One can do type-level computations with this.
--
class FromSymbol (s :: Symbol) a where
    fromSymbol :: a

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance (KnownSymbol s, a ~ Char) => FromSymbol s [a] where
    fromSymbol = symbolVal (Proxy :: Proxy s)

-------------------------------------------------------------------------------
-- text
-------------------------------------------------------------------------------

instance KnownSymbol s => FromSymbol s T.Text where
    fromSymbol = fromString (symbolVal (Proxy :: Proxy s))

instance KnownSymbol s => FromSymbol s TL.Text where
    fromSymbol = fromString (symbolVal (Proxy :: Proxy s))

-------------------------------------------------------------------------------
-- bytestring
-------------------------------------------------------------------------------

type family SeqList (xs :: [Symbol]) :: Constraint where
    SeqList '[]       = ()
    SeqList (x ': xs) = SeqList xs
    SeqList xs        = TypeError ('Text "Cannot reduce list " ':$$: 'ShowType xs)

instance (KnownSymbol s, SeqList (ToList s)) => FromSymbol s BS.ByteString where
    fromSymbol = fromString (symbolVal (Proxy :: Proxy s))

instance (KnownSymbol s, SeqList (ToList s)) => FromSymbol s BSL.ByteString where
    fromSymbol = fromString (symbolVal (Proxy :: Proxy s))
