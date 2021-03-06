{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Overloaded.CodeStrings where

import Control.Monad                     (when)
import Data.Char                         (ord)
import Data.Word                         (Word8)
import Language.Haskell.TH               (appE)
import Language.Haskell.TH.Syntax        (lift, reportWarning)
import Language.Haskell.TH.Syntax.Compat (SpliceQ, unsafeSpliceCoerce)

import qualified Data.ByteString as BS

-- | Class for auto-spliced string literals
--
-- The string literals @"beer"@ is desugared into @$$(codeFromString \@"beer")@ splice.
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:CodeLabels #-}
-- @
--
-- This feature is not very usable, see https://gitlab.haskell.org/ghc/ghc/-/issues/18211
class IsCodeString a where
    codeFromString :: String -> SpliceQ a

instance a ~ Char => IsCodeString [a] where
    codeFromString = unsafeSpliceCoerce . lift

instance IsCodeString BS.ByteString where
    codeFromString str = unsafeSpliceCoerce $ do
        when (any (> '\255') str ) $
            reportWarning "Splicing non-ASCII ByteString"

        let octets :: [Word8]
            octets = map (fromIntegral . ord) str

        [| BS.pack |] `appE` lift octets
