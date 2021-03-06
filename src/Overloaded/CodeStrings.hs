{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Overloaded.CodeStrings where

import Control.Monad                     (when)
import Data.Char                         (ord)
import Data.Word                         (Word8)
import Language.Haskell.TH               (appE)
import Language.Haskell.TH.Syntax        (lift, reportWarning, unsafeTExpCoerce)
import Language.Haskell.TH.Syntax.Compat (SpliceQ)

import qualified Data.ByteString as BS

-- | Class for auto-spliced string literals
--
-- The string literals @"beer"@ is desugared into @$$(codeFromString \@"beer")@ splice.
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:CodeLabels #-}
-- @
class IsCodeString a where
    codeFromString :: String -> SpliceQ a

instance a ~ Char => IsCodeString [a] where
    codeFromString = unsafeTExpCoerce . lift

instance IsCodeString BS.ByteString where
    codeFromString str = do
        when (any (> '\255') str ) $
            reportWarning "Splicing non-ASCII ByteString"

        let octets :: [Word8]
            octets = map (fromIntegral . ord) str

        unsafeTExpCoerce $ [| BS.pack |] `appE` lift octets
