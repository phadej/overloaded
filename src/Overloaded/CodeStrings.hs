{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Overloaded.CodeStrings where

import Data.Char                  (ord)
import Control.Monad              (when)
import Data.Word                  (Word8)
import Language.Haskell.TH        (TExpQ, appE)
import Language.Haskell.TH.Syntax (lift, reportWarning, unsafeTExpCoerce)

import qualified Data.ByteString as BS

class IsCodeString a where
    codeFromString :: String -> TExpQ a

instance a ~ Char => IsCodeString [a] where
    codeFromString = unsafeTExpCoerce . lift

instance IsCodeString BS.ByteString where
    codeFromString str = do
        when (any (> '\255') str ) $
            reportWarning "Splicing non-ASCII ByteString"

        let octets :: [Word8]
            octets = map (fromIntegral . ord) str

        unsafeTExpCoerce $ [| BS.pack |] `appE` lift octets
