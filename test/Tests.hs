module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Overloaded.Test.Chars    as Chr
import qualified Overloaded.Test.If       as Iff
import qualified Overloaded.Test.Labels   as Lbl
import qualified Overloaded.Test.Lists    as Lst
import qualified Overloaded.Test.Naturals as Nat
import qualified Overloaded.Test.Numerals as Num
import qualified Overloaded.Test.Strings  as Str
import qualified Overloaded.Test.Symbols  as Sym

import qualified Overloaded.Test.Labels.GenericLens as GL

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Chr.tests
    , Iff.tests
    , Lbl.tests
    , Lst.tests
    , Nat.tests
    , Num.tests
    , Str.tests
    , Sym.tests
    , GL.tests
    ]