module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Overloaded.Test.Categories             as Cat
import qualified Overloaded.Test.Chars                  as Chr
import qualified Overloaded.Test.CodeLabels             as Cdl
import qualified Overloaded.Test.CodeStrings            as Cst
import qualified Overloaded.Test.Constructors           as Con
import qualified Overloaded.Test.Do                     as Doo
import qualified Overloaded.Test.If                     as Iff
import qualified Overloaded.Test.Labels                 as Lbl
import qualified Overloaded.Test.Lists                  as Lst
import qualified Overloaded.Test.Lists.Bidi             as Lst.Bidi
import qualified Overloaded.Test.Naturals               as Nat
import qualified Overloaded.Test.Numerals               as Num
import qualified Overloaded.Test.RebindableApplications as Rba
import qualified Overloaded.Test.RecordFields           as Rec
import qualified Overloaded.Test.Strings                as Str
import qualified Overloaded.Test.Symbols                as Sym

import qualified Overloaded.Test.Labels.GenericLens as GL

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Chr.tests
    , Cdl.tests
    , Cst.tests
    , Iff.tests
    , Doo.tests
    , Lbl.tests
    , Lst.tests
    , Lst.Bidi.tests
    , Nat.tests
    , Num.tests
    , Str.tests
    , Sym.tests
    , GL.tests
    , Cat.tests
    , Rba.tests
    , Rec.tests
    , Con.tests
    ]
