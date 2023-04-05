-- | @Overloaded*@ language extensions as a source plugin.
module Overloaded (
    -- * Plugin
    plugin,

    -- * Overloaded:Symbols
    FromSymbol (..),

    -- * Overloaded:Strings
    -- | See "Data.String" for 'Data.String.fromString'.

    -- * Overloaded:Numerals
    FromNumeral (..),
    defaultFromNumeral,

    -- * Overloaded:Naturals
    FromNatural (..),

    -- * Overloaded:Chars
    FromChar (..),

    -- * Overloaded:Lists
    Nil (..), Cons (..),

    -- * Overloaded:If
    ToBool  (..),
    ifte,

    -- * Overloaded:Labels
    -- | See "GHC.OverloadedLabels" for 'GHC.OverloadedLabels.fromLabel'.

    -- * Overloaded:CodeLabels
    IsCodeLabel (..),

    -- * Overloaded:CodeStrings
    IsCodeString (..),

    -- * Overloaded:TypeNats
    FromNatC (..),

    -- * Overloaded:TypeSymbols
    FromTypeSymbolC (..),

    -- * Overloaded:Do
    DoMethod (..), Pure, Then, Bind, Monad' (..),

    -- * Overloaded:Categories
    Category,
    identity,
    (%%),
    CartesianCategory (..),
    CocartesianCategory (..),
    BicartesianCategory (..),
    CCC (..),

    -- * Overloaded:RecordFields
    -- | See "GHC.Records.Compat"  from @record-hasfield@ package.

    -- * Overloaded:RebindableApplication
    Apply (..),

    -- * Overloaded:RebindableAbstraction
    Lam (..),
  ) where

import Overloaded.Categories
import Overloaded.Chars
import Overloaded.CodeLabels
import Overloaded.CodeStrings
import Overloaded.Do
import Overloaded.If
import Overloaded.Lists
import Overloaded.Naturals
import Overloaded.Numerals
import Overloaded.Plugin
import Overloaded.RebindableAbstraction
import Overloaded.RebindableApplication
import Overloaded.Symbols
import Overloaded.TypeNats
import Overloaded.TypeSymbols
