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
    --
    -- generic-lens example!
  ) where

import Overloaded.Chars
import Overloaded.If
import Overloaded.Lists
import Overloaded.Naturals
import Overloaded.Numerals
import Overloaded.Plugin
import Overloaded.Symbols