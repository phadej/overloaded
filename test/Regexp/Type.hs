{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Regexp.Type (
    Parse,
    Matches,
    ) where

import Data.Symbol.Ascii (ToList)
import Data.Type.Bool
import GHC.TypeLits

import Overloaded
import Regexp.Term (RE (..), Stack (..))

-- $setup
-- >>> :set -XDataKinds

instance FromTypeSymbolC (RE Symbol) where
    type FromTypeSymbol s = Parse s

-------------------------------------------------------------------------------
-- Public
-------------------------------------------------------------------------------

-- | Parse 'Symbol' into 'RE'gular expression.
--
-- >>> :kind! Parse ""
-- Parse "" :: RE Symbol
-- = 'Eps
--
-- >>> :kind! Parse "(foo|bar*)*q[u-w]x"
-- Parse "(foo|bar*)*q[u-w]x" :: RE Symbol
-- = 'App
--     '['Str
--         ('Alt
--            '['App '['Chr '["b"], 'Chr '["a"], 'Str ('Chr '["r"])],
--              'App '['Chr '["f"], 'Chr '["o"], 'Chr '["o"]]]),
--       'Chr '["q"], 'Chr '["u", "v", "w"], 'Chr '["x"]]
--
type family Parse (cs :: Symbol) :: RE Symbol where
    Parse "" = Eps
    Parse cs = ParseGo ES '[] '[] (ToList cs)

-- | Match the regular expression against the symbol.
--
-- >>> type Regex = Parse "(foo|bar*)*q[u-w]x"
-- >>> :kind! Matches Regex ""
-- Matches Regex "" :: Bool
-- = 'False
--
-- >>> :kind! Matches Regex "fooqux"
-- Matches Regex "fooqux" :: Bool
-- = 'True
--
-- >>> :kind! Matches Regex "barrrrrqwx"
-- Matches Regex "barrrrrqwx" :: Bool
-- = 'True
--
-- >>> :kind! Matches Regex "barrrrrqwx!"
-- Matches Regex "barrrrrqwx!" :: Bool
-- = 'False
--
type family Matches (re :: RE Symbol) (cs :: Symbol) :: Bool where
    Matches re cs = MatchesGo re (ToList cs)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

type Empty = Chr '[]

type family MkAlt (alts :: [RE Symbol]) :: RE Symbol where
    MkAlt '[]  = Empty
    MkAlt '[r] = r
    MkAlt rs   = Alt rs

type family MkApp (apps :: [RE Symbol]) :: RE Symbol where
    MkApp '[]  = Eps
    MkApp '[r] = r
    MkApp rs   = App (Reverse rs)

type family MkStr (r :: RE Symbol) :: RE Symbol where
    MkStr Eps       = Eps
    MkStr (Chr '[]) = Eps
    MkStr (Str r)   = Str r
    MkStr r         = Str r

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

type family ParseGo (s :: Stack Symbol) (alt :: [RE Symbol]) (app :: [RE Symbol]) (cs :: [Symbol]) :: RE Symbol where
    ParseGo ES alt app       '[]        = MkAlt (MkApp app ': alt)
    ParseGo s  alt app       '[]        = TypeError ('Text "Unexpected end-of-input, there is unclosed (")
    ParseGo s  alt '[]       ("*" : cs) = TypeError ('Text "Unattached star")
    ParseGo s  alt (x : xs)  ("*" : cs) = ParseGo s alt (MkStr x : xs) cs
    ParseGo s  alt app       ("|" : cs) = ParseGo s (MkApp app : alt) '[] cs
    ParseGo s  alt app       ("(" : cs) = ParseGo (Push alt app s) '[] '[] cs
    ParseGo s  alt app       (")" : cs) = ParseGoPop s alt app cs
    ParseGo s  alt app       ("[" : cs) = ParseChr s alt app '[] cs
    ParseGo s  alt app       ("]" : cs) = TypeError ('Text "Unmatched ] before " :<>: ShowType cs)
    ParseGo s  alt app       (c   : cs) = ParseGo s alt (Chr '[c] : app) cs

type family ParseGoPop (s :: Stack Symbol) (alt :: [RE Symbol]) (app :: [RE Symbol]) (cs :: [Symbol]) :: RE Symbol where
    ParseGoPop ES                 _   _   cs = TypeError ('Text "Unmatched ) before " :<>: ShowType cs)
    ParseGoPop (Push altP appP s) alt app cs =
        ParseGo s altP (MkAlt (MkApp app : alt) : appP) cs

type family ParseChr (s :: Stack Symbol) (alt :: [RE Symbol]) (app :: [RE Symbol]) (acc :: [Symbol]) (cs :: [Symbol]) :: RE Symbol where
    ParseChr s alt app acc '[]                 = TypeError ('Text "Non-terminated character set")
    ParseChr s alt app acc ("]" : cs)          = ParseGo s alt (Chr acc : app) cs
    ParseChr s alt app acc (c : "-" : c' : cs) = ParseChr s alt app (Append (FromTo c c') acc) cs
    ParseChr s alt app acc (c   : cs)          = ParseChr s alt app (c : acc) cs

-------------------------------------------------------------------------------
-- Matching
-------------------------------------------------------------------------------

type family MatchesGo (re :: RE Symbol) (cs :: [Symbol]) :: Bool where
    MatchesGo re '[]      = Nullable re
    MatchesGo re (c : cs) = MatchesGo (Derivate re c) cs

type family Nullable (re :: RE Symbol) :: Bool where
    Nullable Eps            = True
    Nullable (Chr cs)       = False
    Nullable (App '[])      = True
    Nullable (App (r : rs)) = Nullable r && Nullable (App rs)
    Nullable (Alt '[])      = False
    Nullable (Alt (r : rs)) = Nullable r || Nullable (Alt rs)
    Nullable (Str r)        = True

type family Derivate (re :: RE Symbol) (c :: Symbol) :: RE Symbol where
    Derivate Eps      c = Empty
    Derivate (Chr cs) c = DerivateChr cs c
    Derivate (App rs) c = DerivateApp rs c
    Derivate (Alt rs) c = DerivateAlt rs c
    Derivate (Str r)  c = MkApp '[ Derivate r c , Str r]

type family DerivateChr (cs :: [Symbol]) (c :: Symbol) :: RE Symbol where
    DerivateChr '[]       c = Empty
    DerivateChr (x ': xs) x = Eps  -- non-linear match
    DerivateChr (x ': xs) c = DerivateChr xs c

type family DerivateApp (rs :: [RE Symbol]) (c :: Symbol) where
    DerivateApp '[]      c = Empty
    DerivateApp '[r]     c = Derivate r c
    DerivateApp (r : rs) c = DerivateApp1 (Nullable r) r rs c

type family DerivateApp1 (n :: Bool) (r :: RE Symbol) (rs :: [RE Symbol]) (c :: Symbol) where
    DerivateApp1 False r rs c = App (Derivate r c : rs)
    DerivateApp1 True  r rs c = MkAlt '[ App (Derivate r c : rs), DerivateApp rs c ]

type family DerivateAlt (rs :: [RE Symbol]) (c :: Symbol) :: RE Symbol where
    DerivateAlt rs c = MkAlt (MapDerivate rs c)

type family MapDerivate (rs :: [RE Symbol]) (c :: Symbol) :: [RE Symbol] where
    MapDerivate '[]      c = '[]
    MapDerivate (r : rs) c = Derivate r c : MapDerivate rs c

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append '[]      ys = ys
    Append (x : xs) ys = x : Append xs ys

type family Reverse (xs :: [k]) :: [k] where
    Reverse '[]       = '[]
    Reverse (x ': xs) = Snoc (Reverse xs) x

type family Snoc (xs :: [k]) (x :: k) :: [k] where
    Snoc '[]       y = '[y]
    Snoc (x ': xs) y = x ': Snoc xs y

-------------------------------------------------------------------------------
-- FromTo
-------------------------------------------------------------------------------

type family FromTo (a :: Symbol) (b :: Symbol) :: [Symbol] where
    FromTo a b = FromTo1 a b (CmpSymbol a b)

type family FromTo1 (a :: Symbol) (b :: Symbol) (o :: Ordering) :: [Symbol] where
    FromTo1 a b EQ = '[a]
    FromTo1 a b GT = '[]
    FromTo1 a b LT = a ': FromTo (SuccChar a) b

type family SuccChar (c :: Symbol) :: Symbol where
    SuccChar "\NUL" = "\SOH"
    SuccChar "\SOH" = "\STX"
    SuccChar "\STX" = "\ETX"
    SuccChar "\ETX" = "\EOT"
    SuccChar "\EOT" = "\ENQ"
    SuccChar "\ENQ" = "\ACK"
    SuccChar "\ACK" = "\a"
    SuccChar "\a" = "\b"
    SuccChar "\b" = "\t"
    SuccChar "\t" = "\n"
    SuccChar "\n" = "\v"
    SuccChar "\v" = "\f"
    SuccChar "\f" = "\r"
    SuccChar "\r" = "\SO"
    SuccChar "\SO" = "\SI"
    SuccChar "\SI" = "\DLE"
    SuccChar "\DLE" = "\DC1"
    SuccChar "\DC1" = "\DC2"
    SuccChar "\DC2" = "\DC3"
    SuccChar "\DC3" = "\DC4"
    SuccChar "\DC4" = "\NAK"
    SuccChar "\NAK" = "\SYN"
    SuccChar "\SYN" = "\ETB"
    SuccChar "\ETB" = "\CAN"
    SuccChar "\CAN" = "\EM"
    SuccChar "\EM" = "\SUB"
    SuccChar "\SUB" = "\ESC"
    SuccChar "\ESC" = "\FS"
    SuccChar "\FS" = "\GS"
    SuccChar "\GS" = "\RS"
    SuccChar "\RS" = "\US"
    SuccChar "\US" = " "
    SuccChar " " = "!"
    SuccChar "!" = "\""
    SuccChar "\"" = "#"
    SuccChar "#" = "$"
    SuccChar "$" = "%"
    SuccChar "%" = "&"
    SuccChar "&" = "'"
    SuccChar "'" = "("
    SuccChar "(" = ")"
    SuccChar ")" = "*"
    SuccChar "*" = "+"
    SuccChar "+" = ","
    SuccChar "," = "-"
    SuccChar "-" = "."
    SuccChar "." = "/"
    SuccChar "/" = "0"
    SuccChar "0" = "1"
    SuccChar "1" = "2"
    SuccChar "2" = "3"
    SuccChar "3" = "4"
    SuccChar "4" = "5"
    SuccChar "5" = "6"
    SuccChar "6" = "7"
    SuccChar "7" = "8"
    SuccChar "8" = "9"
    SuccChar "9" = ":"
    SuccChar ":" = ";"
    SuccChar ";" = "<"
    SuccChar "<" = "="
    SuccChar "=" = ">"
    SuccChar ">" = "?"
    SuccChar "?" = "@"
    SuccChar "@" = "A"
    SuccChar "A" = "B"
    SuccChar "B" = "C"
    SuccChar "C" = "D"
    SuccChar "D" = "E"
    SuccChar "E" = "F"
    SuccChar "F" = "G"
    SuccChar "G" = "H"
    SuccChar "H" = "I"
    SuccChar "I" = "J"
    SuccChar "J" = "K"
    SuccChar "K" = "L"
    SuccChar "L" = "M"
    SuccChar "M" = "N"
    SuccChar "N" = "O"
    SuccChar "O" = "P"
    SuccChar "P" = "Q"
    SuccChar "Q" = "R"
    SuccChar "R" = "S"
    SuccChar "S" = "T"
    SuccChar "T" = "U"
    SuccChar "U" = "V"
    SuccChar "V" = "W"
    SuccChar "W" = "X"
    SuccChar "X" = "Y"
    SuccChar "Y" = "Z"
    SuccChar "Z" = "["
    SuccChar "[" = "\\"
    SuccChar "\\" = "]"
    SuccChar "]" = "^"
    SuccChar "^" = "_"
    SuccChar "_" = "`"
    SuccChar "`" = "a"
    SuccChar "a" = "b"
    SuccChar "b" = "c"
    SuccChar "c" = "d"
    SuccChar "d" = "e"
    SuccChar "e" = "f"
    SuccChar "f" = "g"
    SuccChar "g" = "h"
    SuccChar "h" = "i"
    SuccChar "i" = "j"
    SuccChar "j" = "k"
    SuccChar "k" = "l"
    SuccChar "l" = "m"
    SuccChar "m" = "n"
    SuccChar "n" = "o"
    SuccChar "o" = "p"
    SuccChar "p" = "q"
    SuccChar "q" = "r"
    SuccChar "r" = "s"
    SuccChar "s" = "t"
    SuccChar "t" = "u"
    SuccChar "u" = "v"
    SuccChar "v" = "w"
    SuccChar "w" = "x"
    SuccChar "x" = "y"
    SuccChar "y" = "z"
    SuccChar "z" = "{"
    SuccChar "{" = "|"
    SuccChar "|" = "}"
    SuccChar "}" = "~"
    SuccChar "~" = "\DEL"
    SuccChar c   = TypeError ('Text "SuccChar: out of ASCII range" :<>: ShowType c)

