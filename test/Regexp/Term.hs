module Regexp.Term where

-- | Simple regular expressions.
data RE a
    = Eps         -- ^ empty string
    | Chr [a]     -- ^ character "sets", empty list means Empty!
    | App [RE a]  -- ^ append
    | Alt [RE a]  -- ^ union
    | Str (RE a)  -- ^ star
  deriving (Show)

mkApp :: [RE a] -> RE a
mkApp []  = Eps
mkApp [r] = r
mkApp rs  = App rs

mkAlt :: [RE a] -> RE a
mkAlt []  = Chr []
mkAlt [r] = r
mkAlt rs  = Alt rs

mkStr :: RE a -> RE a
mkStr Eps      = Eps
mkStr (Chr []) = Eps
mkStr (Str a)  = Str a
mkStr a        = Str a

type M = Either String

data Stack a
    = ES -- empty stack
    | Push [RE a] [RE a] (Stack a)
  deriving (Show)

type Result = M (RE Char)

parse :: [Char] -> Result
parse = parseGo ES [] []

mkAlt', mkApp' :: [RE a] -> RE a
mkAlt' = mkAlt . reverse
mkApp' = mkApp . reverse

parseGo
    :: Stack Char  -- ^ stack
    -> [RE Char]   -- ^ previous alternatives
    -> [RE Char]   -- ^ previous appendables
    -> [Char]      -- ^ input
    -> Result
parseGo ES alt app    []         = Right (mkAlt' (mkApp' app : alt))
parseGo s  alt app    []         = Left (show $ "Non-empty stack at end-of-input " ++ show s)
parseGo s  alt []     ('*' : cs) = Left (show "Unattached star")
parseGo s  alt (x:xs) ('*' : cs) = parseGo s alt (mkStr x : xs) cs
parseGo s  alt app    ('|' : cs) = parseGo s (mkApp' app : alt) [] cs
parseGo s  alt app    ('(' : cs) = parseGo (Push alt app s) [] [] cs
parseGo s  alt app    (')' : cs) = case s of
    ES                -> Left (show $ "Unmatched ) before " ++ cs)
    Push altP appP sP -> parseGo sP altP (mkAlt' (mkApp' app : alt) : appP) cs
parseGo s  alt app    ('[' : cs) = parseChr s alt app [] cs
parseGo s  alt app    (']' : cs) = Left ("Unmatched ] before " ++ cs)
parseGo s  alt app    (c   : cs) = parseGo s alt (Chr [c] : app) cs

parseChr
    :: Stack Char  -- ^ stack
    -> [RE Char]   -- ^ previous alternatives
    -> [RE Char]   -- ^ previous appendables
    -> [Char]      -- ^ characters accumulator
    -> [Char]      -- ^ input
    -> Result
parseChr s alt app acc []                  = Left "Non-terminated character set at end-of-input"
parseChr s alt app acc (']' : cs)          = parseGo s alt (Chr acc : app) cs
parseChr s alt app acc (c : '-' : c' : cs) = parseChr s alt app ([c .. c'] ++ acc) cs
parseChr s alt app acc (c   : cs)          = parseChr s alt app (c : acc) cs
