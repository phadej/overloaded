module Overloaded.Plugin.V where

data V2 a = V2 a a
  deriving (Eq, Show)

data V4 a = V4 a a a a
  deriving (Eq, Show)
