module Listy where

newtype Listy a =
  Listy [a]
  deriving (Eq, Show)
