module Exercises
  ( sort
  , half
  , reverse
  , capitalizeWord
  , Fool(..)
  , Pool(..)
  ) where

import           Data.Char (toUpper)
import           Data.List (reverse, sort)

half :: Fractional a => a -> a
half x = x / 2

-- From ch11/exercises.lhs
capitalizeWord :: String -> String
capitalizeWord []     = []
capitalizeWord (c:cs) = toUpper c : cs

data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

data Pool
  = Pulse
  | Prue
  deriving (Eq, Show)
