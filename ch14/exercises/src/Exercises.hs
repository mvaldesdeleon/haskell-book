module Exercises
  ( sort
  , half
  , reverse
  , capitalizeWord
  , Fool(..)
  , Pool(..)
  , caesar
  , unCaesar
  , vigenere
  , unVigenere
  ) where

import           Cipher    (caesar, rotChar, unCaesar)
import           Data.Char (ord, toUpper)
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

vigenere :: String -> String -> String
vigenere key plain = map encode $ zipWithSpaces plain $ cycle key
  where
    encode (c, k) = rotChar (ord k - ord ' ') c

unVigenere :: String -> String -> String
unVigenere key code = map decode $ zipWithSpaces code $ cycle key
  where
    decode (c, k) = rotChar (negate $ ord k - ord ' ') c

zipWithSpaces :: [Char] -> [Char] -> [(Char, Char)]
zipWithSpaces [] _ = []
zipWithSpaces _ [] = []
zipWithSpaces (a:as) (b:bs) =
  if a == ' '
    then (a, ' ') : zipWithSpaces as (b : bs)
    else (a, b) : zipWithSpaces as bs
