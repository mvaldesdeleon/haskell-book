module Cipher where

import Data.Char (chr, ord)

rotFromTo :: Int -> Int -> Int -> Int -> Int
rotFromTo f t n x = (x - f + n) `mod` (t - f) + f

rotInt :: Int -> Int -> Int
rotInt n = rotFromTo 32 126 n

rotChar :: Int -> Char -> Char
rotChar n = chr . rotInt n . ord

caesar :: Int -> String -> String
caesar n = map $ rotChar n

unCaesar :: Int -> String -> String
unCaesar = caesar . negate
