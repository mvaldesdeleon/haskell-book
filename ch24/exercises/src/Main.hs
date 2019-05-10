{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Ratio          ((%))
import           Text.RawString.QQ
import           Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

oneEOF = one <* eof

oneTwoEOF = oneTwo <* eof

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

p123 :: String -> IO ()
p123 s = print $ parseString p mempty s
  where
    p = (try (string "123") <|> try (string "12") <|> try (string "1")) <* eof

sprang :: String -> Parser String
sprang = foldr (\c ps -> (:) <$> char c <*> ps) (return "")

inteeg :: Parser Integer
inteeg = integer <* eof

fraction :: Parser Rational
fraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

deceem :: Parser Rational
deceem = do
  int <- decimal
  char '.'
  dec <- decimal
  let exp = (10 ^) . length . show $ dec
  return ((int * exp + dec) % exp)

rational :: Parser Rational
rational = try fraction <|> try deceem

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneEOF:"
  testParse oneEOF
  pNL "oneTwoEOF:"
  testParse oneTwoEOF

-- Chapter Exercises
-- 1
data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Show, Eq)

instance Ord NumberOrString where
  (NOSI _) <= (NOSS _) = True
  (NOSS _) <= (NOSI _) = False
  (NOSI l) <= (NOSI r) = l <= r
  (NOSS l) <= (NOSS r) = l <= r

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Show)

instance Eq SemVer where
  (SemVer lma lmi lpa lre _) == (SemVer rma rmi rpa rre _) =
    lma == rma && lmi == rmi && lpa == rpa && lre == rre

instance Ord SemVer where
  (SemVer lma lmi lpa lre _) `compare` (SemVer rma rmi rpa rre _) =
    lma `compare` rma <> lmi `compare` rmi <> lpa `compare` rpa <> lre `releaseCompare`
    rre
    where
      [] `releaseCompare` [] = EQ
      [] `releaseCompare` _ = GT
      _ `releaseCompare` [] = LT
      lre `releaseCompare` rre = lre `compare` rre

parseSemVer :: Parser SemVer
parseSemVer =
  SemVer <$> decimal <* char '.' <*> decimal <* char '.' <*> decimal <*>
  parseRelease <*>
  parseMetadata <*
  eof
  where
    parseRelease = try (char '-' *> parseNoS `sepBy` char '.') <|> pure []
    parseMetadata = try (char '+' *> parseNoS `sepBy` char '.') <|> pure []
    parseNoS =
      try
        (NOSI . read <$> some digit <* notFollowedBy (choice [letter, char '-'])) <|>
      (NOSS <$> some (choice [alphaNum, char '-']))

-- 2
parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

-- 3
base10Integer' :: Parser Integer
base10Integer' = try (negate <$> (char '-' *> base10Integer)) <|> base10Integer

-- 4
type NumberingPlanArea = Integer

type Exchange = Integer

type LineNumber = Integer

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone =
  PhoneNumber <$> parseNPA <* skipOptional separator <*> digits 3 <*
  skipOptional separator <*>
  digits 4
  where
    parseNPA =
      try (between (char '(') (char ')') (digits 3) <* char ' ') <|>
      try (string "1-" *> digits 3) <|>
      digits 3
    separator = oneOf " -"
    digits n = read <$> replicateM n digit

-- 5
log :: String
log =
  [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]
