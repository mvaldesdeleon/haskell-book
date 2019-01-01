> module Chapter12 where

Chapter Exercises

Determine the kinds

1. Given

id :: a -> a

What is the kind of a?

a :: *

2. r :: a -> f a

What are the kinds of a and f?

a, f a :: *

f :: * -> *

String processing

Because this is the kind of thing linguists ahem enjoy doing in their spare time.

1. Write a recursive function named replaceThe which takes a text/string, breaks it into words and replaces each instance of “the” with “a”.

It’s intended only to replace exactly the word “the”. notThe is a suggested helper function for accomplishing this.

> notThe :: String -> Maybe String
> notThe s =
>   if s /= "the"
>       then Just s
>       else Nothing
>
> replaceThe :: String -> String
> replaceThe = unwords . go . words
>   where
>       go [] = []
>       go (w:ws) =
>           case notThe w of
>               Just w -> w : go ws
>               Nothing -> "a" : go ws

2. Write a recursive function that takes a text/string, breaks it into words, and counts the number of instances of ”the” followed by a vowel-initial word.

> countTheBeforeVowel :: String -> Integer
> countTheBeforeVowel = go . words
>   where
>       go (wa:wb:ws) =
>           case notThe wa of
>               Just _ -> go (wb:ws)
>               Nothing ->
>                   if vowelInitial wb
>                       then 1 + go (wb:ws)
>                       else go (wb:ws)
>       go _ = 0
>
> vowelInitial :: String -> Bool
> vowelInitial [] = False
> vowelInitial (x:_) = isVowel x
>
> isVowel :: Char -> Bool
> isVowel c = c `elem` vowels

3. Return the number of letters that are vowels in a word.

Hint: it’s helpful to break this into steps. Add any helper functions necessary to achieve your objectives.

a) Test for vowelhood
b) Return the vowels of a string
c) Count the number of elements returned

> countVowels :: String -> Integer
> countVowels = fromIntegral . length . filter (isVowel)

Validate the word

Use the Maybe type to write a function that counts the number of vowels in a string and the number of consonants. If the number of vowels exceeds the number of consonants, the function returns Nothing. In many human languages, vowels rarely exceed the number of consonants so when they do, it may indicate the input isn’t a word (that is, a valid input to your dataset):

> newtype Word' =
>   Word' String deriving (Eq, Show)
>
> vowels = "aeiou"
>
> countConsonants :: String -> Integer
> countConsonants = fromIntegral . length . filter (not . isVowel)
>
> mkWord :: String -> Maybe Word'
> mkWord s
>   | countVowels s > countConsonants s = Nothing
>   | otherwise = Just $ Word' s

It’s only Natural

You’ll be presented with a datatype to represent the natural numbers. The only values representable with the naturals are whole numbers from zero to infinity. Your task will be to implement functions to convert Naturals to Integers and Integers to Naturals. The conversion from Naturals to Integers won’t return Maybe because Integer is a strict superset of Natural. Any Natural can be represented by an Integer, but the same is not true of any Integer. Negative numbers are not valid natural numbers.

> -- As natural as any
> -- competitive bodybuilder
> data Nat =
>     Zero
>   | Succ Nat
>   deriving (Eq, Show)
>
> natToInteger :: Nat -> Integer
> natToInteger Zero = 0
> natToInteger (Succ n) = 1 + natToInteger n
>
> integerToNat :: Integer -> Maybe Nat
> integerToNat i =
>   if i >= 0
>       then Just $ go i
>       else Nothing
>   where
>       go 0 = Zero
>       go i = Succ (go $ i - 1)

Small library for Maybe

Write the following functions.

This may take some time.

1. Simple boolean checks for Maybe values.

> isJust :: Maybe a -> Bool
> isJust Nothing = False
> isJust _ = True
>
> isNothing :: Maybe a -> Bool
> isNothing Nothing = True
> isNothing _ = False

2. The following is the Maybe catamorphism. You can turn a Maybe value into anything else with this.

> mayybee :: b -> (a -> b) -> Maybe a -> b
> mayybee b f Nothing = b
> mayybee b f (Just a) = f a

3. In case you just want to provide a fallback value. Try writing it in terms of the maybe catamorphism.

> fromMaybe :: a -> Maybe a -> a
> fromMaybe a = mayybee a id

4. Converting between List and Maybe.

> listToMaybe :: [a] -> Maybe a
> listToMaybe [] = Nothing
> listToMaybe (a:_) = Just a
>
> maybeToList :: Maybe a -> [a]
> maybeToList Nothing = []
> maybeToList (Just a) = [a]

5. For when we want to drop the Nothing values from our list.

> catMaybes :: [Maybe a] -> [a]
> catMaybes [] = []
> catMaybes (ma:ms) =
>   case ma of
>       Just a -> a : catMaybes ms
>       Nothing -> catMaybes ms

6. You’ll see this called “sequence” later.

> flipMaybe :: [Maybe a] -> Maybe [a]
> flipMaybe [] = Just []
> flipMaybe (ma:ms) =
>   case ma of
>       Just a -> (a :) <$> flipMaybe ms
>       Nothing -> Nothing
