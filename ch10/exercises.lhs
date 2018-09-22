Understanding Folds
===================

1. foldr (*) 1 [1..5]

will return the same result as which of the following:

a) flip (*) 1 [1..5]
b) foldl (flip (*)) 1 [1..5]
c) foldl (*) 1 [1..5]

Answer: b and c.

2. Write out the evaluation steps for

foldl (flip (*)) 1 [1..3]

Given th definition provided in the chapter:

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

The evaluation steps would be as follows:

foldl (flip (*)) 1 [1..3]
foldl (flip (*)) ((flip (*)) 1 1) [2, 3]
foldl (flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) [3]
foldl (flip (*)) ((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3) []
((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3)
((flip (*)) ((flip (*)) 1 2) 3)
((flip (*)) 2 3)
6

3. One difference between foldr and foldl is:

a) foldr, but not foldl, traverses the spine of a list from right to left
b) foldr, but not foldl, always forces the rest of the fold
c) foldr, but not foldl, associates to the right
d) foldr, but not foldl, is recursive

Answer: c.

Both traverse the spine in the same direction. foldl always forces the rest of the fold, not foldr. And both are recursive.

4. Folds are catamorphisms, which means they are generally used to

a) reduce structure
b) expand structure
c) render you catatonic
d) generate infinite data structures

Answer: a.

5. The following are simple folds very similar to what you've already seen, but each has at least one error. Please fix them and test in your REPL:

a) foldr (++) ["woot", "WOOT", "woot"]

foldr (++) "" ["woot", "woot", "woot"]

b) foldr max [] "fear is the little death"

foldr max 'a' "fear is the little death"

c) foldr and True [False, True]

foldr (&&) True [False, True]

d) foldr (||) True [False, True]

foldr (||) False [False, True]

e) foldl ((++) . show) "" [1..5]

foldr ((++) . show) "" [1..5]

f) foldr const 'a' [1..5]

foldr (flip const) 'a' [1..5]

g) foldr const 0 "tacos"

foldl const 0 "tacos"

h) foldl (flip const) 0 "burritos"

foldr (flip const) 0 "burritos"

i) foldl (flip const) 'z' [1..5]

foldl const 'z' [1..5]


Database Processing
===================

> import Data.Time
>
> data DatabaseItem = DbString String
>                   | DbNumber Integer
>                   | DbDate UTCTime
>                   deriving (Eq, Ord, Show)
>
> theDatabase :: [DatabaseItem]
> theDatabase =
>   [ DbDate (UTCTime
>       (fromGregorian 1911 5 1)
>       (secondsToDiffTime 34123))
>   , DbNumber 9001
>   , DbString "Hello, world!"
>   , DbDate (UTCTime
>       (fromGregorian 1921 5 1)
>       (secondsToDiffTime 34123))
>  ]
>

1. Write a function that filters for DbDate values and returns a list of the UTCTimes values unside them.

> filterDbDate :: [DatabaseItem] -> [UTCTime]
> filterDbDate = foldr concatDates []
>   where concatDates (DbDate t) ts = t : ts
>         concatDates _          ts = ts

2. Write a function that filters for DbNumber values and returns a list of the Integer values inside them.

> filterDbNumber :: [DatabaseItem] -> [Integer]
> filterDbNumber = foldr concatNumbers []
>   where concatNumbers (DbNumber n) ns = n : ns
>         concatNumbers _            ns = ns

3. Write a function that gets the most recent date.

> mostRecent :: [DatabaseItem] -> UTCTime
> mostRecent = foldr maxDate (UTCTime (ModifiedJulianDay 0) 0)
>  where maxDate (DbDate t) mt = max t mt
>        maxDate _          mt = mt

4. Write a function that sums all of the DbNumber values.

> sumDb :: [DatabaseItem] -> Integer
> sumDb = foldr sumNumbers 0
>  where sumNumbers (DbNumber n) sn = n + sn
>        sumNumbers _            sn = sn

5. Write a function that gets the average of the DbNumber values.

> -- You'll probably need to use fromIntegral
> -- to get from Integer to Double.
> avgDb :: [DatabaseItem] -> Double
> avgDb db = fromIntegral (sumDb db) / fromIntegral (length $ filterDbNumber db)


Scans Exercises
===============

> fibs = 1 : scanl (+) 1 fibs
> fibsN x = fibs !! x

1. Modify your fibs function to only return the first 20 Fibonacci numbers.

> fibs' = take 20 fibs

2. Modify fibs to return the Fibonacci numbers that are less than 100.

> fibs'' = takeWhile (<100) fibs

3. Try to write the factorial function from Recursion as a scan. You'll want scanl again, and your start value will be 1. Warning: this will also generate an infinite list, so you may want to pass it trhough a take function or similar.

> factorial :: Integer -> Integer
> factorial 0 = 1
> factorial n = n * factorial (n - 1)
>
> facts :: [Integer]
> facts = scanl (*) 1 [1..]
>
> factorial' :: Integer -> Integer
> factorial' n = facts !! (fromInteger n)


Chapter Exercises
=================

Warm-up and review
------------------

1. Given the following sets of consonants and vowels

> stops = "pbtdkg"
> vowels = "aeiou"

a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combinations. These will not all correspond to real words in English, although the stop-vowel-stop pattern is common enough that many of them will.

> svs = [[s1, v, s2] | s1 <- stops, v <- vowels, s2 <- stops]

b) Modify that function so that it only returns the combinations that begin with a p.

> svs' = [[s1, v, s2] | s1 <- stops, v <- vowels, s2 <- stops, s1 == 'p']

c) Now set up lists of nouns and verbs (instead of stops and vowels) and modify the function to make tuples representing possible noun-verb-noun sentences.

> nouns = ["dog", "cat", "bird", "tomato", "peach"]
> verbs = ["eat", "drink", "kill"]
>
> sentences = [n1  ++ " " ++ v ++ " " ++ n2 | n1 <- nouns, v <- verbs, n2 <- nouns]

2. What does the following mystery functon do? What is its type? Try to get a good sense of what it does before you test it in the REPL to verify it.

> seekritFunc x =
>   div (sum (map length (words x)))
>       (length (words x))

> seekritFunc :: String -> Int

It calculates the average word length of the input string.

3. We'd really like the answer to be more precise. Can you rewrite that using fractional division?

> seekritFunc' x =
>   fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))


Rewriting functions using folds
-------------------------------

1. myOr returns True if any Bool in the list is True.

> myOr :: [Bool] -> Bool
> myOr = foldr (||) False

2. myAny returns True if a -> Bool applied to any of the values in the list returns True.

> myAny :: (a -> Bool) -> [a] -> Bool
> myAny f = foldr (||) False . map f

3. Write two versions of myElem. One version should use folding and the other should use any.

> myElem :: Eq a => a -> [a] -> Bool
> myElem e = foldr (\a b -> b || a == e) False
>
> myElem' :: Eq a => a -> [a] -> Bool
> myElem' e = any (== e)

4. Implement myReverse, don't worry about trying to make it lazy.

> myReverse :: [a] -> [a]
> myReverse = foldl (flip (:)) []

5. Write myMap in terms of foldr. It should have the same behaviour as the built-in map.

> myMap :: (a -> b ) -> [a] -> [b]
> myMap f = foldr ((:) . f) []

6. Write myFilter in terms of foldr. It should have the same behaviour as the built-in filter.

> myFilter :: (a -> Bool) -> [a] -> [a]
> myFilter f = foldr (\a b -> if f a then (a:b) else b) []

7. squish flattens a list of lists into a list.

> squish :: [[a]] -> [a]
> squish = foldr (++) []

8. squishMap maps a function over a list and concatenates the results.

> squishMap :: (a -> [b]) -> [a] -> [b]
> squishMap f = foldr ((++) . f) []

9. squishAgain flattens a list of lists into a list. This time re-use the squishMap function.

> squishAgain :: [[a]] -> [a]
> squishAgain = squishMap id

10. myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returned GT for.

> myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
> myMaximumBy f = head . foldr (\a b ->
>                          case b of
>                            [] -> [a]
>                            (b:bs) -> if f a b == GT then [a] else [b]
>                       ) []

11. myMinimumBy takes a comparison function and a list and returns the last element of the list based on the last value that the comparison returned LT for.

> myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
> myMinimumBy f as = case as of
>                      [] -> undefined
>                      (a:as) -> foldr (\a b -> if f a b == LT then a else b) a as
