# EnumFromTo

Write your own `enumFromTo` definitions for the types provided. Do not use range syntax to do so. It should return the same results as if you did `[start..stop]`.

```
eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

eft :: (Ord a, Enum a) => a -> a -> [a]
eft f t
    | f >  t = []
    | f == t = [f]
    | f <  t = f : eft (succ f) t
```

We can provide such a generic recursive function because all the target types have `Ord` instances.

# Thy Fearful Symmetry

1. Using `takeWhile` and `dropWhile`, write a function that takes a string and returns a list of strings, using spaces to separate the elements of the string into words.

```
myWords :: String -> [String]
myWords [] = []
myWords str = if length word == 0 then []
              else word : myWords rest
              where isSpace = flip elem " "
                    trim = dropWhile isSpace str
                    word = takeWhile (not . isSpace) trim
                    rest = dropWhile (not . isSpace) trim
```

2. Next, write a function that takes a string and returns a list of strings, using newline separators to break up the string.

```
myLines :: String -> [String]
myLines [] = []
myLines str = if length line == 0 then []
              else line : myLines rest
              where isNewline = flip elem "\n"
                    trim = dropWhile isNewline str
                    line = takeWhile (not . isNewline) trim
                    rest = dropWhile (not . isNewline) trim
```

3. Now let’s look at what those two functions have in common. Try writing a new function that parameterizes the character you’re breaking the string argument on and rewrite `myWords` and `myLines` using it.

```
mySplit :: Char -> String -> [String]
mySplit _ [] = []
mySplit s str = if length term == 0 then []
              else term : mySplit s rest
              where isSeparator = (==) s
                    trim = dropWhile isSeparator str
                    term = takeWhile (not . isSeparator) trim
                    rest = dropWhile (not . isSeparator) trim

myWords' = mySplit ' '
myLines' = mySplit '\n'
```

# Comprehend Thy Lists

```
mySqr = [x^2 | x <- [1..5]]
```

```
[x | x <- mySqr, rem x 2 == 0]
```

`[4, 16]`

```
[(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
```

`[]`

```
take 5 [ (x, y) | x <- mySqr
       , y <- mySqr, x < 50, y > 50 ]
```

`[]`

# Square Cube

Given the following:

```
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
```

1. First write an expression that will make tuples of the outputs of `mySqr` and `myCube`.

`[(x, y) | x <- mySqr, y <- myCube]`

2. Now alter that expression so that it only uses the x and y values that are less than 50.

`[(x, y) | x <- mysqr, y <- mycube, x < 50, y < 50]`

3. Apply another function to that list comprehension to determine how many tuples inhabit your output list.

`length [(x, y) | x <- mysqr, y <- mycube, x < 50, y < 50]`

# Bottom Madness

Will it blow up?

1. `[x^y | x <- [1..5], y <- [2, undefined]]`

`undefined`

2. `take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]`

`[1]`

3. `sum [1, undefined, 3]`

`undefined`

4. `length [1, 2, undefined]`

`3`

5. `length $ [1, 2, 3] ++ undefined`

`undefined`

6. `take 1 $ filter even [1, 2, 3, undefined]`

`[2]`

7. `take 1 $ filter even [1, 3, undefined]`

`undefined`

8. `take 1 $ filter odd [1, 3, undefined]`

`[1]`

9. `take 2 $ filter odd [1, 3, undefined]`

`[1, 3]`

10. `take 3 $ filter odd [1, 3, undefined]`

`undefined`

# Is it in normal form?

For each expression below, determine whether it’s in normal form (which implies weak head normal form), weak head normal form only, or neither.

1. `[1, 2, 3, 4, 5]`

**NF+WHNF**

2. `1 : 2 : 3 : 4 : _`

**WHNF**

3. `enumFromTo 1 10`

**Neither**

4. `length [1, 2, 3, 4, 5]`

**Neither**

5. `sum (enumFromTo 1 10)`

**Neither**

6. `['a'..'m'] ++ ['n'..'z']`

**Neither**

7. `(_, 'b')`

**WHNF**

# More Bottoms

1. Will the following expression return a value or be ⊥?

`take 1 $ map (+1) [undefined, 2, 3]`

`undefined`

2. Will the following expression return a value?

`take 1 $ map (+1) [1, undefined, 3]`

`[2]`

3. Will the following expression return a value?

`take 2 $ map (+1) [1, undefined, 3]`

`undefined`

4. What does the following mystery function do? What is its type? Describe it (to yourself or a loved one) in standard English and then test it out in the REPL to make sure you were correct.

`itIsMystery xs = map (\x -> elem x "aeiou") xs`

`itIsMystery :: [Char] -> [Bool]`

Takes a `String` (`[Char]`) as input and returns a list of `Bool`s, where the Bool at each position indicates wether the string character at that same position was a vowel (`True`) or a consonant (`False`).

5. What will be the result of the following functions:

a) `map (^2) [1..10]`

`[1, 4, 9, 16, ... , 100]`

b) `map minimum [[1..10], [10..20], [20..30]]`

`[1, 10, 20]`

c) `map sum [[1..5], [1..5], [1..5]]`

`[15, 15, 15]`

6. Back in the Functions chapter, you wrote a function called `foldBool`. That function exists in a module known as Data.Bool and is called `bool`. Write a function that does the same (or similar,if you wish) as the `map (if-then-else)` function you saw above but uses `bool` instead of the `if-then-else` syntax. Your first step should be bringing the `bool` function into scope by typing `import Data.Bool` at your Prelude prompt.

`map (\x -> if x == 3 then (-x) else (x)) [1..10]`

`map (\x -> bool x (-x) (x == 3)) [1..10]`

# Filtering

1. Given the above, how might we write a filter function that would give us all the multiples of 3 out of a list from 1-30?

`filter (\x -> (rem x 3) == 0) [1..30]`

2. Recalling what we learned about function composition, how could we compose the above function with the length function to tell us *how many* multiples of 3 there are between 1 and 30?

`length . filter (\x -> (rem x 3) == 0) $ [1..30]`

3. Next we’re going to work on removing all articles (’the’, ’a’, and ’an’) from sentences. You may recall that earlier in this chapter we asked you to write a function that separates a string into a list of strings by separating them at spaces. That is a standard library function called `words`.

```
dearticulate :: String -> [String]
dearticulate = filter (\x -> not . elem x $ ["the", "a", "an"]) . words
```

# Zipping exercises

1. Write your own version of `zip :: [a] -> [b] -> [(a, b)]` and ensure it behaves the same as the original.

```
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (a:as) (b:bs) = (a, b) : zip as bs
```

2. Do what you did for zip, but now for `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`

```
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
```

3. Rewrite your `zip` in terms of the `zipWith` you wrote.

`zip = zipWith (,)`

# Chapter Exercises

## Data.Char

1. Query the types of `isUpper` and `toUpper`.

`isUpper :: Char -> Bool`

`toUpper :: Char -> Char`

2. Write that function such that, given the input “HbEfLrLxO”, your function will return “HELLO”.

```
upperOnly :: String -> String
upperOnly = filter isUpper
```

3. Write a function that will capitalize the first letter of a String and return the entire String. For example, if given the argument
“julie,” it will return “Julie.”

```
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs
```

4. Now make a new version of that function that is recursive such that if you give it the input “woot” it will holler back at you “WOOT.” The type signature won’t change, but you will want to
add a base case.

```
capitalizeAll :: String -> String
capitalizeAll [] = []
capitalizeAll (x:xs) = toUpper x : capitalizeAll xs
```

5. To do the final exercise in this section, we’ll need another standard function for lists called `head`. Query the type of `head` and experiment with it to see what it does. Now write a function that will capitalize the first letter of a String and return only that letter as the result.

```
capitalizeHead :: String -> Char
capitalizeHead xs = toUpper . head $ xs
```

6. Cool. Good work. Now rewrite it as a composed function. Then, for fun, rewrite it pointfree.

```
capitalizeHead :: String -> Char
capitalizeHead = toUpper . head
```

## Ciphers

See [cipher.hs](https://github.com/mvaldesdeleon/haskell-book/blob/master/ch09/cipher.hs)

## Writing your own standard functions

See [stdfuns.hs](https://github.com/mvaldesdeleon/haskell-book/blob/master/ch09/stdfuns.hs)