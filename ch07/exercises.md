# Grab Bag

Note the following exercises are from source code files, not written for use directly in the REPL. Of course, you can change them to test directly in the REPL if you prefer.

1. Which (two or more) of the following are equivalent?

   a) `mTh x y z = x * y * z`  
   b) `mTh x y = \z -> x * y * z`  
   c) `mTh x = \y -> \z -> x * y * z`  
   d) `mTh = \x -> \y -> \z -> x * y * z`

   They are all equivalent.

2. The type of `mTh` (above) `isNum a => a -> a -> a -> a`. Which is the type of `mTh 3`?

   a) `Integer -> Integer -> Integer`  
   b) `Num a => a -> a -> a -> a`  
   c) `Num a => a -> a`  
   d) `Num a => a -> a -> a`

   D, `Num a => a -> a -> a`.

3. Next, we’ll practice writing anonymous lambda syntax. For example, one could rewrite:

   `addOne x = x + 1`

   Into:

   `addOne = \x -> x + 1`

   This will make it easier to validate your answers.

   a) Rewrite the `f` function in the where clause.

   ```
   addOneIfOdd n = case odd n of
   True -> f n
   False -> n
   where f n = n + 1
   ```

   `f = \n -> n + 1`

   b) Rewrite the following to use anonymous lambda syntax:

   ```
   addFive x y = (if x > y then y else x) + 5
   ```

   `addFive = \x -> \y -> (if x > y then y else x) + 5`

   c) Rewrite the following so that it doesn’t use anonymous
   lambda syntax:

   ```
   mflip f = \x -> \y -> f y x
   ```

   `mflip f x y = f y x`

# Variety Pack

1. Given the following declarations

   ```
   k (x, y) = x
   k1 = k ((4-1), 10)
   k2 = k ("three", (1 + 2))
   k3 = k (3, True)
   ```

   a) What is the type of `k`?

   `k :: (a, b) -> a`

   b) What is the type of `k2`? Is it the same type as `k1` or `k3`?

   `k2 :: [Char]`. And no, `k1, k3 :: Num a => a`.

   c) Of `k1`, `k2`, `k3`, which will return the number `3` as the result?

   `k3`.

2. Fill in the definition of the following function:

   ```
   -- Remember: Tuples have the same syntax for their
   -- type constructors and their data constructors.
   f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
   f = undefined
   ```

   `f (a, b, c) (d, e, f) = ((a, d), (c, f))`

# Case Practice

We’re going to practice using case expressions by rewriting functions. Some of these functions you’ve seen in previous chapters (and some you’ll see later using different syntax yet again!), but you’ll be writing new versions now. Please note these are all written as they would be in source code files, and we recommend you write your answers in source files and then load into GHCi to check, rather than trying to do them directly into the REPL.

First, rewrite `if-then-else` expressions into case expressions.

1. The following should return `x` when `x` is greater than `y`.

   ```
   functionC x y = if (x > y) then x else y
   ```

   ```
   functionC x y = case x > y of
   True  -> x
   False -> y
   ```

2. The following will add `2` to even numbers and otherwise simply
return the input value.

   ```
   ifEvenAdd2 n = if even n then (n + 2) else n
   ```

   ```
   ifEvenAdd2 n = case even n of
                    True  -> n + 2
                    False -> n
   ```

The next exercise doesn’t have all the cases covered. See if you
can fix it.

3. The following compares a value, `x`, to zero and returns an indicator for whether `x` is a postive number or negative number. But what if `x` is `0`? You may need to play with the compare function a bit to find what to do.

   ```
   nums x =
     case compare x 0 of
       LT -> -1
       GT -> 1
   ```

   Fix: Add `EQ -> 0`.

# Artful Dodgy

Given the following definitions tell us what value results from further applications. When you’ve written down at least some of the answers and think you know what’s what, type the definitions into a file and load them in GHCi to test your answers.

```
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2
```

_(Example exercise 1 has been omitted)_

2. `dodgy 1 1`

   `11`

3. `dodgy 2 2`

   `22`

4. `dodgy 1 2`

   `21`

5. `dodgy 2 1`

   `12`

6. `oneIsOne 1`

   `11`

7. `oneIsOne 2`

   `21`

8. `oneIsTwo 1`

   `21`

9. `oneIsTwo 2`

   `22`

10. `oneIsOne 3`

   `31`

11. `oneIsTwo 3`

   `23`

# Guard Duty

1. It is probably clear to you why you wouldn’t put an `otherwise` in your top-most guard, but try it with `avgGrade` anyway and see what happens. It’ll be more clear if you rewrite it as an actual otherwise match: `| otherwise = 'F'`. What happens now if you pass a `90` as an argument? `75`? `60`?

   `avgGrade` will now always return `F`.

2. What happens if you take `avgGrade` as it is written and reorder the guards? Does it still typecheck and work the same? Try moving `| y >= 0.7 = 'C'` and passing it the argument `90`, which should be an ‘A.’ Does it return an ‘A’?

   The function will styll typecheck, but the behaviour might not be the same, as guards are evaluated in order. If you move the suggested guard up, it will match before `B` or `A` get a chance to match. If you move it down, `D`, `E` and/or `F` might get match before `C`.

3. The following function returns

   ``
   pal xs
    | xs == reverse xs = True
    | otherwise = False
   ```

   a) `xs` written backwards when it’s `True`  
   b) `True` when `xs` is a palindrome  
   c) `False` when `xs` is a palindrome  
   d) `False` when `xs` is reversed

   `B`.

4. What types of arguments can pal take?

   `xs :: Eq a => [a]`

5. What is the type of the function pal?

   `pal :: Eq a => [a] -> Bool`

6. The following function returns

   ```
   numbers x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 =1
   ```

   a) the value of its argument plus or minus 1  
   b) the negation of its argument  
   c) an indication of whether its argument is a positive or negative number or zero  
   d) binary machine language

   `C`.

7. What types of arguments can numbers take?

   `x :: (Num a, Ord a) => a`

8. What is the type of the function numbers?

   `number :: (Num a, Ord a, Num b) => a -> b`

# Chapter Exercises

## Multiple choice

1. A polymorphic function

   a) changes things into sheep when invoked  
   b) has multiple arguments  
   c) has a concrete type  
   d) may resolve to values of different types, depending on inputs

   `D`.

2. Two functions named `f` and `g` have types `Char -> String` and `String -> [String]` respectively. The composed function `g . f` has the type

   a) `Char -> String`   
   b) `Char -> [String]`   
   c) `[[String]]`   
   d) `Char -> String -> [String]`

   `B`.

3. A function `f` has the type `Ord a => a -> a -> Bool` and we apply it to one numeric value. What is the type now?

a) `Ord a => a -> Bool`  
b) `Num -> Num -> Bool`  
c) `Ord a => a -> a -> Integer`  
d) `(Ord a, Num a) => a -> Bool`

  `D`.

4. A function with the type `(a -> b) -> c`

   a) requires values of three different types   
   b) is a higher-order function   
   c) must take a tuple as its first argument   
   d) has its parameters in alphabetical order

   `B`.

5. Given the following definition of `f`, what is the type of `f True`?

   ```
   f :: a -> a
   f x = x
   ```

   a) `f True :: Bool`  
   b) `f True :: String`  
   c) `f True :: Bool -> Bool`  
   d) `f True :: a`

   `A`.

## Let’s write code

1. The following function returns the tens digit of an integral argument.

   ```
   tensDigit :: Integral a => a -> a
   tensDigit x = d
      where xLast = x `div` 10
            d = xLast `mod` 10
   ```

   a) First, rewrite it using `divMod`.

   ```
   tensDigit :: Integral a => a -> a
   tensDigit x = d
         where (xLast, _) = x `divMod` 10
               (_, d) = xLast `divMod` 10
   ```

   b) Does the `divMod` version have the same type as the original version?

   Why wouldn't it?

   c) Next, let’s change it so that we’re getting the hundreds digit instead. You could start it like this (though that may not be the only possibility):

   ```
   hunsD x = d2
      where d = undefined ...
   ```

   ```
   hunsDigit :: Integral a => a -> a
   hunsDigit x = d
         where (xLast, _) = x `divMod` 100
               (_, d) = xLast `divMod` 10
   ```

2. Implement the function of the type `a -> a -> Bool -> a` once each using a case expression and once with a guard.

   ```
   foldBool :: a -> a -> Bool -> a
   foldBool = error "Error: Need to implement foldBool!"
   ```

   The result is semantically similar to if-then-else expressions but syntactically quite different.

   ```
   foldBool :: a -> a -> Bool -> a
   foldBool x y b = case b of
                         True -> x
                         False -> y
   ```

   ```
   foldBool :: a -> a -> Bool -> a
   foldBool x y b
    | b = x
    | otherwise = y
   ```

3. Fill in the definition. Note that the first argument to our function is also a function which can be applied to values. Your second argument is a tuple, which can be used for pattern matching:

   ```
   g :: (a -> b) -> (a, c) -> (b, c)
   g f (a, c) = (f a, c)
   ```

_(Example exercise 4 has been omitted)_

5. Next, write a pointfree version of `roundTrip`. (n.b., this refers to the function definition, not to its application in main)

   ```
   roundTrip :: (Show a, Read a) => a -> a
   roundTrip = read . show
   ```

6. Your task now is to change the type of `roundTrip` to `(Show a, Read b) => a -> b`. How might we tell GHC which instance of Read to dispatch against the `String` now? Make the expression `print (roundTrip 4)` work. You will only need the _has the type_ syntax of `::` and parentheses for scoping.

   `print ((roundTrip 4) :: Int)`
