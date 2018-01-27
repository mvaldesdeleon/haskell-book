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

Example not included.

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
