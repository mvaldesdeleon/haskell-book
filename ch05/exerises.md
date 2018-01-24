# Type Matching

Below you’ll find a list of several standard functions we’ve talked about previously. Under that is a list of their type signatures. Match the function to its type signature. Try to do it without peeking at the type signatures (either in the text or in GHCi) and then check your work. You may find it easier to start from the types and work out what you think a function of that type would do.

1. Functions:

   a) `not`

   b) `length`

   c) `concat`

   d) `head`
   
   e) `(<)`
   
2. Type signatures:

   a) `_ :: [a] -> a`

   `head`

   b) `_ :: [[a]] -> [a]`

   `concat`

   c) `_ :: Bool -> Bool`

   `not`

   d) `_ :: [a] -> Int`

   `length`

   e) `_ :: Ord a => a -> a -> Bool`

   `(<)`

# Type Arguments

Given a function and its type, tell us what type results from applying some or all of the arguments.

1. If the type of `f` is `a -> a -> a -> a`, and the type of `𝑥` is `Char` then the type of `f x` is:

   a) `Char -> Char -> Char`
   b) `x -> x -> x -> x`
   c) `a -> a -> a`
   d) `a -> a -> a -> Char`

   `Char -> Char -> Char`

2. If the type of `g` is `a -> b -> c -> b`, then the type of `g 0 'c' "woot"` is:

   a) `String`
   b) `Char -> String`
   c) `Int`
   d) `Char`

   `Char`

3. If the type of `h` is `(Num a, Num b) => a -> b -> b`, then the type of `h 1.0 2` is:

   a) `Double`
   b) `Integer`
   c) `Integral b => b`
   d) `Num b => b`

   `Num b => b`

   Note that because the type variables `𝑎` and `𝑏` are different, the compiler _must_ assume that the types could be different.

4. If the type of `h` is `(Num a, Num b) => a -> b -> b`, then the type of `h 1 (5.5 :: Double)` is:

   a) `Integer`
   b) `Fractional b => b`
   c) `Double`
   d) `Num b => b`

   `Double`

5. If the type of `jackal` is `(Ord a, Eq b) => a -> b -> a`, then the type of
     `jackal "keyboard" "has the word jackal in it"` is:

   a) `[Char]`
   b) `Eq b => b`
   c) `b -> [Char]`
   d) `b`
   e) `Eq b => b -> [Char]`

   `[Char]`

6. If the type of `jackal` is `(Ord a, Eq b) => a -> b -> a`, then the type of
     `jackal "keyboard"` is:

   a) `b`
   b) `Eq b => b`
   c) `[Char]`
   d) `b -> [Char]`
   e) `Eq b => b -> [Char]`

   `Eq b => b -> [Char]`

7. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of
`kessel 1 2` is:

   a) `Integer`
   b) `Int`
   c) `a`
   d) `(Num a, Ord a) => a`
   e) `Ord a => a`
   f) `Num a => a`

   `(Num a, Ord a) => a`

8. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of
`kessel 1 (2 :: Integer)` is:

   a) `(Num a, Ord a) => a`
   b) `Int`
   c) `a`
   d) `Num a => a`
   e) `Ord a => a`
   f) `Integer`

   `(Num a, Ord a) => a`

9. If the type of `kessel` is (`Ord a, Num b) => a -> b -> a`, then the type of
`kessel (1 :: Integer) 2` is:

   a) `Num a => a`
   b) `Ord a => a`
   c) `Integer`
   d) `(Num a, Ord a) => a`
   e) `a`

   `Integer`

# Parametricity

All you can really do with a parametrically polymorphic value is pass or not pass it to some other expression. Prove that to yourself with these small demonstrations.

1. Given the type `a -> a`, which is the type for `id`, attempt to make a function that is not bottom and terminates successfully that does something other than returning the same value. This is impossible, but you should try it anyway.

   Welp.

2. We can get a more comfortable appreciation of parametricity by looking at `a -> a -> a`. This hypothetical function `a -> a -> a` has two–and only two–implementations. Write both possible versions of `a -> a -> a`. After doing so, try to violate the constraints of parametrically polymorphic values we outlined above.

   ```
   f :: a -> a -> a
   f x y = x
   
   g :: a -> a -> a
   g x y = y
   ```

3. Implement `a -> b -> b`. How many implementations can it have? Does the behavior change when the types of `𝑎` and `𝑏` change?

   ```
   f :: a -> b -> b
   f x y = y
   ```

   This is the only implementation, and the behaviour does not change when the types of `a` and `b` change.

# Apply Yourself

Look at these pairs of functions. One function is unapplied, so the compiler will infer maximally polymorphic type. The second function has been applied to a value, so the inferred type signature may have become concrete, or at least less polymorphic. Figure out how the type would change and why, make a note of what you think the new inferred type would be and then check your work in GHCi.

1. 
   ```
   -- Type signature of general function
   (++) :: [a] -> [a] -> [a]

   -- How might that change when we apply
   -- it to the following value?
   myConcat x = x ++ " yo"
   ```

   `(++) :: [Char] -> [Char] -> [Char]`

2. 
   ```
   -- General function
   (*) :: Num a => a -> a -> a

   -- Applied to a value
   myMult x = (x / 3) * 5
   ```

   `(*) :: Fractional a => a -> a -> a`

3. 
   ```
   take :: Int -> [a] -> [a]

   myTake x = take x "hey you"
   ```

   `take :: Int -> [Char] -> [Char]`

4. 
   ```
   (>) :: Ord a => a -> a -> Bool

   myCom x = x > (length [1..10])
   ```

   `(>) :: Int -> Int -> Bool`

5. 
   ```
   (<) :: Ord a => a -> a -> Bool

   myAlph x = x < 'z'
   ```

   `(<) :: Char -> Char -> Bool`
