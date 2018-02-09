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

# Chapter Exercises

## Multiple choice

1. A value of type `[a]` is

   a) a list of alphabetic characters  
   b) a list of lists  
   c) a list whose elements are all of some type `𝑎`  
   d) a list whose elements are all of different types

   C. In particular, it _could_ be A and B, but it does not _have_ to.

2. A function of type `[[a]] -> [a]` could

   a) take a list of strings as an argument  
   b) transform a character into a string  
   c) transform a string in to a list of strings  
   d) take two arguments

   A.

3. A function of type `[a] -> Int -> a`

   a) takes one argument  
   b) returns one element of type `𝑎` from a list  
   c) must return an `Int` value  
   d) is completely fictional

   B.

4. A function of type `(a, b) -> a`

   a) takes a list argument and returns a `Char` value  
   b) has zero arguments  
   c) takes a tuple argument and returns the first value  
   d) requires that `𝑎` and `𝑏` be of different types

   C.

## Determine the type

For the following functions, determine the type of the specified value. We suggest you type them into a file and load the contents of the file in GHCi. In all likelihood, it initially will not have the polymorphic types you might expect due to the monomorphism restriction. We’ll explain more much later, but for now it means that top-level declarations by default will have a concrete type if any can be determined.

1. All function applications return a value. Determine the value returned by these function applications and the type of that value.

  a) `(* 9) 6`

  `54 :: Num a => a`

  b) `head [(0, "doge"), (1, "kitteh")]`

  `(0, "doge") :: Num a => (a, [Char])`

  c) `head [(0 :: Integer , "doge"), (1, "kitteh")]`

  `(0, "doge") :: (Integer, [Char])`

  d) `if False then True else False`

  `False :: Bool`

  e) `length [1, 2, 3, 4, 5]`

  `5 :: Int`

  f) `(length [1, 2, 3, 4]) > (length "TACOCAT")`

  `False :: Bool`

2. Given

   ```
   x = 5
   y = x + 5
   w = y * 10
   ```

   What is the type of `w`?

   `Num a => a`

3. Given

   ```
   x = 5
   y = x + 5
   z y = y * 10
   ```

   What is the type of `z`?

   `Num a => a -> a`

4. Given

   ```
   x = 5
   y = x + 5
   f = 4 / y
   ```

   What is the type of `f`?

   `Fractional a => a`

5. Given

   ```
   x = "Julie"
   y = " <3 "
   z = "Haskell"
   f = x ++ y ++ z
   ```
   
   What is the type of `f`?

   `[Char]`

## Does it compile?

For each set of expressions, figure out which expression, if any, causes the compiler to squawk at you (n.b. we do not mean literal squawking) and why. Fix it if you can.

1. 

   ```
   bigNum = (^) 5 $ 10
   wahoo = bigNum $ 10
   ```

   Can't apply `10` to `(5^10)` on line 2. Fix:

   ```
   bigNum = (^) 5
   wahoo = bigNum $ 10
   ```

2. 

   ```
   x = print
   y = print "woohoo!"
   z = x "hello world"
   ```

   Compiles. When `z` is evaluated it will print `"hello world"`.

3. 

   ```
   a = (+)
   b = 5
   c = b 10
   d = c 200
   ```

   Can't apply `10` to `5` on line 3. Fix:

   ```
   a = (+)
   b = a
   c = b 10
   d = c 200
   ```

4. 
   
   ```
   a = 12 + b
   b = 10000 * c
   ```

   `c` is not in scope. Fix (for GHCi you must also reverse the input order):

   ```
   a = 12 + b
   b = 10000 * c
   c = 1
   ```

## Type variable or specific type constructor?

You will be shown a type declaration, and you should categorize each type. The choices are a fully polymorphic type variable, constrained polymorphic type variable, or concrete type constructor.

_(Example exercise 1 has been omitted)_

2. Categorize each component of the type signature as described in the previous example.

   `f :: zed -> Zed -> Blah`

   `zed`: Fully polymorphic type variable  
   `Zed`: Concrete type constructor.  
   `Blah`: Concrete type constructor.

3. Categorize each component of the type signature

   `f :: Enum b => a -> b -> C`

   `a`: Fully polymorphic type variable  
   `b`: Constraiend polymorphic type.  
   `C`: Concrete type constructor.

4. Categorize each component of the type signature

   `f :: f -> g -> C`

   `f`: Fully polymorphic type variable  
   `g`: Fully polymorphic type variable.  
   `C`: Concrete type constructor.

## Write a type signature

For the following expressions, please add a type signature. You should be able to rely on GHCi type inference to check your work, although you might not have precisely the same answer as GHCi gives (due to polymorphism, etc).

1. While we haven’t fully explained this syntax yet, you’ve seen it in Chapter 2 and as a solution to an exercise in Chapter 4. This syntax is a way of destructuring a single element of a list.

   ```
   functionH ::
   functionH (x:_) = x
   ```

   `functionH :: [a] -> a`

2. 
   ```
   functionC ::
   functionC x y = if (x > y) then True else False
   ```

   `functionC :: Ord a => a -> a -> Bool`

3. 
   ```
   functionS ::
   functionS (x, y) = y
   ```

   `functionS :: (a, b) -> b`

## Given a type, write the function

You will be shown a type and a function that needs to be written. Use the information the type provides to determine what the function should do. We’ll also tell you how many ways there are to write the function. Syntactically different but semantically equivalent implementations are not counted as being different. For example, writing a function one way then rewriting the semantically identical function but using anonymous lambda syntax does not count as two implementations.

1. There is only one function definition that typechecks and doesn’t go into an infinite loop when you run it.

    ```
    i :: a -> a
    i = undefined
    ```

    `i a = a`

2. There is only one version that works.

    ```
    c :: a -> b -> a
    c = undefined
    ```

    `c a b = a`

3. Given alpha equivalence are `c''` and `c` (see above) the same thing?

    ```
    c'' :: b -> a -> b
    c'' = ?
    ```

    Yes. `c'' b a = b`.

4. Only one version that works.

    ```
    c' :: a -> b -> b
    c' = undefined
    ```

    `c' a b = b`

5. There are multiple possibilities, at least two of which you’ve seen in previous chapters.

    ```
    r :: [a] -> [a]
    r = undefined
    ```

    `r = tail`
    `r = reverse`

6. Only one version that will typecheck.

    ```
    co :: (b -> c) -> (a -> b) -> a -> c
    co = undefined
    ```

    `co bc ab a = bc $ ab $ a`

7. One version will typecheck.

    ```
    a :: (a -> c) -> a -> a
    a = undefined
    ```

    `a ac a = a`

8. One version will typecheck.

    ```
    a' :: (a -> b) -> a -> b
    a' = undefined
    ```

    `a' ab a = ab a`

## Fix it

Won’t someone take pity on this poor broken code and fix it up? Be sure to check carefully for things like capitalization, parentheses, and indentation.

See [sing.hs](https://github.com/mvaldesdeleon/haskell-book/blob/master/ch05/sing.hs) and [arith3broken.hs.hs](https://github.com/mvaldesdeleon/haskell-book/blob/master/ch05/arith3broken.hs.hs)]

## Type-Kwon-Do

1. 

   ```
   f :: Int -> String
   f = undefined

   g :: String -> Char
   g = undefined

   h :: Int -> Char
   h = ???
   ```

   `h i = g $ f i`

2.  

   ```
   data A
   data B
   data C
   
   q :: A -> B
   q = undefined
   
   w :: B -> C
   w = undefined
   
   e :: A -> C
   e = ???
   ```

   `e a = w $ q a`

3. 

   ```
   data X
   data Y
   data Z
   
   xz :: X -> Z
   xz = undefined
   
   yz :: Y -> Z
   yz = undefined
   
   xform :: (X, Y) -> (Z, Z)
   xform = ???
   ```

   `xform (x, y) = (xz x, yz y)`

4. 

   ```
   munge :: (x -> y) -> (y -> (w, z)) -> x -> w
   munge = ???
   ```

   `munge xy ywz x = fst $ ywz $ xy x`