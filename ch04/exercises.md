# Mood Swing

Given the following datatype, answer the following questions:

`data Mood = Blah | Woot deriving Show`

1. What is the type constructor, or name of this type?

   `Mood`.

2. If the function requires a `Mood` value, what are the values you could possibly use there?

   `Blah` or `Woot`.

3. We are trying to write a function `changeMood` to change Chris’s mood instantaneously. It should act like not in that, given one value, it returns the other value of the same type. So far, we’ve written a type signature `changeMood :: Mood -> Woot`. What’s wrong with that?

   `Woot` is a data constructor, so it cannot be used in a type signature. Use `Mood` instead.

4. Now we want to write the function that changes his mood. Given an input mood, it gives us the other one. Fix any mistakes and complete the function:

   ```
   changeMood Mood = Woot
   changeMood    _ = Blah
   ```

   `Mood` is a type constructor, and cannot be used at the code level. Use `Blah` instead.

5. Enter all of the above — datatype (including the deriving Show bit), your corrected type signature, and the corrected function into a source file. Load it and run it in GHCi to make sure you got it right.

   See [moodswing.hs](https://github.com/mvaldesdeleon/haskell-book/blob/master/ch04/moodswing.hs)

# Find the Mistakes

The following lines of code may have mistakes — some of them won’t compile! You know what you need to do.

1. `not True && true`
   
   Error, `true` is not in scope. Use `True` instead.

2. `not (x = 6)`

   Parse error on `=`, use `==` instead. `x` should be in scope.

3. `(1 * 2) > 5`

   `False`

4. `[Merry] > [Happy]`

   Error, `Merry` and `Happy` are not in scope. Use `"Merry"` and `"Happy"` instead, respectively.

5. `[1, 2, 3] ++ "look at me!"`

   Error, cannot concatenate lists containing elements of different types. Fixing this would require either dropping the concatenation along with an operand, or replacing an operand to match the type of the remaining one.

# Chapter Exercises

## Unnamed Section

As in previous chapters, you will gain more by working out the answer before you check what GHCi tells you, but be sure to use your REPL to check your answers to the following exercises. Also, you will need to have the `awesome`, `alsoAwesome`, and `allAwesome` code from above in scope for this REPL session. For convenience of reference, here are those values again:

```
awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]
```

`length` is a function that takes a list and returns a result that tells how many items are in the list.

1. Given the definition of `length` above, what would the type signature be? How many arguments, of what type does it take? What is the type of the result it evaluates to?

   `length :: [a] -> Integer`

2. What are the results of the following expressions?

   a) `length [1, 2, 3, 4, 5]`

   `5`

   b) `length [(1, 2), (2, 3), (3, 4)]`

   `4`

   c) `length allAwesome`

   `2`

   d) `length (concat allAwesome)`

   `5`

3. Given what we know about numeric types and the type signature of `length`, look at these two expressions. One works and one returns an error. Determine which will return an error and why.

   ```
   Prelude> 6 / 3
   -- and
   Prelude> 6 / length [1, 2, 3]
   ```

   The first one will work and the second one will fail. The use of `/` requires a `Fractional` instance, but `length` returns `Integer`, which does not have such an instance. The same error can be forced by explicitly specifying a type for `3` in the first expression: `6 / (3 :: Integer)`.

4. How can you fix the broken code from the preceding exercise using a different division function/operator?

   By replacing `/` with either `div` or `quot`.

5. What is the type of the expression `2 + 3 == 5`? What would we expect as a result?

   `True :: Bool`.

6. What is the type and expected result value of the following:

   ```
   Prelude> let x = 5
   Prelude> x + 3 == 5
   ```

   `False :: Bool`.

7. Below are some bits of code. Which will work? Why or why not? If they will work, what value would these reduce to?

   `Prelude> length allAwesome == 2`

   `True`

   `Prelude> length [1, 'a', 3, 'b']`

   Error, cannot create a list with values of mixed types.

   `Prelude> length allAwesome + length awesome`

   `5`

   `Prelude> (8 == 8) && ('b' < 'a')`

   `False`

   `Prelude> (8 == 8) && 9`

   Error, `&&` expects two `Bool`, not `9`.

8. Write a function that tells you whether or not a given String (or list) is a palindrome. Here you’ll want to use a function called `reverse`, a predefined function that does just what it sounds like.

   ```
   reverse :: [a] -> [a]
   reverse "blah"
   "halb"
   ```

   ```
   isPalindrome :: (Eq a) => [a] -> Bool
   isPalindrome x = reverse x == x
   ```

9. Write a function to return the absolute value of a number using if-then-else

   ```
   myAbs :: Integer -> Integer
   myAbs x = if x > 0 then x else negate x
   ```

10. Fill in the definition of the following function, using `fst` and `snd`:

    ```
    f :: (a, b) -> (c, d) -> ((b, d), (a, c))
    f l r = ((snd l, snd r), (fst l, fst r))
    ```

## Correcting syntax

In the following examples, you’ll be shown syntactically incorrect code. Type it in and try to correct it in your text editor, validating it with GHC or GHCi.

1. Here, we want a function that adds 1 to the length of a string argument and returns that result.

   ```
   x = (+)

   F xs = w 'x' 1
      where w = length xs
   ```

   ```
   x = (+)

   f xs = w `x` 1
      where w = length xs
   ```

2. This is supposed to be the identity function, `id`.

   ```
   \X=x
   ```

   ```
   \x -> x
   ```

3. When fixed, this function will return 1 from the value [1, 2, 3].

   ```
   \x:xs -> x
   ```

   ```
   \(x:xs) -> x
   ```

4. When fixed, this function will return 1 from the value (1, 2).

   ```
   f (a b) = A
   ```

   ```
   f (a, b) = a
   ```

## Match the function names to their types

1. Which of the following types is the type of `show`?

   a) `show a => a -> String`  
   b) `Show a -> a -> String`  
   c) `Show a => a -> String`

   `Show a => a -> String`

2. Which of the following types is the type of `(==)`?

   a) `a -> a -> Bool`  
   b) `Eq a => a -> a -> Bool`  
   c) `Eq a -> a -> a -> Bool`  
   d) `Eq a => A -> Bool`

   `Eq a => a -> a -> Bool`

3. Which of the following types is the type of `fst`?
  
   a) `(a, b) -> a`  
   b) `b -> a`  
   c) `(a, b) -> b`

   `(a, b) -> a`

4. Which of the following types is the type of `(+)`?
 
   a) `(+) :: Num a -> a -> a -> Bool`  
   b) `(+) :: Num a => a -> a -> Bool`  
   c) `(+) :: num a => a -> a -> a`  
   d) `(+) :: Num a => a -> a -> a`  
   e) `(+) :: a -> a -> a`

   `(+) :: Num a => a -> a -> a`