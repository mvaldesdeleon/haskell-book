# Scope

1. These lines of code are from a REPL session. Is _𝑦_ in scope for _𝑧_?

   ```
   Prelude> let x = 5
   Prelude> let y = 7
   Prelude> let z = x * y
   ```

   Yes. Both `x` and `y` are in scope by the time `z` is declared.

2. These lines of code are from a REPL session. Is _h_ in scope for function _𝑔_? Go with your gut here.

   ```
   Prelude> let f = 3
   Prelude> let g = 6 * f + h
   ```

   Based solely on the code displayed, no.

3. This code sample is from a source file. Is everything we need to execute _area_ in scope?

   ```
   area d = pi * (r * r)
   r = d / 2
   ```

   No. When declaring `r`, `d` is not in scope.

4. This code is also from a source file. Now are _𝑟_ and _𝑑_ in scope for _area_?

   ```
   area d = pi * (r * r)
      where r = d / 2
   ```

   Yes. Now `d` is in scope when declaring `r`, and `r` is in scope when declaring `area`.

# Syntax Errors

Read the syntax of the following functions and decide whether it will compile. Test them in your REPL and try to fix the syntax errors where they occur.

1. `++ [1, 2, 3] [4, 5, 6]`

   Syntax error, parenthesis are required when using the `++` operator in prefix position. Fix:

   `(++) [1, 2, 3] [4, 5, 6]`

2. `'<3' ++ ' Haskell'`

   Syntax error, single quotes used instead of double quotes. Fix:

   `"<3" ++ " Haskell"`

3. `concat ["<3", " Haskell"]`
   
   No syntax errors, evaluates to `"<3 Haskell"`.

# Chapter Exercises

## Reading syntax

1. For the following lines of code, read the syntax carefully and decide if they are written correctly. Test them in your REPL after you’ve decided to check your work. Correct as many as you can.

   a) `concat [[1, 2, 3], [4, 5, 6]]`

   `[1, 2, 3, 4, 5, 6]`

   b) `++ [1, 2, 3] [4, 5, 6]`

   Syntax error, missing parentheses. Fix: 

   `(++) [1, 2, 3] [4, 5, 6]`

   c) `(++) "hello" " world"`

   `"hello world"`

   d) `["hello" ++ " world]`

   Syntax error, missing double-quotes. Fix:

   `["hello" ++ " world"]`

   e) `4 !! "hello"`

   Type error, the operands are reversed. Fix:

   `"hello" !! 4`

   f) `(!!) "hello" 4` 

   `'o'`

   g) `take "4 lovely"`

   Type error, a string is provided instead of a number. Fix:

   `take 4 "lovely"`

   h) `take 3 "awesome"`

   `"awe"`

2. Next we have two sets: the first set is lines of code and the other is a set of results. Read the code and figure out which results came from which lines of code. Be sure to test them in the REPL.

   a) `concat [[1 * 6], [2 * 6], [3 * 6]]`

   b) `"rain" ++ drop 2 "elbow"`

   c) `10 * head [1, 2, 3]`

   d) `(take 3 "Julie") ++ (tail "yes")`

   e) `concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]]`

   Can you match each of the previous expressions to one of these results presented in a scrambled order?

   a) `"Jules"`

   `(take 3 "Julie") ++ (tail "yes")`

   b) `[2,3,5,6,8,9]`

   `concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]]`

   c) `"rainbow"`

   `"rain" ++ drop 2 "elbow"`

   d) `[6,12,18]`

   `concat [[1 * 6], [2 * 6], [3 * 6]]`

   e) `10`

   `10 * head [1, 2, 3]`

## Building functions

1. Given the list-manipulation functions mentioned in this chap- ter, write functions that take the following inputs and return the expected outputs. Do them directly in your REPL and use the take and drop functions you’ve already seen.

   a) 
      ```
      -- Given
      "Curry is awesome"
      -- Return
      "Curry is awesome!"
      ```

      `"Curry is awesome" ++ "!"`

   b) 
      ```
      -- Given
      "Curry is awesome!"
      -- Return
      "y"
      ```

      `"Curry is awesome!" !! 4`

   c) 
      ```
      -- Given
      "Curry is awesome!"
      -- Return
      "awesome!"
      ```

      `drop 9 "Curry is awesome!"`

For remaining exercises, see [exercises.hs](https://github.com/mvaldesdeleon/haskell-book/blob/master/ch03/exercises.hs)