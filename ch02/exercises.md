# Comprehension Check

1. Given the following lines of code as they might appear in a source file, how would you change them to use them directly in the REPL?

   **half x = x / 2**

   `let half x = x / 2`

   **square x = x * x**

   `let square x = x * x`

2. Write one function that can accept one argument and work for all the following expressions. Be sure to name the function.

   **3.14 * (5 * 5)  
   3.14 * (10 * 10)  
   3.14 * (2 * 2)  
   3.14 * (4 * 4)**

   `areaCircle r = 3.14 * square r`


3. There is a value in Prelude called `pi`. Rewrite your function to use `pi` instead of 3.14.

   `areaCircle' r = pi * square r`

# Parentheses and Association

Below are some pairs of functions that are alike except for parenthe- sization. Read them carefully and decide if the parentheses change the results of the function. Check your work in GHCi.

1. **a) 8 + 7 * 9  
   b) (8 + 7) * 9**

   The parentheses change the results because (*) has higher precedence than (+).

2. **a) perimeter x y = (x * 2) + (y * 2)  
   b) perimeter x y = x * 2 + y * 2**

   For the same reasons as before, the parentheses do not change the results.

3. **a) f x = x / 2 + 9  
   b) f x = x / (2 + 9)**

   The parentheses change the results because (/) has higher precedence than (+).

# Heal the Sick

The following code samples are broken and won’t compile. The first two are as you might enter into the REPL; the third is from a source file. Find the mistakes and fix them so that they will.

1. `let area x = 3. 14 * (x * x)`

   `let area x = 3.14 * (x * x)`

2. `let double x = b * 2`

   `let double x = x * 2`

3. ```
   x = 7
    y = 10
   f = x + y
   ```

   ```
   x = 7
   y = 10
   f = x + y
   ```

# A Head Code

Now for some exercises. First, determine in your head what the following expressions will return, then validate in the REPL:

1. `let x = 5 in x`
   
   `5`

2. `let x = 5 in x * x`
   
   `25`

3. `let x = 5; y = 6 in x * y`
   
   `30`

4. `let x = 3; y = 1000 in x + 3`
   
   `6`

_Rewrite with where clauses_, see [where.hs](https://github.com/mvaldesdeleon/haskell-book/blob/master/ch02/where.hs)

# Chapter Exercises

## Parenthesization

Given what we know about the precedence of (*), (+), and (^), how can we parenthesize the following expressions more explicitly with- out changing their results? Put together an answer you think is correct, then test in the GHCi REPL.

1. `2 + 2 * 3 - 1`

   `2 + (2 * 3) - 1`

2. `(^) 10 $ 1 + 1`

   `(10^) $ (1 + 1)`

3. `2 ^ 2 * 4 ^ 5 + 1`
   
   `(2 ^ 2) * (4 ^ 5) + 1`

## Equivalent expressions

Which of the following pairs of expressions will return the same result when evaluated? Try to reason them out in your head by reading the code and then enter them into the REPL to check your work:

1. `1 + 1`
   `2`

   `1 + 1` will evaluate to `2`.

2. `10 ^ 2`
   `10 + 9 * 10`

   Both expressions will evaluate to `100`.

3. `400 - 37`
   `(-) 37 400`

   The first expression will evaluate to `363`, whereas the second one will evaluate to `-363`

4. `100 'div' 3`
   `100 / 3`

   The first expression will perform an integer division, whereas the second one will perform a fractional division. I expect them to evaluate to `33` and `33.3...` respectively.

5. `2 * 5 + 18`
   `2 * (5 + 18)`

   In the first expression, the multiplication will be evaluated first. In the second, it will be the addition. The results will not be the same.

## More fun with functions

Here is a bit of code as it might be entered into a source file. Remem- ber that when you write code in a source file, the order is unimpor- tant, but when writing code directly into the REPL the order does matter. Given that, look at this code and rewrite it such that it could be evaluated in the REPL (remember: you’ll need let when entering it directly into the REPL). Be sure to enter your code into the REPL to make sure it evaluates correctly.

```
let z = 7

let y = z + 8 -- 15

let x = y ^ 2 -- 225

let waxOn = x * 5 -- 1125
```

1. Now you have a value called waxOn in your REPL. What do you
think will happen if you enter:

   In all cases, the expression will be evaluated by applying the functions to their arguments.

   `10 + waxOn`

   `1135`

   `(+10) waxOn`

   `1135`

   `(-) 15 waxOn`

   `-1110`

   `(-) waxOn 15`

   `1110`

2. Earlier we looked at a function called triple. While your REPL has waxOn in session, re-enter the triple function at the prompt:

   `let triple x = x * 3`

3. Now, what will happen if we enter this at our GHCi prompt. Try to reason out what you think will happen first, considering what role waxOn is playing in this function call. Then enter it, see what does happen, and check your understanding:

   `triple waxOn`

   `waxOn` is just an integer. `triple` will be applied to it, producing another integer as the result:

   `3375`

For remaining exercises, see [where.hs](https://github.com/mvaldesdeleon/haskell-book/blob/master/ch02/where.hs)