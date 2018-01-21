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
