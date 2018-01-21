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
