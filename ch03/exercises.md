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