# Exercise

Write out the evaluation of the following. It might be a little less noisy if you do so with the form that didn’t use `(.)`.

`applyTimes 5 (+1) 5`

```
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)
```

```
applyTimes 5 (+1) 5

(+1) (applyTimes 4 (+1) 5)

(+1) ((+1) (applyTimes 3 (+1) 5))

(+1) ((+1) ((+1) (applyTimes 2 (+1) 5)))

(+1) ((+1) ((+1) ((+1) (applyTimes 1 (+1) 5))))

(+1) ((+1) ((+1) ((+1) ((+1) (applyTimes 0 (+1) 5)))))

(+1) ((+1) ((+1) ((+1) ((+1) 5))))

(+1) ((+1) ((+1) ((+1) 6)))

(+1) ((+1) ((+1) 7))

(+1) ((+1) 8)

(+1) 9

10
```

# Chapter Exercises

## Review of types

1. What is the type of `[[True, False], [True, True], [False, True]]`?

   a) `Bool`  
   b) mostly `True`  
   c) `[a]`  
   d) `[[Bool]]`

   `D`.

2. Which of the following has the same type as `[[True, False], [True, True], [False, True]]`?

   a) `[(True, False), (True, True), (False, True)]`  
   b) `[[3 == 3], [6 > 5], [3 < 4]]`  
   c) `[3 == 3, 6 > 5, 3 < 4]`  
   d) `["Bool", "more Bool", "Booly Bool!"]`

   `B`.

3. For the following function

   ```
   func :: [a] -> [a] -> [a]
   func x y = x ++ y
   ```

   which of the following is true?

   a) `x` and `y` must be of the same type  
   b) `x` and `y` must both be lists  
   c) if `x` is a String then `y` must be a String  
   d) all of the above

   `D`.

4. For the `func` code above, which is a valid application of `func` to both of its arguments?

   a) `func "Hello World"`
   b) `func "Hello" "World"`  
   c) `func [1, 2, 3] "a, b, c"`  
   d) `func ["Hello", "World"]`

   `B`.

## Reviewing currying

Given the following definitions, tell us what value results from further applications.

```
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"
```

`flippy :: String -> String -> String`
`appedCatty :: String -> String`
`frappe :: String -> String`

1. What is the value of `appedCatty "woohoo!"` ? Try to determine the
answer for yourself, then test in the REPL.

`"woops mrow woohoo!"`

2. `frappe "1"`

`"1 mrow haha"`

3. `frappe (appedCatty "2")`

`"woops mrow 2 mrow haha"`

4. `appedCatty (frappe "blue")`

`"woops mrow blue mrow haha"`

5. `cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))`

`"pink mrow haha mrow green mrow woops mrow blue"`

6. `cattyConny (flippy "Pugs" "are") "awesome"`

`"are mrow Pugs mrow awesome"`

## Recursion

1. Write out the steps for reducing `dividedBy 15 2` to its final answer according to the Haskell code.

`dividedBy 15 2`

`go 15 2 0`

`go 13 2 1`

`go 11 2 2`

`go 9 2 3`

`go 7 2 4`

`go 5 2 5`

`go 3 2 6`

`go 1 2 7`

`(7, 1)`

2. Write a function that recursively sums all numbers from 1 to n, n being the argument. So that if n was 5, you’d add 1 + 2 + 3 + 4 + 5 to get 15. The type should be `(Eq a, Num a) => a -> a`.

```
sum a = go a 0
    where go n count
           | n == 0 = count
           | otherwise = go (n - 1) (count + n)
```

3. Write a function that multiplies two integral numbers using recursive summation. The type should be `(Integral a) => a -> a -> a`.

```
mult a b = go a b 0
    where go a b count
            | a == 0 = count
            | otherwise = go (a - 1) b (count + b)
```

## Fixing dividedBy

Our dividedBy function wasn’t quite ideal. For one thing. It was a partial function and doesn’t return a result (bottom) when given a divisor that is 0 or less.

Using the pre-existing div function we can see how negative numbers should be handled.

The next issue is how to handle zero. Zero is undefined for division in math, so really we ought to use a datatype that lets us say there was no sensible result when the user divides by zero.

```
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
           | n < d = (count, n)
           | otherwise = go (n - d) d (count + 1)
```

```
data DividedResult =
    Result Integer
    | DividedByZero
    deriving Show

dividedBy' :: Integral a => a -> a -> DividedResult
dividedBy' num denom = go num denom 0
    where go n d count
           | d == 0 = DividedByZero
           | d < 0 = case dividedBy' n (-d) of
                DividedByZero -> DividedByZero
                Result r -> Result (-r)
           | n < 0 = case dividedBy' (-n) d of
                DividedByZero -> DividedByZero
                Result r -> Result (-r)
           | n < d = Result count
           | otherwise = go (n - d) d (count + 1)
```

## McCarthy 91 function

We’re going to describe a function in English, then in math notation, then show you what your function should return for some test inputs. Your task is to write the function in Haskell.

The McCarthy 91 function yields 𝑥 − 10 when 𝑥 > 100 and 91 otherwise. The function is recursive.

```
mc91 n
    | n > 100 = n - 10
    | otherwise = mc91 (mc91 (n + 11))
```

## Numbers into words

See [wordnumber.hs](https://github.com/mvaldesdeleon/haskell-book/blob/master/ch08/wordnumber.hs)]
