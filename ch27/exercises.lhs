> {-# LANGUAGE Strict, BangPatterns #-}
>
> module Chapter27 where

Exercises: Evaluate

Expand the expression in as much detail as possible. Then, work outside-in to see what the expression evaluates to.

1. const 1 undefined

2. const undefined 1

3. flip const undefined 1

4. flip const 1 undefined

5. const undefined undefined

6. foldr const 'z' ['a'..'e']

7. foldr (flip const) 'z' ['a'..'e']


Strict List

Try messing around with the following list type and compare what it does with the bang-patterned list variants we experimented with earlier:

> data List a = Nil | Cons a (List a)
>   deriving (Show)
>
> take' n _ | n <= 0 = Nil
> take' _ Nil = Nil
> take' n (Cons x xs) = (Cons x (take' (n-1) xs))
>
> map' _ Nil = Nil
> map' f (Cons x xs) = (Cons (f x) (map' f xs))
>
> repeat' x = xs where xs = (Cons x xs)
>
> main = do
>   print $ take' 10 $ map' (+1) (repeat' 1)

What will :sprint output?

We show you a definition or multiple definitions, you determine what :sprint will output when passed the bindings listed in your head before testing it.

1. let x = 1

x = _

2. let x = ['1']

x = "1"

3. let x = [1]

x = _

4. let x = 1 :: Int

x = 1

5. let f = \x -> x
   let x = f 1

x = _

6. let f :: Int -> Int; f = \x -> x
   let x = f 1

x = _

Will get shared once evaluated.


Will printing this expression result in bottom?

1. snd (undefined, 1)

No

2. let x = undefined
   let y = x `seq` 1 in snd (x, y)

Yes

3. length $ [1..5] ++ undefined

Yes

4. length $ [1..5] ++ [undefined]

No

5. const 1 undefined

No

6. const 1 (undefined `seq` 1)

No

7. const undefined 1

Yes


Make the expression bottom

Using only bang patterns or seq, make the code bottom out when executed.

1.

> x = undefined
> y = x `seq` "blah"
>
> boom = do
>   print (snd (x, y))
