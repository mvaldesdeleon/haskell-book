> module Chapter16 where

Exercises: Be Kind

Given a type signature, determine the kinds of each type variable:

1. What’s the kind of 𝑎?

f :: a -> a

(->) :: * -> *, thus
a :: *

2. What are the kinds of 𝑏 and 𝑇? (The 𝑇 is capitalized on purpose!)

f :: a -> b a -> T (b a)

a, b a :: *, thus
b :: * -> *

b a, T (b a) :: *, thus
T :: * -> *

3. What’s the kind of 𝑐?

f :: c a b -> c b a

c a b :: *, thus
c :: * -> * -> *o


Wait, how does that even typecheck?

(.) :: (b -> c) -> (a -> b) -> a -> c

fmap :: Functor f => (m -> n) -> f m -> f n

fmap :: Functor g => (x -> y) -> g x -> g y

By simple substitution

fmap . fmap
    :: (Functor f, Functor g)
    => (x -> y)
    -> (f m -> f n)

with the additional constraint that
    (g x -> g y) = (m -> n)

So, we can replace this to get:

fmap . fmap
    :: (Functor f, Functor g)
    => (x -> y)
    -> f (g x)
    -> f (g y)

As we would expect, it modifies the value inside two nested functors, preserving both structures.


Exercises: Heavy Lifting

Add fmap, parentheses, and function composition to the expression as needed for the expression to typecheck and produce the expected result.
It may not always need to go in the same place, so don’t get complacent.

1.

> a = fmap (+1) $ read "[1]" :: [Int]

2.

> b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

3.

> c  =      (*2) . (\x -> x - 2)
> c' = fmap (*2)   (\x -> x - 2)

4.

> d =
>   ((return '1' ++) . show) .
>   (\x -> [x, 1..3])
> -- and d' same as above

5.

> e :: IO Integer
> e = let ioi = readIO "1" :: IO Integer
>         changed = fmap (read . ("123"++) . show) ioi
>     in fmap (*3) changed


