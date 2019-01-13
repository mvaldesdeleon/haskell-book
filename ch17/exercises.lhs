> module Chapter17 where
>
> import Data.List (elemIndex)
> import Control.Applicative (liftA3)

Exercises: Lookups

In the following exercises you will need to use the following terms to make the expressions typecheck:

1. pure
2. (<$>)
3. (<*>)

Make the following expressions typecheck.

1.

> added :: Maybe Integer
> added =
>   pure (+3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

2.

> y :: Maybe Integer
> y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]
>
> z :: Maybe Integer
> z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]
>
> tupled :: Maybe (Integer, Integer)
> tupled = (,) <$> y <*> z

3.

> x' :: Maybe Int
> x' = elemIndex 3 [1, 2, 3, 4, 5]
>
> y' :: Maybe Int
> y' = elemIndex 4 [1, 2, 3, 4, 5]
>
> max' :: Int -> Int -> Int
> max' = max
>
> maxed :: Maybe Int
> maxed = max' <$> x' <*> y'

4.

> xs = [1, 2, 3]
> ys = [4, 5, 6]
>
> x'' :: Maybe Integer
> x'' = lookup 3 $ zip xs ys
>
> y'' :: Maybe Integer
> y'' = lookup 2 $ zip xs ys
>
> summed :: Maybe Integer
> summed = fmap sum $ (,) <$> x'' <*> y''


Exercise: Identity Instance

Write an Applicative instance for Identity.

> newtype Identity a = Identity a
>   deriving (Eq, Ord, Show)
>
> instance Functor Identity where
>   fmap f (Identity a) = Identity (f a)
>
> instance Applicative Identity where
>   pure = Identity
>   (Identity f) <*> (Identity a) = Identity (f a)


Exercise: Constant Instance

Write an Applicative instance for Constant.

> newtype Constant a b = Constant { getConstant :: a }
>   deriving (Eq, Ord, Show)
>
> instance Functor (Constant a) where
>   fmap _ (Constant a) = Constant a
>
> instance Monoid a => Applicative (Constant a) where
>   pure _ = Constant mempty
>   (Constant l) <*> (Constant r) = Constant (l <> r)


Exercise: Fixer Upper

Given the function and values provided, use (<$>) from Functor, (<*>) and pure from the Applicative type class to fill in missing bits of the broken code to make it work.

1.

> fix1 =
>   const <$> Just "Hello" <*> pure "World"

2.

> fix2 =
>   (,,,) <$> Just 90 <*> Just 10  <*> Just "Tierness" <*> pure [1, 2, 3]


Chapter Exercises

Given a type that has an instance of Applicative, specialize the types of the methods.

1. []

pure  :: a -> [a]
(<*>) :: [(a -> b)] -> [a] -> [b]

2. IO

pure  :: a -> IO a
(<*>) :: IO (a -> b) -> IO a -> IO b

3. (,) a

pure  :: a -> (l, a)
(<*>) :: (l, (a -> b)) -> (l, a) -> (l, b)

4. (->) e

pure  :: a -> (e -> a)
(<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)


Combinations

> stops :: String
> stops = "pbtdkg"
>
> vowels :: String
> vowels = "aeiou"
>
> combos :: [a] -> [b] -> [c] -> [(a, b, c)]
> combos = liftA3 (,,)
