> {-# LANGUAGE InstanceSigs #-}
>
> module Chapter22 where
>
> import Control.Applicative
> import Data.Maybe

Short Exercise: Warming Up

> import Data.Char
>
> cap :: [Char] -> [Char]
> cap xs = map toUpper xs
>
> rev :: [Char] -> [Char]
> rev xs = reverse xs
>
> composed :: [Char] -> [Char]
> composed = cap . rev
>
> fmapped :: [Char] -> [Char]
> fmapped = fmap cap rev
>
> tupled :: [Char] -> ([Char], [Char])
> tupled = (,) <$> cap <*> rev
>
> tupledM :: [Char] -> ([Char], [Char])
> tupledM = do
>   c <- cap
>   r <- rev
>   return (c, r)
>
> tupledM' :: [Char] -> ([Char], [Char])
> tupledM' = cap >>= \c -> rev >>= \r -> return (c, r)

Exercise: Ask

> newtype Reader r a =
>   Reader { runReader :: r -> a }
>
> ask :: Reader a a
> ask = Reader id

Exercise: Reading Comprehension

1. Write liftA2 yourself. Think about it in terms of abstracting out the difference between getDogR and getDogR' if that helps.

> myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
> myLiftA2 f fa fb = f <$> fa <*> fb

2. Write the following function. Again, it is simpler than it looks.

> asks :: (r -> a) -> Reader r a
> asks f = Reader f

3. Implement the Applicative for Reader.

> instance Functor (Reader r) where
>   fmap f (Reader ra) = Reader $ f . ra
>
> instance Applicative (Reader r) where
>   pure :: a -> Reader r a
>   pure a = Reader $ const a
>   (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
>   (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r $ ra r

Exercise: Reader Monad

1. Implement the Reader Monad.

> instance Monad (Reader r) where
>   return = pure
>   (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
>   (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

2. Rewrite the monadic getDogRM to use your Reader datatype.

> newtype HumanName = HumanName String
>   deriving (Eq, Show)
>
> newtype DogName = DogName String
>   deriving (Eq, Show)
>
> newtype Address = Address String
>   deriving (Eq, Show)
>
> data Person = Person {
>       humanName :: HumanName
>     , dogName :: DogName
>     , address :: Address
>   } deriving (Eq, Show)
>
> data Dog = Dog {
>       dogsName :: DogName
>     , dogsAddress :: Address
>   } deriving (Eq, Show)
>
> getDogRM :: Reader Person Dog
> getDogRM = do
>   name <- asks dogName
>   addy <- asks address
>   return $ Dog name addy

Chapter Exercises

A warm-up stretch

> x = [1, 2, 3]
> y = [4, 5, 6]
> z = [7, 8, 9]
>
> xs :: Maybe Integer
> xs = lookup 3 $ zip x y
>
> ys :: Maybe Integer
> ys = lookup 6 $ zip y z
>
> zs :: Maybe Integer
> zs = lookup 4 $ zip x y
>
> z' :: Integer -> Maybe Integer
> z' n = lookup n $ zip x z
>
> x1 :: Maybe (Integer, Integer)
> x1 = (,) <$> xs <*> ys
>
> x2 :: Maybe (Integer, Integer)
> x2 = (,) <$> ys <*> zs
>
> x3 :: Integer -> (Maybe Integer, Maybe Integer)
> x3 = (,) <$> z' <*> z'
>
> summed :: Num c => (c, c) -> c
> summed = uncurry (+)
>
> bolt :: Integer -> Bool -- use &&, >3, <8
> bolt = (&&) <$> (>3) <*> (<8)
>
> sequA :: Integral a => a -> [Bool]
> sequA m = sequenceA [(>3), (<8), even] m
>
> s' :: Maybe Integer
> s' = summed <$> ((,) <$> xs <*> ys)
>
> main :: IO ()
> main = do
>   print $
>       sequenceA [Just 3, Just 2, Just 1]
>   print $ sequenceA [x, y]
>   print $ sequenceA [xs, ys]
>   print $ summed <$> ((,) <$> xs <*> ys)
>   print $ fmap summed ((,) <$> xs <*> zs)
>   print $ bolt 7
>   print $ fmap bolt z
>   print $ sequenceA [(>3), (<8), even] 7
>   print $ foldr (&&) True $ sequA 7
>   print $ fromMaybe [] $ sequA <$> s'
>   print $ fromMaybe False $ bolt <$> ys 
