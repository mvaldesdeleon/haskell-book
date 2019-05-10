> {-# LANGUAGE InstanceSigs #-}
>
> module Chapter25 where
>
> newtype Compose f g a =
>   Compose { getCompose :: f (g a) }
>   deriving (Eq, Show)
>
> instance (Functor f, Functor g) => Functor (Compose f g) where
>   f `fmap` (Compose fga) = Compose $ (fmap . fmap) f fga
>
> instance (Applicative f, Applicative g) => Applicative (Compose f g) where
>   pure :: a -> Compose f g a
>   pure a = Compose $ (pure . pure) a
>
>   (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
>   (Compose fgf) <*> (Compose fga) = Compose $ (<*>) <$> fgf <*> fga

Exercises: Compose Instances

1. Write the Compose Foldable instance.

> instance (Foldable f, Foldable g) => Foldable (Compose f g) where
>   foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
>   foldMap f (Compose fga) = (foldMap . foldMap) f fga

2. Write the Compose Traversable instance.


> instance (Traversable f, Traversable g) => Traversable (Compose f g) where
>   traverse :: Applicative h => (a -> h b) -> Compose f g a -> h (Compose f g b)
>   traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

And now for something completely different

> class Bifunctor p where
>   {-# MINIMAL bimap | first, second #-}
>
>   bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
>   bimap f g = first f . second g
>
>   first :: (a -> b) -> p a c -> p b c
>   first f = bimap f id
>
>   second :: (b -> c) -> p a b -> p a c
>   second = bimap id

Write Bifunctor instances for the following types:

1.

> data Deux a b = Deux a b
>
> instance Bifunctor Deux where
>   bimap f g (Deux a b) = Deux (f a) (g b)

2.

> data Const a b = Const a
>
> instance Bifunctor Const where
>   bimap f _ (Const a) = Const (f a)

3.

> data Drei a b c = Drei a b c
>
> instance Bifunctor (Drei a) where
>   bimap f g (Drei a b c) = Drei a (f b) (g c)

4.

> data SuperDrei a b c = SuperDrei a b
>
> instance Bifunctor (SuperDrei a) where
>   bimap f _ (SuperDrei a b) = SuperDrei a (f b)

5.

> data SemiDrei a b c = SemiDrei a
>
> instance Bifunctor (SemiDrei a) where
>   bimap _ _ (SemiDrei a) = SemiDrei a

6.

> data Quadriceps a b c d = Quadzzz a b c d
>
> instance Bifunctor (Quadriceps a b) where
>   bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

7.

> data Eitherino a b = Lefty a | Righty b
>
> instance Bifunctor Eitherino where
>   bimap f _ (Lefty a) = Lefty (f a)
>   bimap _ g (Righty b) = Righty (g b)
