> module Chapter18 where
>
> import Control.Monad (join)

Write bind in terms of fmap and join. Fear is the mind-killer, friend. You can do it.

> bind :: Monad m => (a -> m b) -> m a -> m b
> bind f = join . fmap f


Short Exercise: Either Monad

Implement the Either Monad.

> data Sum a b = First a | Second b
>   deriving (Eq, Show)
>
> instance Functor (Sum a) where
>   fmap _ (First a) = First a
>   fmap f (Second b) = Second $ f b
>
> instance Applicative (Sum a) where
>   pure = Second
>   (Second f) <*> (Second b) = Second $ f b
>   (First a) <*> _ = First a
>   _ <*> (First a) = First a
>
> instance Monad (Sum a) where
>   return = pure
>   (Second b) >>= f = f b
>   (First a) >>= _ = First a


Write the following functions using the methods provided by Monad and Functor. Using stuff like identity and composition is fine, but it has to typecheck with types provided.

1.

> j :: Monad m => m (m a) -> m a
> j mma = mma >>= id

2.

> l1 :: Monad m => (a -> b) -> m a -> m b
> l1 f ma = ma >>= return . f

3.

> l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
> l2 f ma mb = ma >>= \a -> mb >>= \b -> return $ f a b

4.

> a :: Monad m => m a -> m (a -> b) -> m b
> a ma mf = ma >>= \a -> mf >>= \f -> return $ f a

5. You’ll need recursion for this one.

> meh :: Monad m => [a] -> (a -> m b) -> m [b]
> meh (a:as) f = f a >>= \b -> (meh as f) >>= \bs -> return $ b : bs

6. Hint: reuse “meh”

> flipType :: (Monad m) => [m a] -> m [a]
> flipType ms = meh ms id
