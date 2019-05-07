> {-# LANGUAGE LambdaCase #-}
>
> module Chapter20 where
>
> import Data.Monoid

Exercises: Library Functions

Implement the functions in terms of foldMap or foldr from Foldable, then try them out with multiple types that have Foldable instances.

1. This and the next one are nicer with foldMap, but foldr is fine too.

> sum' :: (Foldable t, Num a) => t a -> a
> sum' = getSum . foldMap Sum

2.

> product' :: (Foldable t, Num a) => t a -> a
> product' = getProduct . foldMap Product

3.

> elem' :: (Foldable t, Eq a) => a -> t a -> Bool
> elem' a = getAny . foldMap (Any . (== a))

4.

> minimum' :: (Foldable t, Ord a) => t a -> Maybe a
> minimum' = foldr f Nothing
>   where
>     f a = \case
>               Just m -> Just $ min m a
>               Nothing -> Just a

5.

> maximum' :: (Foldable t, Ord a) => t a -> Maybe a
> maximum' = foldr f Nothing
>   where
>     f a = \case
>               Just m -> Just $ max m a
>               Nothing -> Just a

6.

> null' :: (Foldable t) => t a -> Bool
> null' = foldr f True
>   where
>     f _ _ = False

7.

> length' :: (Foldable t) => t a -> Int
> length' = getSum . foldMap (const $ Sum 1)

8. Some say this is all Foldable amounts to.

> toList' :: (Foldable t) => t a -> [a]
> toList' = foldMap pure

9. Hint: use foldMap.

> fold' :: (Foldable t, Monoid m) => t m -> m
> fold' = foldMap id

10. Define foldMap in terms of foldr.

> foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
> foldMap' f = foldr g mempty
>   where
>     g a b = b <> f a


Chapter Exercises

Write Foldable instances for the following datatypes.

1.

> data Constant a b = Constant b
>
> instance Foldable (Constant a) where
>   foldMap f (Constant b) = f b

2.

> data Two a b = Two a b
>
> instance Foldable (Two a) where
>   foldMap f (Two _ b) = f b

3.

> data Three a b c = Three a b c
>
> instance Foldable (Three a b) where
>   foldMap f (Three _ _ c) = f c

4.

> data Three' a b = Three' a b b
>
> instance Foldable (Three' a) where
>   foldMap f (Three' _ l r) = f l <> f r

5.

> data Four' a b = Four' a b b b
>
> instance Foldable (Four' a) where
>   foldMap f (Four' _ l m r) = f l <> f m <> f r

Thinking cap time. Write a filter function for Foldable types using
foldMap.

> filterF :: ( Applicative f
>            , Foldable t
>            , Monoid (f a))
>         => (a -> Bool) -> t a -> f a
> filterF pr = foldMap f
>   where
>     f a = if pr a then pure a
>                   else mempty
