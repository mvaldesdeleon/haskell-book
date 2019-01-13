module Exercises
  ( List
  , ZipList'
  , Validation
  , Pair
  , Two
  , Three
  , Three'
  , Four
  , Four'
  ) where

import           Test.QuickCheck          hiding (Failure, Success)
import           Test.QuickCheck.Checkers

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons a as)
  | n <= 0 = Nil
  | otherwise = Cons a (take' (n - 1) as)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (2, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> as = fmap f as `append` (fs <*> as)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
         in take' 3000 l
      ys' =
        let (ZipList' l) = ys
         in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (fmap f xs)

instance Applicative ZipList' where
  pure a = ZipList' as
    where
      as = Cons a as
  (ZipList' Nil) <*> _ = ZipList' Nil
  _ <*> (ZipList' Nil) = ZipList' Nil
  ZipList' (Cons f fs) <*> ZipList' (Cons a as) = ZipList' $ Cons (f a) bs
    where
      (ZipList' bs) = (ZipList' fs <*> ZipList' as)

data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = frequency [(1, Failure <$> arbitrary), (2, Success <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (Validation a b) where
  (=-=) = eq

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (Failure l) <*> (Failure r) = Failure (l <> r)
  (Success f) <*> (Success a) = Success (f a)
  (Failure e) <*> _ = Failure e
  _ <*> (Failure e) = Failure e

data Pair a =
  Pair a
       a
  deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    l <- arbitrary
    r <- arbitrary
    return $ Pair l r

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

instance Functor Pair where
  fmap f (Pair l r) = Pair (f l) (f r)

instance Applicative Pair where
  pure a = Pair a a
  (Pair fl fr) <*> (Pair al ar) = Pair (fl al) (fr ar)

data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two l f) <*> (Two r a) = Two (l <> r) (f a)

data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three l' l'' f) <*> (Three r' r'' a) = Three (l' <> r') (l'' <> r'') (f a)

data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    l <- arbitrary
    r <- arbitrary
    return $ Three' a l r

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance Functor (Three' a) where
  fmap f (Three' a l r) = Three' a (f l) (f r)

instance Monoid a => Applicative (Three' a) where
  pure a = Three' mempty a a
  (Three' l f' f'') <*> (Three' r a' a'') = Three' (l <> r) (f' a') (f'' a'')

data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four l' l'' l''' f) <*> (Four r' r'' r''' a) =
    Four (l' <> r') (l'' <> r'') (l''' <> r''') (f a)

data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    l <- arbitrary
    m <- arbitrary
    r <- arbitrary
    b <- arbitrary
    return $ Four' l m r b

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance Functor (Four' a) where
  fmap f (Four' l m r b) = Four' l m r (f b)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' l' l'' l''' f) <*> (Four' r' r'' r''' a) =
    Four' (l' <> r') (l'' <> r'') (l''' <> r''') (f a)
