{-# LANGUAGE FlexibleContexts #-}

module Exercises
  ( Identity
  , Constant
  , Optional
  , List
  , Three
  , Pair
  , Big
  , Bigger
  , S
  , Tree
  ) where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers

newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

newtype Constant a b =
  Constant
    { getConstant :: a
    }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = Constant <$> pure a

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

data Optional a
  = Nada
  | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (3, Yep <$> arbitrary)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons a as) = f a <> foldMap f as

instance Traversable List where
  traverse _ Nil         = pure Nil
  traverse f (Cons a as) = Cons <$> f a <*> traverse f as

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (3, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

data Three a b c =
  Three a b c
  deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Pair a b =
  Pair a b
  deriving (Eq, Ord, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

data Big a b =
  Big a b b
  deriving (Eq, Ord, Show)

instance Functor (Big a) where
  fmap f (Big a bl br) = Big a (f bl) (f br)

instance Foldable (Big a) where
  foldMap f (Big _ bl br) = f bl <> f br

instance Traversable (Big a) where
  traverse f (Big a bl br) = Big a <$> f bl <*> f br

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

data Bigger a b =
  Bigger a b b b
  deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a bl bm br) = Bigger a (f bl) (f bm) (f br)

instance Foldable (Bigger a) where
  foldMap f (Bigger _ bl bm br) = f bl <> f bm <> f br

instance Traversable (Bigger a) where
  traverse f (Bigger a bl bm br) = Bigger a <$> f bl <*> f bm <*> f br

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

data S n a =
  S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S fa a) = S (f <$> fa) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S fa a) = foldMap f fa <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S fa a) = S <$> traverse f fa <*> f a

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) =>
         EqProp (S n a) where
  (=-=) = eq

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty          = Empty
  fmap f (Leaf a)       = Leaf (f a)
  fmap f (Node lt a rt) = Node (fmap f lt) (f a) (fmap f rt)

instance Foldable Tree where
  foldMap _ Empty          = mempty
  foldMap f (Leaf a)       = f a
  foldMap f (Node lt a rt) = foldMap f lt <> f a <> foldMap f rt

instance Traversable Tree where
  traverse _ Empty          = pure Empty
  traverse f (Leaf a)       = Leaf <$> f a
  traverse f (Node lt a rt) = Node <$> traverse f lt <*> f a <*> traverse f rt

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    frequency
      [ (1, return Empty)
      , (2, Leaf <$> arbitrary)
      , (3, Node <$> arbitrary <*> arbitrary <*> arbitrary)
      ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq
