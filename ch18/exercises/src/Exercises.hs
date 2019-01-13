module Exercises
  ( Nope
  , PhhhbbtttEither
  , Identity
  , List
  ) where

import           Prelude                  hiding (Left, Right)
import           Test.QuickCheck
import           Test.QuickCheck.Checkers

data Nope a =
  NopeDotJpg
  deriving (Show, Eq)

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

data PhhhbbtttEither b a
  = Left a
  | Right b
  deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = frequency [(3, Left <$> arbitrary), (1, Right <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

instance Functor (PhhhbbtttEither b) where
  fmap f (Left a)  = Left $ f a
  fmap _ (Right b) = Right b

instance Applicative (PhhhbbtttEither b) where
  pure = Left
  (Left f) <*> (Left a) = Left $ f a
  (Right b) <*> _ = Right b
  _ <*> (Right b) = Right b

instance Monad (PhhhbbtttEither b) where
  return = pure
  (Left a) >>= f = f a
  (Right b) >>= _ = Right b

newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity $ f a

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Show, Eq)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (3, Cons <$> arbitrary <*> arbitrary)]

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

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons a as) >>= f = f a `append` (as >>= f)
