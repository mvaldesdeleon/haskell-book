module Exercises
  ( List
  , ZipList'
  ) where

import           Test.QuickCheck
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

instance EqProp a => EqProp (List a) where
  Nil =-= Nil = property True
  (Cons x xs) =-= (Cons y ys) = x =-= y .&. xs =-= ys
  _ =-= _ = property False

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
