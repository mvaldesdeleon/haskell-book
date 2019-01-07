module Exercises
  ( First'
  ) where

import           Test.QuickCheck

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

newtype First' a = First'
  { getFirst' :: Optional a
  } deriving (Eq, Show)

instance Semigroup (First' a) where
  (First' (Only a)) <> _ = First' (Only a)
  _ <> (First' (Only b)) = First' (Only b)
  _ <> _ = First' Nada

instance Monoid (First' a) where
  mempty = First' Nada
  mappend = (<>)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (3, Only <$> arbitrary)]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = First' <$> arbitrary
