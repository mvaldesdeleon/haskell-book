module Exercises
  ( First'
  , Trivial
  , Identity
  , Two
  , Three
  , Four
  , BoolConj
  , BoolDisj
  , Or
  , Combine
  , unCombine
  , Comp
  , unComp
  , Validation
  , Mem
  , runMem
  ) where

import           Test.QuickCheck hiding (Failure, Success)

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

data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') =
    Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> (BoolDisj False) = BoolDisj True
  (BoolDisj False) <> (BoolDisj True) = BoolDisj True
  _ <> _ = BoolDisj False

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd b) <> _ = Snd b
  _ <> (Snd b) = Snd b
  _ <> (Fst a) = Fst a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

newtype Combine a b = Combine
  { unCombine :: a -> b
  }

instance Show (Combine a b) where
  show (Combine _) = "Combine"

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\a -> f a <> g a) -- We can avoid the lambda, since Semigroup b => Semigroup (a -> b)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (const mempty)
  mappend = (<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

newtype Comp a = Comp
  { unComp :: a -> a
  }

instance Show (Comp a) where
  show (Comp _) = "Comp"

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

data Validation a b
  = Failure a
  | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Success b) <> _ = Success b
  _ <> (Success b) = Success b
  (Failure a) <> (Failure a') = Failure (a <> a')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = frequency [(1, Failure <$> arbitrary), (3, Success <$> arbitrary)]

newtype Mem s a = Mem
  { runMem :: s -> (a, s)
  }

instance Show (Mem s a) where
  show (Mem _) = "Mem"

instance Semigroup a => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) =
    Mem $ \s ->
      let (a', s') = g s
          (a'', s'') = f s'
       in (a'' <> a', s'')

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)

instance (CoArbitrary s, Arbitrary a, Arbitrary s) => Arbitrary (Mem s a) where
  arbitrary = Mem <$> arbitrary
