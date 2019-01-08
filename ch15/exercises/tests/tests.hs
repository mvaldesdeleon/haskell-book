module Main where

import           Data.Monoid     (Product, Sum)
import           Test.Hspec
import           Test.QuickCheck

import           Exercises       (BoolConj, BoolDisj, Combine, Comp, First',
                                  Four, Identity, Or, Three, Trivial, Two,
                                  Validation, unCombine, unComp)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

combAssoc ::
     (Eq b, Semigroup b)
  => Combine a b
  -> Combine a b
  -> Combine a b
  -> a
  -> Bool
combAssoc f g h a = unCombine (f <> (g <> h)) a == unCombine ((f <> g) <> h) a

compAssoc :: Eq a => Comp a -> Comp a -> Comp a -> a -> Bool
compAssoc f g h a = unComp (f <> (g <> h)) a == unComp ((f <> g) <> h) a

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

type FstAssoc = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

type TrvAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdAssoc a = Identity a -> Identity a -> Identity a -> Bool

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

type CombAssoc a b = Combine a b -> Combine a b -> Combine a b -> a -> Bool

type CompAssoc a = Comp a -> Comp a -> Comp a -> a -> Bool

type ValAssoc a b = Validation a b -> Validation a b -> Validation a b -> Bool

main :: IO ()
main =
  hspec $ do
    describe "First' Monoid" $ do
      it "Associative" $ property (monoidAssoc :: FstAssoc)
      it "Left identity" $ property (monoidLeftIdentity :: FstId)
      it "Right identity" $ property (monoidRightIdentity :: FstId)
    describe "Trivial Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: TrvAssoc)
    describe "Identity Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: IdAssoc String)
    describe "Two Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: TwoAssoc (Sum Int) String)
    describe "Three Semigroup" $ do
      it "Associative" $
        property (semigroupAssoc :: ThreeAssoc (Sum Int) String [Bool])
    describe "Four Semigroup" $ do
      it "Associative" $
        property
          (semigroupAssoc :: FourAssoc (Sum Int) String [Bool] (Product Int))
    describe "BoolConj Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: BoolConjAssoc)
    describe "BoolDisj Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: BoolDisjAssoc)
    describe "Or Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: OrAssoc (Sum Int) String)
    describe "Combine Semigroup" $ do
      it "Associative" $ property (combAssoc :: CombAssoc Float (Product Int))
    describe "Comp Semigroup" $ do
      it "Associative" $ property (compAssoc :: CompAssoc Float)
    describe "Validation Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: ValAssoc (Sum Int) String)
