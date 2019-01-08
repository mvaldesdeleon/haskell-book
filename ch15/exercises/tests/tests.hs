module Main where

import           Data.Monoid     (Product, Sum)
import           Test.Hspec
import           Test.QuickCheck

import           Exercises       (BoolConj, BoolDisj, Combine, Comp, First',
                                  Four, Identity, Mem, Or, Three, Trivial, Two,
                                  Validation, runMem, unCombine, unComp)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

combSemigroupAssoc ::
     (Eq b, Semigroup b)
  => Combine a b
  -> Combine a b
  -> Combine a b
  -> a
  -> Bool
combSemigroupAssoc f g h a =
  unCombine (f <> (g <> h)) a == unCombine ((f <> g) <> h) a

combMonoidAssoc ::
     (Eq b, Monoid b) => Combine a b -> Combine a b -> Combine a b -> a -> Bool
combMonoidAssoc f g h a =
  unCombine (f <> (g <> h)) a == unCombine ((f <> g) <> h) a

combLeftIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
combLeftIdentity f a = unCombine (mempty <> f) a == unCombine f a

combRightIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
combRightIdentity f a = unCombine (f <> mempty) a == unCombine f a

compAssoc :: Eq a => Comp a -> Comp a -> Comp a -> a -> Bool
compAssoc f g h a = unComp (f <> (g <> h)) a == unComp ((f <> g) <> h) a

compLeftIdentity :: Eq a => Comp a -> a -> Bool
compLeftIdentity f a = unComp (mempty <> f) a == unComp f a

compRightIdentity :: Eq a => Comp a -> a -> Bool
compRightIdentity f a = unComp (f <> mempty) a == unComp f a

memMonoidAssoc ::
     (Eq s, Eq a, Monoid a) => Mem s a -> Mem s a -> Mem s a -> s -> Bool
memMonoidAssoc f g h a = runMem (f <> (g <> h)) a == runMem ((f <> g) <> h) a

memLeftIdentity :: (Eq s, Eq a, Monoid a) => Mem s a -> s -> Bool
memLeftIdentity f a = runMem (mempty <> f) a == runMem f a

memRightIdentity :: (Eq s, Eq a, Monoid a) => Mem s a -> s -> Bool
memRightIdentity f a = runMem (f <> mempty) a == runMem f a

type FstAssoc = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

type TrvAssoc = Trivial -> Trivial -> Trivial -> Bool

type TrvId = Trivial -> Bool

type IdAssoc a = Identity a -> Identity a -> Identity a -> Bool

type IdId a = Identity a -> Bool

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

type TwoId a b = Two a b -> Bool

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type BoolConjId = BoolConj -> Bool

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type BoolDisjId = BoolDisj -> Bool

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

type CombAssoc a b = Combine a b -> Combine a b -> Combine a b -> a -> Bool

type CombId a b = Combine a b -> a -> Bool

type CompAssoc a = Comp a -> Comp a -> Comp a -> a -> Bool

type CompId a = Comp a -> a -> Bool

type ValAssoc a b = Validation a b -> Validation a b -> Validation a b -> Bool

type MemAssoc s a = Mem s a -> Mem s a -> Mem s a -> s -> Bool

type MemId s a = Mem s a -> s -> Bool

main :: IO ()
main =
  hspec $ do
    describe "First' Monoid" $ do
      it "Associative" $ property (monoidAssoc :: FstAssoc)
      it "Left identity" $ property (monoidLeftIdentity :: FstId)
      it "Right identity" $ property (monoidRightIdentity :: FstId)
    describe "Trivial Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: TrvAssoc)
    describe "Trivial Monoid" $ do
      it "Associative" $ property (monoidAssoc :: TrvAssoc)
      it "Left identity" $ property (monoidLeftIdentity :: TrvId)
      it "Right identity" $ property (monoidRightIdentity :: TrvId)
    describe "Identity Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: IdAssoc String)
    describe "Identity Monoid" $ do
      it "Associative" $ property (monoidAssoc :: IdAssoc String)
      it "Left identity" $ property (monoidLeftIdentity :: IdId String)
      it "Right identity" $ property (monoidRightIdentity :: IdId String)
    describe "Two Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: TwoAssoc (Sum Int) String)
    describe "Two Monoid" $ do
      it "Associative" $ property (monoidAssoc :: TwoAssoc (Sum Int) String)
      it "Left identity" $
        property (monoidLeftIdentity :: TwoId (Sum Int) String)
      it "Right identity" $
        property (monoidRightIdentity :: TwoId (Sum Int) String)
    describe "Three Semigroup" $ do
      it "Associative" $
        property (semigroupAssoc :: ThreeAssoc (Sum Int) String [Bool])
    describe "Four Semigroup" $ do
      it "Associative" $
        property
          (semigroupAssoc :: FourAssoc (Sum Int) String [Bool] (Product Int))
    describe "BoolConj Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: BoolConjAssoc)
    describe "BoolConj Monoid" $ do
      it "Associative" $ property (monoidAssoc :: BoolConjAssoc)
      it "Left identity" $ property (monoidLeftIdentity :: BoolConjId)
      it "Right identity" $ property (monoidRightIdentity :: BoolConjId)
    describe "BoolDisj Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: BoolDisjAssoc)
    describe "BoolDisj Monoid" $ do
      it "Associative" $ property (monoidAssoc :: BoolDisjAssoc)
      it "Left identity" $ property (monoidLeftIdentity :: BoolDisjId)
      it "Right identity" $ property (monoidRightIdentity :: BoolDisjId)
    describe "Or Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: OrAssoc (Sum Int) String)
    describe "Combine Semigroup" $ do
      it "Associative" $
        property (combSemigroupAssoc :: CombAssoc Float (Product Int))
    describe "Combine Monoid" $ do
      it "Associative" $
        property (combMonoidAssoc :: CombAssoc Float (Product Int))
      it "Left identity" $
        property (combLeftIdentity :: CombId Float (Product Int))
      it "Right identity" $
        property (combRightIdentity :: CombId Float (Product Int))
    describe "Comp Semigroup" $ do
      it "Associative" $ property (compAssoc :: CompAssoc Float)
    describe "Comp Monoid" $ do
      it "Associative" $ property (compAssoc :: CompAssoc Float)
      it "Left identity" $ property (compLeftIdentity :: CompId Float)
      it "Right identity" $ property (compRightIdentity :: CompId Float)
    describe "Validation Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: ValAssoc (Sum Int) String)
    describe "Mem Monoid" $ do
      it "Associative" $
        property (memMonoidAssoc :: MemAssoc Float (Product Int))
      it "Left identity" $
        property (memLeftIdentity :: MemId Float (Product Int))
      it "Right identity" $
        property (memRightIdentity :: MemId Float (Product Int))
