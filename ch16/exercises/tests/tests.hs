module Main where

import           Test.Hspec
import           Test.QuickCheck

import           Exercises       (Four, Four', Identity, Pair, Three, Three',
                                  Trivial, Two)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Bool
functorCompose (Fn f) (Fn g) x = (fmap g (fmap f x)) == (fmap (g . f) x)

type FId1 f a = f a -> Bool

type FId2 f x a = f x a -> Bool

type FId3 f x y a = f x y a -> Bool

type FId4 f x y z a = f x y z a -> Bool

type FComp1 f a b c = Fun a b -> Fun b c -> f a -> Bool

type FComp2 f x a b c = Fun a b -> Fun b c -> f x a -> Bool

type FComp3 f x y a b c = Fun a b -> Fun b c -> f x y a -> Bool

type FComp4 f x y z a b c = Fun a b -> Fun b c -> f x y z a -> Bool

main :: IO ()
main =
  hspec $ do
    describe "Identity" $ do
      it "Identity" $ property (functorIdentity :: FId1 Identity String)
      it "Compose" $
        property (functorCompose :: FComp1 Identity String Float Int)
    describe "Pair" $ do
      it "Identity" $ property (functorIdentity :: FId1 Identity String)
      it "Compose" $ property (functorCompose :: FComp1 Pair Float String Int)
    describe "Two" $ do
      it "Identity" $ property (functorIdentity :: FId2 Two String Float)
      it "Compose" $
        property (functorCompose :: FComp2 Two Float Double String Int)
    describe "Three" $ do
      it "Identity" $
        property (functorIdentity :: FId3 Three Double String Float)
      it "Compose" $
        property
          (functorCompose :: FComp3 Three (Maybe Int) Float Double String Int)
    describe "Three'" $ do
      it "Identity" $ property (functorIdentity :: FId2 Three' String Float)
      it "Compose" $
        property (functorCompose :: FComp2 Three' Float Double String Int)
    describe "Four" $ do
      it "Identity" $
        property (functorIdentity :: FId4 Four (Maybe Int) Double String Float)
      it "Compose" $
        property
          (functorCompose :: FComp4 Four (Maybe Int) [String] Float Double String Int)
    describe "Four'" $ do
      it "Identity" $ property (functorIdentity :: FId2 Four' String Float)
      it "Compose" $
        property (functorCompose :: FComp2 Four' Float Double String Int)
