module Main where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Exercises                (Identity, List, Nope,
                                           PhhhbbtttEither)

main :: IO ()
main =
  hspec $ do
    describe "Nope" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: Nope (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative (undefined :: Nope (Int, Float, String)))
      it "Monad" $
        property (quickBatch $ monad (undefined :: Nope (Int, Float, String)))
    describe "PhhhbbtttEither" $ do
      it "Functor" $
        property
          (quickBatch $
           functor (undefined :: PhhhbbtttEither Float (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $
           applicative (undefined :: PhhhbbtttEither Float (Int, Float, String)))
      it "Monad" $
        property
          (quickBatch $
           monad (undefined :: PhhhbbtttEither Float (Int, Float, String)))
    describe "Identity" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: Identity (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative (undefined :: Identity (Int, Float, String)))
      it "Monad" $
        property
          (quickBatch $ monad (undefined :: Identity (Int, Float, String)))
    describe "List" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: List (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative (undefined :: List (Int, Float, String)))
      it "Monad" $
        property (quickBatch $ monad (undefined :: List (Int, Float, String)))
