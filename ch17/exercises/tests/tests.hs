module Main where

import           Data.Monoid              (Product)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Exercises                (Four, Four', List, Pair, Three,
                                           Three', Two, Validation, ZipList')

main :: IO ()
main =
  hspec $ do
    describe "List" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: List (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative (undefined :: List (Int, Float, String)))
    describe "ZipList'" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: ZipList' (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative (undefined :: ZipList' (Int, Float, String)))
    describe "Validation" $ do
      it "Functor" $
        property
          (quickBatch $
           functor (undefined :: Validation String (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $
           applicative (undefined :: Validation String (Int, Float, String)))
    describe "Pair" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: Pair (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative (undefined :: Pair (Int, Float, String)))
    describe "Two" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: Two String (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $
           applicative (undefined :: Two String (Int, Float, String)))
    describe "Three" $ do
      it "Functor" $
        property
          (quickBatch $
           functor (undefined :: Three String [Int] (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $
           applicative (undefined :: Three String [Int] (Int, Float, String)))
    describe "Three'" $ do
      it "Functor" $
        property
          (quickBatch $
           functor (undefined :: Three' String (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $
           applicative (undefined :: Three' String (Int, Float, String)))
    describe "Four" $ do
      it "Functor" $
        property
          (quickBatch $
           functor
             (undefined :: Four (Product Int) String [Int] (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $
           applicative
             (undefined :: Four (Product Int) String [Int] (Int, Float, String)))
    describe "Four'" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: Four' String (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $
           applicative (undefined :: Four' String (Int, Float, String)))
