module Main where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Exercises                (Big, Bigger, Constant, Identity,
                                           List, Optional, Pair, S, Three, Tree)

main :: IO ()
main =
  hspec $ do
    describe "Identity" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: Identity (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $
           foldable (undefined :: Identity (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $ traversable (undefined :: Identity (Int, Float, String)))
    describe "Constant" $ do
      it "Functor" $
        property
          (quickBatch $
           functor (undefined :: Constant String (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $
           foldable
             (undefined :: Constant String (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $
           traversable (undefined :: Constant String (Int, Float, String)))
    describe "Optional" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: Optional (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $
           foldable (undefined :: Optional (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $ traversable (undefined :: Optional (Int, Float, String)))
    describe "List" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: List (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $
           foldable (undefined :: List (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $ traversable (undefined :: List (Int, Float, String)))
    describe "Three" $ do
      it "Functor" $
        property
          (quickBatch $
           functor (undefined :: Three String Int (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $
           foldable
             (undefined :: Three String Int (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $
           traversable (undefined :: Three String Int (Int, Float, String)))
    describe "Pair" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: Pair String (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $
           foldable (undefined :: Pair String (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $
           traversable (undefined :: Pair String (Int, Float, String)))
    describe "Big" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: Big String (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $
           foldable (undefined :: Big String (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $
           traversable (undefined :: Big String (Int, Float, String)))
    describe "Bigger" $ do
      it "Functor" $
        property
          (quickBatch $
           functor (undefined :: Bigger String (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $
           foldable
             (undefined :: Bigger String (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $
           traversable (undefined :: Bigger String (Int, Float, String)))
    describe "S" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: S [] (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $
           foldable (undefined :: S [] (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $ traversable (undefined :: S [] (Int, Float, String)))
    describe "Tree" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: Tree (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $
           foldable (undefined :: Tree (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $ traversable (undefined :: Tree (Int, Float, String)))
