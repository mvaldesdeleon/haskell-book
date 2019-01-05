module Main where

import           Data.List               (elemIndices, (\\))
import           Hangman                 (Puzzle (..), fillInCharacter,
                                          freshPuzzle, handleGuess, randomWord',
                                          wrongGuesses)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

prop_randomWords :: Property
prop_randomWords =
  monadicIO $ do
    a <- run randomWord'
    b <- run randomWord'
    assert (a /= b)

prop_freshPuzzle :: [Char] -> Bool
prop_freshPuzzle w = length discovered == length word && null guessed
  where
    (Puzzle word discovered guessed) = freshPuzzle w

genPuzzle :: Gen Puzzle
genPuzzle = do
  w <- arbitrary
  return $ freshPuzzle $ getNonEmpty (w :: NonEmptyList Char)

instance Arbitrary Puzzle where
  arbitrary = genPuzzle

genCorrectGuess :: Puzzle -> Gen Char
genCorrectGuess p@(Puzzle word _ guessed) =
  let chars = word \\ guessed
   in if null chars
        then error $ "No remaining correct guesses for Puzzle: " ++ show p
        else elements chars

genIncorrectGuess :: Puzzle -> Gen Char
genIncorrectGuess p@(Puzzle word _ guessed) = do
  c <- arbitrary
  if c `notElem` word && c `notElem` guessed
    then return c
    else genIncorrectGuess p

genPuzzleAndCorrectGuess :: Gen (Puzzle, Char)
genPuzzleAndCorrectGuess = do
  p <- arbitrary
  c <- genCorrectGuess p
  return (p, c)

genPuzzleAndIncorrectGuesses :: Gen (Puzzle, [Char])
genPuzzleAndIncorrectGuesses = do
  p <- arbitrary
  n <- choose (1 :: Int, 20)
  c <- vectorOf n $ genIncorrectGuess p
  return (p, c)

genPuzzleAndIncorrectGuess :: Gen (Puzzle, Char)
genPuzzleAndIncorrectGuess = do
  p <- genPuzzle
  c <- genIncorrectGuess p
  return (p, c)

prop_fillInCharacterCorrect :: (Puzzle, Char) -> Bool
prop_fillInCharacterCorrect (p, c) = all (== Just c) filled && c `elem` guessed
  where
    (Puzzle word before _) = p
    (Puzzle _ after guessed) = fillInCharacter p c
    filled = map (after !!) $ elemIndices c word

prop_fillInCharacterIncorrect :: (Puzzle, Char) -> Bool
prop_fillInCharacterIncorrect (p, c) = before == after && c `elem` guessed
  where
    (Puzzle _ before _) = p
    (Puzzle _ after guessed) = fillInCharacter p c

prop_handleGuessNew :: Puzzle -> Char -> Property
prop_handleGuessNew p c =
  monadicIO $ do
    np <- run $ handleGuess p c
    return (np /= p)

prop_handleGuessOld :: Puzzle -> Char -> Property
prop_handleGuessOld p c =
  monadicIO $ do
    np <- run $ handleGuess p c
    nnp <- run $ handleGuess np c
    return (np == nnp)

prop_wrongGuesses :: (Puzzle, [Char]) -> Bool
prop_wrongGuesses (p, cs) = wrongGuesses fp == length cs
  where
    fp = foldr (flip fillInCharacter) p cs

main :: IO ()
main =
  hspec $ do
    describe "randomWords'" $ do
      it "generates random words in the IO monad" $ property prop_randomWords
    describe "freshPuzzle" $ do
      it
        "initializes the Puzzle with one guess slot per word character, and no guesses" $
        property prop_freshPuzzle
    describe "fillInCharacter" $ do
      it "Handles correct guesses" $
        property $ forAll genPuzzleAndCorrectGuess prop_fillInCharacterCorrect
      it "Handles incorrect guesses" $
        property $
        forAll genPuzzleAndIncorrectGuess prop_fillInCharacterIncorrect
    describe "handleGuess" $ do
      it "Handles new guesses" $ property prop_handleGuessNew
      it "Handles old guesses" $ property prop_handleGuessOld
    describe "wrongGuesses" $ do
      it "Counts wrong guesses" $
        property $ forAll genPuzzleAndIncorrectGuesses prop_wrongGuesses
