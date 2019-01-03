-- Addition.hs
module Addition where

import           Test.Hspec

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

mult :: (Eq a, Num a) => a -> a -> a
mult a b = go a b 0
  where
    go a b count
      | a == 0 = count
      | otherwise = go (a - 1) b (count + b)

main :: IO ()
main =
  hspec $ do
    describe "Addition" $ do
      it "1 + 1 is greater than 1" $ do (1 + 1) > 1 `shouldBe` True
      it "2 + 2 is equal to 4" $ do 2 + 2 `shouldBe` 4
    describe "Division" $ do
      it "15 divided by 3 is 5" $ do dividedBy 15 3 `shouldBe` (5, 0)
      it "22 divided by 5 is 4 remainder 2" $ do
        dividedBy 22 5 `shouldBe` (4, 2)
    describe "Multiplication" $ do
      it "4 times 5 is 20" $ do mult 4 5 `shouldBe` 20
      it "8 times 0 is 0" $ do mult 8 0 `shouldBe` 0
      it "6 times 1 is 6" $ do mult 6 1 `shouldBe` 6
