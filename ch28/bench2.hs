module Main where

import           Criterion.Main

infixl 9 !?

{-# INLINABLE (!?) #-}
xs !? n
  | n < 0 = Nothing
  | otherwise =
    foldr
      (\x r k ->
         case k of
           0 -> Just x
           _ -> r (k - 1))
      (const Nothing)
      xs
      n

myList :: [Int]
myList = [1 .. 9999]

main :: IO ()
main =
  defaultMain
    [ bench "index list 9999" $ whnf (myList !!) 9998
    , bench "index list maybe index 9999" $ whnf (myList !?) 9998
    ]
