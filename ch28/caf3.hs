module Main where

incdInts :: [Integer] -> [Integer]
incdInts = map (+ 1)

main :: IO ()
main = do
  print (incdInts [1 ..] !! 1000)
  print (incdInts [1 ..] !! 9001)
  print (incdInts [1 ..] !! 90010)
  print (incdInts [1 ..] !! 9001000)
  print (incdInts [1 ..] !! 9501000)
  print (incdInts [1 ..] !! 9901000)
