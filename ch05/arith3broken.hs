-- arith3broken.hs
module Arith3Broken where

main :: IO ()
main = do -- was `Main`
    print $ 1 + 2 -- missing `$`
    putStrLn "10" -- was `10`
    print (negate 1) -- was `-1`, defeating the point of the `negate`
    print ((+) 0 blah)
    where blah = negate 1
