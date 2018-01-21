module ExercisesChapter2 where

scream :: String -> String
scream str = str ++ "!"

fifthElement :: [a] -> a
fifthElement xs = xs !! 4

dropNine :: [a] -> [a]
dropNine xs = drop 9 xs

thirdLetter :: String -> Char
thirdLetter xs = xs !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! (x - 1)

rvrs :: String -> String
rvrs cia = let curry = take 5 cia
               is = take 4 (drop 5 cia) -- Include the spaces around `is`
               awesome = take 7 (drop 9 cia)
           in awesome ++ is ++ curry

main :: IO ()
main = print $ rvrs "Curry is awesome"
