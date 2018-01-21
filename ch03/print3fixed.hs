-- print3fixed.hs
module Print3Fixed where

greeting :: String
greeting = "Yarrrrr"

printSecond :: IO ()
printSecond = do
    putStrLn greeting

main :: IO ()
main = do
    putStrLn greeting
    printSecond