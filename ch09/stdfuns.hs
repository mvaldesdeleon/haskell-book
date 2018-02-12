module StdFuns where

-- myOr returns True if any Bool in the list is True
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x then True
                   else myOr xs

-- myAny returns True if a -> Bool applied to any of the values in the list returns True
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if f x then True
                        else myAny f xs

-- After you write the recursive myElem, write another version that uses any.
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs) = if y == x then True
                            else myElem y xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' y = myAny $ (==) y

-- Implement myReverse.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- squishMap maps a function over a list and concatenates the results.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

-- squishAgain flattens a list of lists into a list. This time re-use the squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returned GT for.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:[]) = x
myMaximumBy f (x:xs) = case f x mxs of
                         LT -> mxs
                         EQ -> x
                         GT -> x
                       where mxs = myMaximumBy f xs

-- myMinimumBy takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:[]) = x
myMinimumBy f (x:xs) = case f x mxs of
                         LT -> x
                         EQ -> x
                         GT -> mxs
                       where mxs = myMinimumBy f xs

-- Using the myMinimumBy and myMaximumBy functions, write your own versions of maximum and minimum.
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
