module StdFuns where

-- myOr returns True if any Bool in the list is True
myOr :: [Bool] -> Bool
myOr = undefined

-- myAny returns True if a -> Bool applied to any of the values in the list returns True
myAny :: (a -> Bool) -> [a] -> Bool
myAny = undefined

-- After you write the recursive myElem, write another version that uses any.
myElem :: Eq a => a -> [a] -> Bool
myElem = undefined

-- Implement myReverse.
myReverse :: [a] -> [a]
myReverse = undefined

-- squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish = undefined

-- squishMap maps a function over a list and concatenates the results.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = undefined


-- squishAgain flattens a list of lists into a list. This time re-use the squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain = undefined

-- myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returned GT for.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = undefined

-- myMinimumBy takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = undefined

-- Using the myMinimumBy and myMaximumBy functions, write your own versions of maximum and minimum.
myMaximum :: (Ord a) => [a] -> a
myMaximum = undefined

myMinimum :: (Ord a) => [a] -> a
myMinimum = undefined
