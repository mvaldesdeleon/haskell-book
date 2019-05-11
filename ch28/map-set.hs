module Main where

import           Criterion.Main
import qualified Data.Map       as M
import qualified Data.Set       as S

bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where
    stream = iterate bumpIt (0, 0)

n :: M.Map Int Int
n = M.fromList $ take 10000 stream
  where
    stream = iterate bumpIt (10000, 10000)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where
    stream = iterate (+ 1) 0

t :: S.Set Int
t = S.fromList $ take 10000 stream
  where
    stream = iterate (+ 1) 10000

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

insertMap :: Int -> M.Map Int Int
insertMap i = M.insert i i m

insertSet :: Int -> S.Set Int
insertSet i = S.insert i s

unionMap :: () -> M.Map Int Int
unionMap _ = M.union m n

unionSet :: () -> S.Set Int
unionSet _ = S.union s t

main :: IO ()
main =
  defaultMain
    [ bench "member check map" $ whnf membersMap 9999
    , bench "member check set" $ whnf membersSet 9999
    , bench "insert map" $ whnf insertMap 10000
    , bench "insert set" $ whnf insertSet 10000
    , bench "union map" $ whnf unionMap ()
    , bench "union set" $ whnf unionSet ()
    ]
