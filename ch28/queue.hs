module Main where

import           Criterion.Main
import           Data.Sequence  (Seq (..))
import qualified Data.Sequence  as S

data Queue a = Queue
  { enqueue :: [a]
  , dequeue :: [a]
  } deriving (Eq, Show)

empty :: Queue a
empty = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue back front) = Queue (x : back) front

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue back front) =
  case (back, front) of
    (_, x:xs) -> Just (x, Queue back xs)
    ([], []) -> Nothing
    (_, []) ->
      let (x:xs) = reverse back
       in Just (x, Queue xs [])

q :: Queue Int
q = Queue [1 .. 1000] []

pushPopQueue :: Queue Int -> Int -> Queue Int
pushPopQueue xs n = go n xs
  where
    go 0 xs = xs
    go n xs =
      case pop $ push n xs of
        Just (_, ys) -> go (n - 1) ys
        Nothing      -> go (n - 1) empty

pushL :: a -> [a] -> [a]
pushL = (:)

popL :: [a] -> Maybe (a, [a])
popL [] = Nothing
popL xs =
  let (x:nxs) = reverse xs
   in Just (x, reverse nxs)

pushPopList :: [Int] -> Int -> [Int]
pushPopList xs n = go n xs
  where
    go 0 xs = xs
    go n xs =
      case popL $ pushL n xs of
        Just (_, ys) -> go (n - 1) ys
        Nothing      -> go (n - 1) []

pushS :: a -> Seq a -> Seq a
pushS = (S.<|)

popS :: Seq a -> Maybe (a, Seq a)
popS Empty      = Nothing
popS (xs :|> x) = Just (x, xs)

s :: Seq Int
s = S.fromList [1 .. 1000]

pushPopSeq :: Seq Int -> Int -> Seq Int
pushPopSeq xs n = go n xs
  where
    go 0 xs = xs
    go n xs =
      case popS $ pushS n xs of
        Just (_, ys) -> go (n - 1) ys
        Nothing      -> go (n - 1) S.empty

main :: IO ()
main =
  defaultMain
    [ bench "push-pop queue, empty" $ whnf (pushPopQueue empty) 10000
    , bench "push-pop list, empty" $ whnf (pushPopList []) 10000
    , bench "push-pop sequence, empty" $ whnf (pushPopSeq S.empty) 10000
    , bench "push-pop queue, full" $ whnf (pushPopQueue q) 10000
    , bench "push-pop list, full" $ whnf (pushPopList [1 .. 1000]) 10000
    , bench "push-pop sequence, full" $ whnf (pushPopSeq s) 10000
    ]
