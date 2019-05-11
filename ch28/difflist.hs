module Main where

import           Criterion.Main

newtype DList a = DL
  { unDL :: [a] -> [a]
  }

{-# INLINE empty #-}
empty :: DList a
empty = DL $ const []

{-# INLINE singleton #-}
singleton :: a -> DList a
singleton x = DL $ const [x]

{-# INLINE toList #-}
toList :: DList a -> [a]
toList xs = unDL xs []

infixr `cons`

{-# INLINE cons #-}
cons :: a -> DList a -> DList a
cons x xs = DL ((x :) . unDL xs)

infixl `snoc`

{-# INLINE snoc #-}
snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x :))

{-# INLINE append #-}
append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go 0 xs = xs
    go n xs = go (n - 1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where
    go 0 xs = xs
    go n xs = go (n - 1) (singleton n `append` xs)

main :: IO ()
main =
  defaultMain
    [ bench "concat list" $ whnf schlemiel 123456
    , bench "concat dlist" $ whnf constructDlist 123456
    ]
