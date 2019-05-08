> {-# LANGUAGE InstanceSigs #-}
>
> module Chapter23 where
>
> import System.Random (StdGen, randomR)
> import qualified Control.Monad.Trans.State as S (State, get, put, execState)
>
> data Die =
>     DieOne
>   | DieTwo
>   | DieThree
>   | DieFour
>   | DieFive
>   | DieSix
>   deriving (Eq, Show)
>
> intToDie :: Int -> Die
> intToDie n =
>   case n of
>       1 -> DieOne
>       2 -> DieTwo
>       3 -> DieThree
>       4 -> DieFour
>       5 -> DieFive
>       6 -> DieSix
>       x -> error $ "intToDie got non 1-6 integer: " ++ show x
>
> rollsToGetTwenty :: StdGen -> Int
> rollsToGetTwenty g = go 0 0 g
>   where
>       go :: Int -> Int -> StdGen -> Int
>       go sum count gen
>           | sum >= 20 = count
>           | otherwise =
>               let (die, nextGen) = randomR (1, 6) gen
>               in go (sum + die) (count + 1) nextGen

Exercises: Roll Your Own

1. Refactor rollsToGetTwenty into having the limit be a function argument.

> rollsToGetN :: Int -> StdGen -> Int
> rollsToGetN n = go 0 0
>   where
>       go :: Int -> Int -> StdGen -> Int
>       go sum count gen
>           | sum >= n = count
>           | otherwise =
>               let (die, nextGen) = randomR (1, 6) gen
>               in go (sum + die) (count + 1) nextGen

2. Change rollsToGetN to recording the series of die that occurred in addition to the count.

> rollsCountLogged :: Int -> StdGen -> (Int, [Die])
> rollsCountLogged n = go 0 []
>   where
>       go :: Int -> [Die] -> StdGen -> (Int, [Die])
>       go sum dice gen
>           | sum >= n = (length dice, dice)
>           | otherwise =
>               let (die, nextGen) = randomR (1, 6) gen
>               in go (sum + die) (intToDie die : dice) nextGen

Write State for yourself

> newtype Moi s a = Moi { runMoi :: s -> (a, s) }
>
> instance Functor (Moi s) where
>   fmap :: (a -> b) -> Moi s a -> Moi s b
>   fmap f (Moi g) = Moi $ \s -> let (a, ns) = g s
>                                in (f a, ns)
>
> instance Applicative (Moi s) where
>   pure :: a -> Moi s a
>   pure a = Moi $ \s -> (a, s)
>
>   (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
>   (Moi f) <*> (Moi g) = Moi $ \s -> let (fun, ns) = f s
>                                         (arg, fs) = g ns
>                                     in (fun arg, fs)
>
> join :: Moi s (Moi s a) -> Moi s a
> join (Moi f) = Moi $ \s -> let (Moi g, ns) = f s
>                            in g ns
>
> instance Monad (Moi s) where
>   return = pure
>
>   (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
>   (Moi f) >>= g = Moi $ \s -> let (a, ns) = f s
>                               in runMoi (g a) ns

Fizzbuzz Differently

> fizzBuzz :: Integer -> String
> fizzBuzz n
>   | n `mod` 15 == 0 = "FizzBuzz"
>   | n `mod` 5 == 0 = "Buzz"
>   | n `mod` 3 == 0 = "Fizz"
>   | otherwise = show n
>
> addResult :: Integer -> S.State [String] ()
> addResult n = do
>   xs <- S.get
>   let result = fizzBuzz n
>   S.put (result : xs)
>
> fizzbuzzFromTo :: Integer -> Integer -> [String]
> fizzbuzzFromTo from to = S.execState (mapM_ addResult [to,pred to..from]) []
>
> main :: IO ()
> main = mapM_ putStrLn $ fizzbuzzFromTo 1 100

Chapter Exercises

1. Construct a State where the state is also the value you return.

> get :: Moi s s
> get = Moi $ \s -> (s, s)

2. Construct a State where the resulting state is the argument
provided and the value is defaulted to unit.

> put :: s -> Moi s ()
> put s = Moi $ \_ -> ((), s)

3. Run the State with s and get the state that results.

> exec :: Moi s a -> s -> s
> exec (Moi sa) = snd . sa

4. Run the State with s and get the value that results.

> eval :: Moi s a -> s -> a
> eval (Moi sa) = fst . sa

5. Write a function which applies a function to create a new State.

> modify :: (s -> s) -> Moi s ()
> modify f = Moi $ \s -> ((), f s)
