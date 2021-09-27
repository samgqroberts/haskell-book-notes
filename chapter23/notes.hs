{-# LANGUAGE InstanceSigs #-}

import System.Random
import Control.Monad
import Control.Monad.Trans.State

{-| Exercises: Roll your own -}
data Die =
  DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  -- Use error sparingly
  x ->
    error $
    "intToDie got non 1-6 integer: "
    ++ show x

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) =
              randomR (1, 6) gen
        in go (sum + die)
              (count + 1) nextGen

-- 1. Refactor rollsToGetTwenty so that the limit is an argument to the function:

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN l g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= l = count
      | otherwise =
        let (die, nextGen) =
              randomR (1, 6) gen
        in go (sum + die)
              (count + 1) nextGen

-- 2. Change rollsToGetN to record the series of dice that are rolled,
--    in addition to the count of the total number of rolls:

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged l g = go [] g
  where
    go :: [Int] -> StdGen -> (Int, [Die])
    go rolls gen
      | sum rolls >= l = (length rolls, intToDie <$> rolls)
      | otherwise =
        let (die, nextGen) =
              randomR (1, 6) gen
        in go (rolls ++ [die]) nextGen

{-| 23.6 Write State for yourself -}

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ (\t -> (f (fst t), snd t)) . g

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) =
    -- f :: s -> (a -> b, s)
    -- g :: s -> (a, s)
    Moi (\s ->
      let t = f s
          t' = g $ snd t
      in
          (fst t $ fst t', snd t'))
  
instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g =
    -- f :: s -> (a, s)
    Moi (\s ->
      let fout = f s
          gout = g $ fst fout
      in
          runMoi gout $ snd fout)

{-| FizzBuzz differently -}

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 3 == 0 = "Fizz"
           | otherwise = show n

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

-- for reference
fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo start stop =
  let list = enumFromThenTo stop (stop - 1) start
  in
    execState (mapM_ addResult list) []