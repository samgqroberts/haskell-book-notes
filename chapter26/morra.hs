module Main where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans
import System.Random
import Text.Read

data MorraState = MorraState {
    computerPoints :: Int,
    humanPoints :: Int
} deriving (Show)

initialState :: MorraState
initialState = MorraState 0 0

parseInput :: Int -> Int -> String -> Maybe Int
parseInput min max =
    maybe Nothing f . readMaybe
  where
    f i = if i >= min && i <= max
        then Just i
        else Nothing

morraRound :: StateT MorraState IO ()
morraRound = StateT $ \s -> do
    putStrLn "Choose 1-5 to throw"
    userThrowInput <- getLine
    case parseInput 1 5 userThrowInput of
        Nothing -> do
            putStrLn "Bad input."
            return ((), s)
        Just userThrow -> do
            putStrLn "Choose 2-10 to guess"
            userGuessInput <- getLine
            case parseInput 2 10 userGuessInput of
                Nothing -> do
                    putStrLn "Bad input."
                    return ((), s)
                Just userGuess -> do
                    computerThrow <- randomRIO (1, 5) :: IO Int
                    computerGuess <- randomRIO (2, 10) :: IO Int
                    let
                        actualSum = userThrow + computerThrow
                    putStrLn $ "You threw        : " <> show userThrow
                    putStrLn $ "Computer threw   : " <> show computerThrow
                    putStrLn $ "Actual sum       : " <> show actualSum
                    putStrLn $ "You guessed      : " <> show userGuess
                    putStrLn $ "Computer guessed : " <> show computerGuess
                    return ((), s)

main :: IO ()
main = do
    let
        roundResultIO :: IO ((), MorraState)
        roundResultIO = runStateT morraRound $ MorraState 0 0
    roundResult <- roundResultIO
    let
        state = snd roundResult
    putStrLn $ show state
