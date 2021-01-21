import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower)

{-| Modifying code -}

-- 2.
palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case ((fmap toLower line1) == (fmap toLower $ reverse line1)) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess