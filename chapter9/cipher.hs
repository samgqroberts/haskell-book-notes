module Caesar where

import Data.Char

caesar :: Int -> String -> String
caesar shift = map $ chr . (+) (ord 'a') . flip mod 26 . (+) (shift - ord 'a') . ord

unCaesar :: Int -> String -> String
unCaesar shift = map $ chr . (+) (ord 'a') . flip mod 26 . (+) ((-shift) - ord 'a') . ord