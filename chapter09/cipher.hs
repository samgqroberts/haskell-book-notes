module Caesar where

import Data.Char

(|>) :: a -> (a -> b) -> b
(|>) a f = f a

-- (|>) :: (a -> b) -> (b -> c) -> (a -> c)
-- (|>) = flip (.)

(>>>>) = flip (.)


caesar :: Int -> String -> String
caesar shift = map $ chr . (+) (ord 'a') . flip mod 26 . (+) (shift - ord 'a') . ord

caesar' shift = map (\x -> x |> ord |> (+) (shift - ord 'a') |> flip mod 26 |> (+) (ord 'a') |> chr)
caesar'' shift = map $ ord >>>> (+) (shift - ord 'a') >>>> flip mod 26 >>>> (+) (ord 'a') >>>> chr
caesar''' shift = map (chr ((+) (ord 'a') $ flip mod 26 $ (+) (shift - ord 'a') $ ord))

unCaesar :: Int -> String -> String
unCaesar shift = map $ chr . (+) (ord 'a') . flip mod 26 . (+) ((-shift) - ord 'a') . ord