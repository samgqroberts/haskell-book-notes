module Buildingfns where

-- 1.
{-|
  a) (++) "Curry is awesome" "!"
  b) "Curry is awesome!" !! 4
  c) drop 9 "Curry is awesome!"
-}

-- 2.
addExclamation str = (++) str "!"
take5th str = str !! 4
drop9 = drop 9

-- 3.
thirdLetter :: String -> Char
thirdLetter x = x !! 2

--4.
letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

--5.
rvrs x = drop 9 x ++ take 4 (drop 5 x) ++ take 5 x 