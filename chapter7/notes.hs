{-| Exercises: Grab bag
  1. all are equivalent, a b c d
  2. a
-}

-- 3a
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

-- 3b
addFive x y = (if x > y then y else x) + 5
addFiveL = \x y -> (if x > y then y else x) + 5

-- 3c
mflip f = \x -> \y -> f y x
mflipL = \f -> \x -> \y -> f y x

{-| Exercises: Variety pack
  1. a) (a, b) -> a
     b) String (no not the same)
     c) k3
  2. f (a, b, c) (d, e, f) = ((a, d), (c, f))
-}

{-| Exercises: Case practice
-}

-- 1.
functionC x y = case x > y of
  True -> x
  False -> y

-- 2.
ifEvenAdd2 n = case even n of
  True -> n + 2
  False -> n

-- 3.
nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0

{-| Exercises: Artful dodgy
-}
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2
-- 2. 11
-- 3. 22
-- 4. 21
-- 5. 12
-- 6. 11
-- 7. 21
-- 8. 21
-- 9. 22
-- 10. 31
-- 11. 23

{-| Exercises: Guard duty
  1. 
-}
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.8 = 'B'
  | y >= 0.9 = 'A'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100
-- 3. answer: (b)
pal xs
    | xs == reverse xs = True
    | otherwise = False
-- 4. [a]
-- 5. [a] -> Bool
-- 6. (c)
-- 7. (Ord, Num)
-- 8. (Ord a, Num a) => a -> Bool