import Data.Bool
{-| Exercise: EnumFromTo -}

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True = [False, True]
eftBool True False = []
eftBool True True = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd GT GT = [GT]
eftOrd GT _ = []
eftOrd EQ GT = [EQ, GT]
eftOrd EQ EQ = [EQ]
eftOrd EQ _ = []
eftOrd LT GT = [LT, EQ, GT]
eftOrd LT EQ = [LT, EQ]
eftOrd LT LT = [LT]

eftInt :: Int -> Int -> [Int]
eftInt from to
  | from < to = from : eftInt (succ from) to
  | from == to = [from]
  | from > to = []

eftChar :: Char -> Char -> [Char]
eftChar from to
  | from < to = from : eftChar (succ from) to
  | from == to = [from]
  | from > to = []

{-| Exercises: Thy fearful symmetry -}
-- 1.
myWords :: String -> [String]
myWords "" = []
myWords (' ' : s) = myWords s
myWords s = takeWhile (/= ' ') s : myWords (dropWhile (/= ' ') s)

-- how would i turn the above into point-free style?

-- 2. in module PoemLines
-- 3. in module PoemLines

{-| Exercises: Comprehend thy lists -}

{-| Exercises: Square cube -}
{-|
*PoemLines> mySqr = [x^2 | x <- [1..5]]
*PoemLines> mySqr
[1,4,9,16,25]
*PoemLines> myCube = [y^3 | y <- [1..5]]
*PoemLines> myCube
[1,8,27,64,125]
*PoemLines> [(x, y) | x <- mySqr, y <- myCube]
[(1,1),(1,8),(1,27),(1,64),(1,125),(4,1),(4,8),(4,27),(4,64),(4,125),(9,1),(9,8),(9,27),(9,64),(9,125),(16,1),(16,8),(16,27),(16,64),(16,125),(25,1),(25,8),(25,27),(25,64),(25,125)]
*PoemLines> [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
[(1,1),(1,8),(1,27),(4,1),(4,8),(4,27),(9,1),(9,8),(9,27),(16,1),(16,8),(16,27),(25,1),(25,8),(25,27)]
*PoemLines> length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
15
-}

{-| Exercises: Bottom madness -}
-- 1. ⊥
-- 2. value
-- 3. ⊥
-- 4. value
-- 5. ⊥
-- 6. value
-- 7. ⊥
-- 8. value
-- 9. value
-- 10. ⊥

{-| Intermission: Is it in normal form? -}
-- 1. NF
-- 2. WHNF
-- 3. Neither
-- 4. Neither
-- 5. Neither
-- 6. Neither
-- 7. WHNF

{-| Exercises: More bottoms -}
-- 1. ⊥
-- 2. value
-- 3. ⊥
-- 4. takes a string and returns a list where each
--      character in the string is mapped to either
--      True if it's a lowercase vowel, False if not
-- 5. a) [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
--    b) [1, 10, 20]
--    c) [15, 15, 15]
-- 6.
invert3 = map (\x -> bool x (-x) (x == 3))

{-| Exercises: Filtering -}
-- 1.
-- *Main Data.Bool> filter (\x -> rem x 3 == 0) [1..30]
-- [3,6,9,12,15,18,21,24,27,30]
-- 2.
-- *Main Data.Bool> length . filter (\x -> rem x 3 == 0) $ [1..30]
-- 10
-- 3.
-- *Main Data.Bool> myFilter = filter (not . (flip elem $ ["the", "a", "an"])) . words
-- *Main Data.Bool> myFilter "the brown dog was a goof"
-- ["brown","dog","was","goof"]

{-| Zipping exercises -}
-- 1.
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a, b) : zip' as bs
-- 2.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs
