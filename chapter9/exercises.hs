import Data.Char

{-| Data.Char -}
-- 1.
-- *Main Data.Bool Data.Char> :t isUpper
-- isUpper :: Char -> Bool
-- *Main Data.Bool Data.Char> :t toUpper
-- toUpper :: Char -> Char

-- 2.
onlyUpper :: String -> String
onlyUpper = filter isUpper

-- 3.
capitalize :: String -> String
capitalize "" = ""
capitalize (h:s) = toUpper h : s

-- 4. 
capitalizeAll :: String -> String
capitalizeAll "" = ""
capitalizeAll (h:s) = toUpper h : capitalizeAll s

-- 5.
firstAsCapital :: String -> Char
firstAsCapital = toUpper . head

-- 6. lol did it the first time

{-| Writing your own standard functions -}
-- 1. 
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

-- 2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny pred (x:xs) = if pred x then True else myAny pred xs

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = if e == x then True else myElem e xs

myElem' e = any (== e)

-- 4.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 5.
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- 6.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (a:as) = f a ++ squishMap f as

-- 7.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comp as = go as (as!!0)
                    where go [] cur = cur
                          go (a:as') cur = if (comp a cur) == GT then go as' a else go as' cur 

-- 9.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comp as = go as (as!!0)
                    where go [] cur = cur
                          go (a:as') cur = if (comp a cur) == LT then go as' a else go as' cur 

-- 10.
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
