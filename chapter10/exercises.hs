{-| Warm-up and review -}
-- 1.
stops = "pbtdkg"
vowels = "aeiou"
-- a)
svs :: [(Char, Char, Char)]
svs = [ (a, b, c) | a <- stops, b <- vowels, c <- stops ]
-- b)
svsb = [ (a, b, c) | a <- take 1 stops, b <- vowels, c <- stops]
-- c)
nouns = ["dog", "cat", "hat"]
verbs = ["pets", "bites", "eats"]
nvn = [ (a, b, c) | a <- nouns, b <- verbs, c <- nouns ]
-- 2. gets the average word length of a sentence
-- 3.
seekritFunc x = div (sum (map length (words x))) (length (words x))
seekritFunc' x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

{-| Rewriting functions using folds -}
-- 1.
myOr :: [Bool] -> Bool
myOr = foldr (||) False
-- 2.
myAny f = foldr (\a b -> f a || b) False
-- 3.
myElemFold :: Eq a => a -> [a] -> Bool
myElemFold test = foldr (\a b -> a == test || b) False
myElemAny :: Eq a => a -> [a] -> Bool
myElemAny test = any (==test)
-- 4.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []
-- 5.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []
-- 6.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []
-- 7.
squish :: [[a]] -> [a]
squish = foldr (++) []
-- 8.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []
-- 9.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
-- 10.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [] = error "the list is empty you fool"
myMaximumBy f (a:as) = foldr (\a' b -> if f a' b == GT then a' else b) a as
-- 11.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [] = error "the list is empty you fool"
myMinimumBy f (a:as) = foldr (\a' b -> if f a' b == LT then a' else b) a as
