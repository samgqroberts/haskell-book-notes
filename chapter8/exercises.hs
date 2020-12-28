{-| Review of types -}
-- 1. (d)
-- 2. (b)
-- 3. (d)
-- 4. (b)

{-| Reviewing currying -}
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny
appedCatty = cattyConny "woops"
frappe = flippy "haha"

-- 1. String -> String, f y = "woohoo! mrow " ++ y
-- 2. "1 mrow haha"
-- 3. "woops mrow 2 mrow haha"
-- 4. "woops mrow blue mrow haha"
-- 5. "pink mrow haha mrow green mrow woops mrow blue" 
-- 6. "are mrow Pugs mrow awesome"

{-| Recursione -}
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise =
            go (n - d) d (count + 1)
-- 1.
-- go 15 2 0
-- go (15 - 2) 2 (0 + 1)
-- go 13 2 1
-- go (13 - 2) 2 (1 + 1)
-- go 11 2 2
-- go (11 - 2) 2 (2 + 1)
-- go 9 2 3
-- go (9 - 2) 2 (3 + 1)
-- go 7 2 4
-- go (7 - 2) 2 (4 + 1)
-- go 5 2 5
-- go (5 - 2) 2 (5 + 1)
-- go 3 2 6
-- go (3 - 2) 2 (6 + 1)
-- go 1 2 7
-- (7, 1)

-- 2.
sumNums :: (Eq a, Num a) => a -> a
sumNums 0 = 0
sumNums n = n + sumNums (n - 1)

-- 3.
multByRec :: (Integral a) => a -> a -> a
multByRec x 1 = x
multByRec x y = x + multByRec x (y - 1)

{-| Fixing dividedBy -}
data DividedResult a
 = Result (a, a)
 | DividedByZero
 deriving (Show)
dividedBy' :: Integral a => a -> a -> DividedResult a
dividedBy' _ 0 = DividedByZero
dividedBy' num denom =
  Result (signum num * signum denom * fst result, snd result)
  where
    result = go (abs num) (abs denom) 0
    go n d count
          | n < d = (count, n)
          | otherwise =
            go (n - d) d (count + 1)

mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ n + 11
  