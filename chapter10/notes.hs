{-| Exercises: Understanding folds -}
-- 1. b and c
-- 2.
{-|
foldl (flip (*)) 1 [1..3]
f = (flip (*))
((1 `f` 1) `f` 2) `f` 3
(1 `f` 2) `f` 3
2 `f` 3
6
-}
-- 3. c
-- 4. a
-- 5.
{-
a)
Prelude> foldr (++) "" ["woot", "WOOT", "woot"]
"wootWOOTwoot"
b)
Prelude> foldr max ' ' "fear is the little death"
't'
c)
Prelude> foldr (&&) True [False, True]
False
d)
Prelude> foldr (||) False [False, True]
True
e)
Prelude> foldl (\acc el -> acc ++ show el) "" [1..5]
"12345"
f)
Prelude> foldr const 0 [1..5]
1
g)
Prelude> foldr const 'a' "tacos"
't'
h)
Prelude> foldl (flip const) '0' "burritos"
's'
i)
Prelude> foldl (flip const) 0 [1..5]
5
-}

-- hey on page 556 they're talking about foldl vs foldl'
-- https://github.com/hasura/graphql-engine/pull/2933#discussion_r328821960

{-| Exercises: Database processing -}

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- 1.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr go []
             where go (DbDate time) = (:) time
                   go _ = id

-- 2.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr go []
               where go (DbNumber num) = (:) num
                     go _ = id

-- 3.
mostRecent :: [DatabaseItem] -> UTCTime
-- mostRecent = maximum . filterDbDate 
mostRecent = go . filterDbDate
           where go [] = error "No dates in database"
                 go (x:xs) = foldr max x xs

-- 4.
sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

-- 5.
avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sum numbers) / fromIntegral (length numbers)
         where numbers = filterDbNumber db

{-| Scans exercises -}
fibs = 1 : scanl (+) 1 fibs
{-
fibs = 1 : scanl (+) 2 fibs
fibs = 1 : [1, 1 + fibs!!0, (1 + fibs!!0) + fibs!!1, (1 + fibs!!0 + fibs!!1) + fibs!!2, ...]
fibs = 1 : [1, 1 + 1, ]

scanl                   :: (b -> a -> b) -> b -> [a] -> [b]
scanl                   = scanlGo
  where
    scanlGo           :: (b -> a -> b) -> b -> [a] -> [b]
    scanlGo f q ls    = q : (case ls of
                               []   -> []
                               x:xs -> scanlGo f (f q x) xs)

    :
   / \
  1   (scanlGo (+) 2 fibs)
     /  \
    2    (scanlGo (+) ((+) 2 fibs!!0) (drop 1 fibs))
         / \
        3   (scanlGo ((+) 2 fibs!!0 + fibs!!1) (drop 2 fibs))
            /
           5
           
    
-}
-- 1.
fibsFirst20 = 1 : scanl (+) 1 (take 18 fibs)
-- 2.
fibsLessThan100 = takeWhile (<100) $ 1 : scanl (+) 1 fibs
-- 3.
factorial = scanl (*) 1 [1..]
factorialN n = factorial!!n

