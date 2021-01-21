import Data.List

{-| Determine the kinds
1. *
2. a :: *, f :: * -> *
-}

{-| String processing -}
-- 1.
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

replaceThe :: String -> String
replaceThe = unwords . fmap (maybe "a" id . notThe) . words

-- 2.
vowels = "aeiou"
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go (words s) False
                        where go [] _ = 0
                              go (x:xs) True
                                | head x `elem` vowels = 1 + go xs False
                                | otherwise = go xs (x == "the")
                              go (x:xs) False  = go xs (x == "the")

-- 3. 
countVowels = length . filter (`elem` vowels)

{-| Validate the word -}
newtype Word' = Word' String deriving (Eq, Show)
mkWord :: String -> Maybe Word'
mkWord s
  | numV > numC = Nothing
  | otherwise = Just (Word' s)
  where numV = countVowels s
        numC = countConsonants s
        countConsonants = length . filter (`notElem` vowels)

{-| It's only natural -}
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x == 0 = Just Zero
  | x < 0 = Nothing
  | otherwise = go (integerToNat (x - 1))
  where go Nothing = Nothing
        go (Just Zero) = Just (Succ Zero)
        go (Just (Succ y)) = Just (Succ (Succ y))

-- better version, using fmap!
integerToNat' :: Integer -> Maybe Nat
integerToNat' x
  | x == 0 = Just Zero
  | x < 0 = Nothing
  | otherwise = fmap Succ (integerToNat (x - 1))

{-| Small library for Maybe -}
-- 1.
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

-- 2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just x) = f x
mayybee def _ Nothing = def

-- 3.
fromMaybe :: a -> Maybe a -> a
fromMaybe def = mayybee def id

-- 4.
listToMaybe :: [a] -> Maybe a
listToMaybe (x:xs) = Just x
listToMaybe _ = Nothing

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5.
catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList

-- 6.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr go (Just []) 
          where go :: Maybe a -> Maybe [a] -> Maybe [a]
                go Nothing _ = Nothing
                go (Just a) b = fmap (a:) b

{-| Small library for Either -}
-- 1.
lefts' :: [Either a b] -> [a]
lefts' = foldr go []
       where go (Left a) as = a : as
             go _ as = as

-- 2.
rights' :: [Either a b] -> [b]
rights' = foldr go []
        where go (Right b) bs = b : bs
              go _ bs = bs

-- 3.
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr go ([], [])
                  where go (Left a) (as, bs) = (a : as, bs)
                        go (Right b) (as, bs) = (as, b : bs)

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' _ _ = Nothing

-- 5.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b

-- 6.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

{-| Write your own iterate and unfoldr -}
-- 1.
myIterate :: (a -> a) -> a -> [a]
myIterate f a = [a] ++ myIterate f (f a)

-- 2.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = maybe [] (\(b', b'') -> [b'] ++ myUnfoldr f b'') (f b)

-- 3.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x

{-| Finally something other than a list! -}
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

-- 1.
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = maybe Leaf (\(l, b, r) -> Node (unfold f l) b (unfold f r)) (f x)

-- 2.
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold go 0
  where go i
          | i < n = Just (i + 1, i, i + 1)
          | otherwise = Nothing
