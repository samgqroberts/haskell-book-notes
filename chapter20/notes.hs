import Data.Monoid (Sum(..), Product(..), Any(..))
import Data.Maybe (isNothing)

-- have to remember, Monoids talk about the operation,
--   and about combining things and preserving structure
--   (... haven't revisited the book in a while)

{-| Exercises: Library functions -}
{-| Implement the functions in terms of foldMap or foldr from Foldable, 
    then try them out with multiple types that have Foldable instances. -}

-- 1.
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-- 2.
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

-- 3.
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' el = getAny . foldMap (\x -> Any (x == el))

-- 4.
-- could I use something fancier than pattern matching for `go`?
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr go Nothing
           where go :: (Ord a) => a -> Maybe a -> Maybe a
                 go el Nothing = Just el
                 go el (Just acc) = Just (if el < acc then el else acc)

-- 5.
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr go Nothing
           where go :: (Ord a) => a -> Maybe a -> Maybe a
                 go el Nothing = Just el
                 go el (Just acc) = Just (if el > acc then el else acc)

-- 6.
null' :: (Foldable t) => t a -> Bool
null' = isNothing . foldr go Nothing
        where go el _ = Just el


-- 7.
length' :: (Foldable t) => t a -> Int
length' = foldr go 0
          where go _ acc = acc + 1

-- 8.
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- 9.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- 10.
foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap'' f = foldr (\el acc -> f el `mappend` acc) mempty