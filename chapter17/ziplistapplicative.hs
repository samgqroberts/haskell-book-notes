import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take 3000 l
          ys' = let (ZipList' l) = ys
                in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = ZipList' . repeat
  (<*>) (ZipList' []) (ZipList' _) = ZipList' []
  (<*>) (ZipList' _) (ZipList' []) = ZipList' []
  -- (<*>) (ZipList' (f:fs)) (ZipList' (a:as)) = ZipList' $ f a : tail
  --                                           where (ZipList' tail) = ZipList' fs <*> ZipList' as
  (<*>) (ZipList' fs) (ZipList' as) = ZipList' $ zipWith ($) fs as
            
instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> liftArbitrary arbitrary

main = quickBatch $ applicative (ZipList' [("b", "w", 1 :: Integer)])