import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{-| specialize types -}

-- 1.

p1 = pure :: a -> [a]
a1 = (<*>) :: [a -> b] -> [a] -> [b]

-- 2.

p2 = pure :: a -> IO a
a2 = (<*>) :: IO (a -> b) -> IO a -> IO b

-- 3.

p3 :: Monoid a => b -> (,) a b
p3 = pure

a3 :: Monoid a => (,) a (b -> c) -> (,) a b -> (,) a c
a3 = (<*>)

-- 4.

p4 :: a -> (->) e a
p4 = pure

a4 :: (->) e (a -> b) -> (->) e a -> (->) e b
a4 = (<*>)

{-| write applicative instances -}

-- 1.

data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

pairCheck = quickBatch $ applicative (Pair ("a", "b", 1 :: Integer) ("c", "d", 2 :: Integer))

-- 2.

data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  (<*>) (Two a f) (Two a' b) = Two (a <> a') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

twoCheck = quickBatch $ applicative (undefined :: Two String (String, String, Integer))

-- 3.

data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (<*>) (Three a b f) (Three a' b' c) = Three (a <> a') (b <> b') (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

threeCheck = quickBatch $ applicative (undefined :: Three String String (String, String, Integer))

-- 4.

data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a f f') (Three' a' b b') = Three' (a <> a') (f b) (f' b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

three'Check = quickBatch $ applicative (undefined :: Three' String (String, String, Integer))

-- 5.

data Four a b c d = Four a b c d deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  (<*>) (Four a b c f) (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

fourCheck = quickBatch $ applicative (undefined :: Four String String String (String, String, Integer))

-- 6.

data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Monoid a) => Applicative (Four' a) where
  pure b = Four' mempty mempty mempty b
  (<*>) (Four' a1 a2 a3 f) (Four' a1' a2' a3' b) = Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

four'Check = quickBatch $ applicative (undefined :: Four' String (String, String, Integer))

{-| Combinations -}

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)