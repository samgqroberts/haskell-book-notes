{-# LANGUAGE ScopedTypeVariables #-}

import Data.Monoid
import Data.Semigroup
import Test.QuickCheck

{-| Semigroup exercises -}

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- 1.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

checkTrivial :: IO ()
checkTrivial = quickCheck (semigroupAssoc :: TrivAssoc)

-- 2.

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do Identity <$> arbitrary

type IdenAssoc a = Identity a -> Identity a -> Identity a -> Bool

checkIdentity :: IO ()
checkIdentity = quickCheck (semigroupAssoc :: IdenAssoc String)

-- 3.

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

-- superclass context is what's before the "fat right arrow" - ian
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

checkTwo :: IO ()
checkTwo = quickCheck (semigroupAssoc :: TwoAssoc String String)

-- 4.

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

checkThree :: IO ()
checkThree = quickCheck (semigroupAssoc :: ThreeAssoc String String String)

-- 5.

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

checkFour :: IO ()
checkFour = quickCheck (semigroupAssoc :: FourAssoc String String String String)

-- 6.

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj x') = BoolConj (x && x')

instance Arbitrary BoolConj where
  arbitrary = do BoolConj <$> arbitrary

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

checkBoolConj :: IO ()
checkBoolConj = quickCheck (semigroupAssoc :: BoolConjAssoc)

-- 7.

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj x') = BoolDisj (x || x')

instance Arbitrary BoolDisj where
  arbitrary = do BoolDisj <$> arbitrary

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

checkBoolDisj :: IO ()
checkBoolDisj = quickCheck (semigroupAssoc :: BoolDisjAssoc)

-- 8.

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst a) <> (Fst a') = Fst a'
  (Fst a) <> (Snd b) = Snd b
  (Snd b) <> (Fst a) = Snd b
  (Snd b) <> (Snd b') = Snd b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (Fst a)), (1, return (Snd b))]

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

checkOr :: IO ()
checkOr = quickCheck (semigroupAssoc :: OrAssoc String String)

-- 9.

-- DONT KNOW THIS ONE
-- it says "The type of functions should already have an Arbitrary instance that you can reuse for testing this instance."
--   but i have no clue how to generate an arbitrary fn

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\a -> f a <> g a)

instance (Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    -- (f :: a -> b) <- arbitrary
    b <- arbitrary
    let f = \x -> b
    return (Combine f)

type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Bool

combineAssociation :: Combine a b -> Combine a b -> Combine a b -> Bool
combineAssociation a c1 c2 c3 = (a $ unCombine (c1 <> (c2 <> c3))) == (a $ unCombine ((c1 <> c2) <> c3))

checkCombine :: IO ()
checkCombine = quickCheck (semigroupAssoc' :: CombineAssoc String String)
