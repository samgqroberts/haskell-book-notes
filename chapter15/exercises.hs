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
-- BECAUSE IT IS POPPYCOCK
-- it says "The type of functions should already have an Arbitrary instance that you can reuse for testing this instance."
--   but i have no clue how to generate an arbitrary fn

-- newtype Combine a b = Combine { unCombine :: a -> b }

-- instance Semigroup b => Semigroup (Combine a b) where
--   (Combine f) <> (Combine g) = Combine (\a -> f a <> g a)

-- instance (Arbitrary b) => Arbitrary (Combine a b) where
--   arbitrary = do
--     -- (f :: a -> b) <- arbitrary
--     b <- arbitrary
--     let f = const b
--     return (Combine f)

-- type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Bool

-- instance Show (Combine a b) where
--   show (Combine _) = "Combine a b"

-- instance Eq (Combine a b) where
--   (==) (Combine x) (Combine y) = x == y

-- -- combineAssociation :: Combine a b -> Combine a b -> Combine a b -> Bool
-- -- combineAssociation c1 c2 c3 = (a $ unCombine (c1 <> (c2 <> c3))) == (a $ unCombine ((c1 <> c2) <> c3))

-- checkCombine :: IO ()
-- checkCombine = quickCheck (semigroupAssoc :: CombineAssoc String String)

-- f = Combine $ \n -> Sum (n + 1)
-- g = Combine $ \n -> Sum (n - 1)
-- h = unCombine (f <> g) $ 0

-- 10. also skipping...

-- 11. 

data Validation a b =
  Failure' a | Success' b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Success' b) (Success' b') = Success' b
  (<>) (Success' b) (Failure' a) = Success' b
  (<>) (Failure' a) (Success' b) = Success' b
  (<>) (Failure' a) (Failure' a') = Failure' (a <> a')

validationMain = do
  let failure :: String -> Validation String Int
      failure = Failure'
      success :: Int -> Validation String Int
      success = Success'
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (Failure' a)), (1, return (Success' b))]
  
type ValidationAssoc a b = Validation a b -> Validation a b -> Validation a b -> Bool

checkValidation = quickCheck (semigroupAssoc :: ValidationAssoc String Int)

{-| Monoid exercises -}

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a
monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- 1.

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

checkTrivial' = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mlr :: Trivial -> Bool)

-- 2.

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

checkIdentity' = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: IdenAssoc String)
  quickCheck (mli :: Identity String -> Bool)
  quickCheck (mlr :: Identity String -> Bool)

-- 3.

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

checkTwo' = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: TwoAssoc String String)
  quickCheck (mli :: Two String String -> Bool)
  quickCheck (mlr :: Two String String -> Bool)

-- 4.

instance Monoid BoolConj where
  mempty = BoolConj True

checkBoolConj' = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: BoolConjAssoc)
  quickCheck (mli :: BoolConj -> Bool)
  quickCheck (mlr :: BoolConj -> Bool)

-- 5.

instance Monoid BoolDisj where
  mempty = BoolDisj False

checkBoolDisj' = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: BoolDisjAssoc)
  quickCheck (mli :: BoolDisj -> Bool)
  quickCheck (mlr :: BoolDisj -> Bool)

-- 6. skipping... because it's poppycock

-- 7. also skipping...

-- 8. also skipping...