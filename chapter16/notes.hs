{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck

{-| Exercises: Be kind
1. *
2. * -> *
3. * -> * -> *
-}

-- would we say "data type X forms a functor under F?"
-- like "List forms a functor under concatenation"

{-| Exercises: Heavy lifting -}
-- 1.
-- a = (+1) $ read "[1]" :: [Int]
a = (+1) <$> read "[1]" :: [Int]

-- 2.
-- b = (++ "lol") (Just ["Hi,", "Hello"])
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.
-- c = (*2) (\x -> x - 2)
c = fmap (*2) (\x -> x - 2)

-- 4.
-- d = ((return '1' ++) . show) (\x -> [x, 1..3])
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- 5.
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed

-- okay so the positioning of arguments to type constructors is
--   what makes tuple and Either functors act on the right value
--   b/c we have to create a * -> * from their * -> * -> * by applying
--   the leftmost type, leaving the right type (the innermost) to be
--   functor'd on

{-| Exercises: Instances of Func -}

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

-- 1. 
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

checkIdentityFunctorIdentity = quickCheck (functorIdentity :: Identity Int -> Bool)
checkIdentityFunctorCompose = quickCheck (functorCompose (+1) (*2) :: Identity Int -> Bool)

-- 2.
data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    Pair a <$> arbitrary

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

checkPairFunctorIdentity = quickCheck (functorIdentity :: Pair Int -> Bool)
checkPairFunctorCompose = quickCheck (functorCompose (+1) (*2) :: Pair Int -> Bool)

-- 3.

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    Two a <$> arbitrary

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

checkTwoFunctorIdentity = quickCheck (functorIdentity :: Two Char Int -> Bool)
checkTwoFunctorCompose = quickCheck (functorCompose (+1) (*2) :: Two Char Int -> Bool)

-- 4.

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Three a b <$> arbitrary

checkThreeFunctorIdentity = quickCheck (functorIdentity :: Three Char Int String -> Bool)
checkThreeFunctorCompose = quickCheck (functorCompose (++ "hi") (++ "bye") :: Three Char Int String -> Bool)

-- 5.

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Three' a b <$> arbitrary

checkThree'FunctorIdentity = quickCheck (functorIdentity :: Three' Char Int -> Bool)
checkThree'FunctorCompose = quickCheck (functorCompose (++ "hi") (++ "bye") :: Three' Char String -> Bool)

-- 6. 

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    Four a b c <$> arbitrary

checkFourFunctorIdentity = quickCheck (functorIdentity :: Four Int Int Int Int -> Bool)
checkFourFunctorCompose = quickCheck (functorCompose (+1) (*2) :: Four Int Int Int Int -> Bool)

-- 7.

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    Four' a a' a'' <$> arbitrary

checkFour'FunctorIdentity = quickCheck (functorIdentity :: Four' Int Int -> Bool)
checkFour'FunctorCompose = quickCheck (functorCompose (+1) (*2) :: Four' Int Int -> Bool)

-- 8. no, because it's type is a constant

{-| Exercise: Possibly -}

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap f LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

{-| Short exercise -}

-- 1.

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

-- 2. because we need to pass * -> * to the Functor instance declaration, and there's no way to get the
--    a in Sum a b to be that first *

class Functor' (f :: * -> *) where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' (Either a) where
  fmap' f (Left a) = Left a
  fmap' f (Right b) = Right $ f b

x = fmap' (+1) (Right 2)
y = fmap' (+1) (Left 2)

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)


instance Functor' (Flip Either a) where
  fmap' f (Flip (Left a)) = Flip (Left $ f a)
  fmap' f (Flip (Right b)) = Flip (Right b)

x' = fmap' (+1) (Flip (Right 2))
y' = fmap' (+1) (Flip (Left 2))

type RjAlias = Either Int String

-- z :: Flip Either 
-- z = undefined

z' :: Either Int String
z' = undefined