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
a = fmap (+1) $ read "[1]" :: [Int]

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
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- 1. 
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do Identity <$> arbitrary

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

checkIdentityFunctorIdentity = quickCheck (functorIdentity :: Identity Int -> Bool)
checkIdentityFunctorCompose = quickCheck (functorCompose (+1) (*2) :: Identity Int -> Bool)

-- 2.
data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return (Pair a a')

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

checkPairFunctorIdentity = quickCheck (functorIdentity :: Pair Int -> Bool)
checkPairFunctorCompose = quickCheck (functorCompose (+1) (*2) :: Pair Int -> Bool)

-- 3.
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

checkTwoFunctorIdentity = quickCheck (functorIdentity :: Two Char Int -> Bool)
checkTwoFunctorCompose = quickCheck (functorCompose (+1) (*2) :: Two Char Int -> Bool)