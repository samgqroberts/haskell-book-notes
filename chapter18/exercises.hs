import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Hspec

{-| Write Monad instances for the following types.
   Use the QuickCheck properties we showed you to validate your instances -}

main :: IO ()
main = do
  nopeTests
  bahEitherTests
  identityTests
  listTests
  jTests

-- 1.

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure x = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  (>>=) _ _ = NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

nopeTests = do
  putStrLn "\nTesting Nope"
  let trigger :: Nope (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
-- 2.

data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a) = PLeft $ f a

instance Applicative (BahEither b) where
  pure = PLeft
  (<*>) (PLeft f) (PLeft a) = PLeft $ f a
  (<*>) (PRight b) _ = PRight b
  (<*>) _ (PRight b) = PRight b

instance Monad (BahEither b) where
  (>>=) (PLeft a) f = f a
  (>>=) (PRight b) _ = PRight b

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

instance (Arbitrary b, Arbitrary a) => Arbitrary (BahEither b a) where
  arbitrary = frequency [(1, PLeft <$> arbitrary), (1, PRight <$> arbitrary)]

bahEitherTests = do
  putStrLn "\nTesting BahEither"
  let trigger :: BahEither String (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

-- 3.

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

instance Monad Identity where
  (>>=) (Identity a) f = f a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

identityTests = do
  putStrLn "\nTesting Identity"
  let trigger :: Identity (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

-- 4.

data List a = Nil | Cons a (List a) deriving (Eq, Show)

lcons :: a -> List a -> List a
lcons a Nil = Cons a Nil
lcons a (Cons a' as) = Cons a $ Cons a' as

instance Semigroup (List a) where
  (<>) Nil as = as
  (<>) as Nil = as
  (<>) (Cons a as) (Cons a' as') = Cons a $ as <> (Cons a' as')

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) (Cons a as) = Cons (f a) $ (fmap f as) <> (fs <*> Cons a as)

instance Monad List where
  (>>=) Nil _ = Nil
  (>>=) (Cons a as) f = f a <> (as >>= f)

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (1, Cons <$> arbitrary <*> arbitrary)]

listTests = do
  putStrLn "\nTesting List"
  let trigger :: List (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

{-| Write the following functions using the methods provided by Monad and Functor.
    Using stuff like identity and composition is fine, but is has to type check
    with the types provided. -}

-- 1.
j :: Monad m => m (m a) -> m a
j m = m >>= id 

jTests :: IO ()
jTests = hspec $ do
  describe "j" $ do
    it "j [[1, 2], [], [3]] == [1, 2, 3]" $ do
      j [[1, 2], [], [3]] `shouldBe` [1, 2, 3]
    it "j (Just (Just 1)) == Just 1" $ do
      j (Just (Just 1)) `shouldBe` Just 1
    it "j (Just Nothing) == Nothing" $ do
      j (Just (Nothing :: Maybe Int)) `shouldBe` (Nothing :: Maybe Int)
    it "j Nothing == Nothing" $ do
      j (Nothing :: Maybe (Maybe Int)) `shouldBe` (Nothing :: Maybe Int)

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = fmap f m

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma <*> mb

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma

-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return [] 
meh (a:as) f = (:) <$> (f a) <*> meh as f

-- 6. 
flipType :: (Monad m) => [m a] -> m [a]
flipType ms = meh ms id
