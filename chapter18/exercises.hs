import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{-| Write Monad instances for the following types -}

-- 1.

data Nope a = NopeDotJpg deriving (Show, Eq)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

checkNope = do
  let trigger :: Nope (String, Int, Char)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

-- 2.

data BahEither b a = PLeft a | PRight b deriving (Show, Eq)

instance Functor (BahEither b) where
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a) = PLeft (f a)

instance Applicative (BahEither b) where
  pure = PLeft
  (<*>) (PRight b) _ = PRight b
  (<*>) _ (PRight b) = PRight b
  (<*>) (PLeft f) (PLeft a) = PLeft $ f a

instance Monad (BahEither b) where
  (>>=) (PRight b) _ = PRight b
  (>>=) (PLeft a) f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = frequency [(1, PLeft <$> arbitrary), (1, PRight <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

checkBahEither = do
  let trigger :: BahEither String (String, Int, Char)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

-- stray thought: Applicative's are monoidal as the book says, but specifically: in the structure
--   they use the term "monoid-of-structure"
--   the monoidness comes from the properties of your implementation of <*>
--   not from subclassing Monoid.
--   in some cases, like in the tuple case, the left value needs Monoid, because that's the structure

-- 3.

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>)  = undefined

instance Monad Identity where
  return = pure
  (>>=) = undefined

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

checkIdentity = do
  let trigger :: Identity (String, Int, Char)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
