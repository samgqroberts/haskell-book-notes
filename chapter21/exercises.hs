import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Hspec

{- Traversable instances
   Write a Traversable instance for the datatype provided, filling in any
   required superclasses. Use Quickcheck to validate your instances. -}

-- Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a
 
instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

identityTest = do
  let trigger :: Identity (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

-- Constant
-- tuff!

-- newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

-- instance Functor (Constant a) where
--   fmap f (Constant a) = Constant a
 
-- instance Foldable (Constant a) where
--   foldMap f (Constant a) = mempty
 
-- instance Traversable (Constant a) where 
--   traverse f (Constant a) = 
 

-- Maybe

data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a
 
instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a
 
instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (1, Yep <$> arbitrary)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

optionalTest = do
  let trigger :: Optional (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

-- List

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a as) = f a `mappend` foldMap f as

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f as =  foldMap  $ fmap f as