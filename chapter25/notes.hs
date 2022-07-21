{-# LANGUAGE InstanceSigs #-}

import Control.Monad (join)

newtype Identity a = Identity { runIdentity :: a } deriving (Show)

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

{-| GOTCHA! Exercise time -}

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ pure (pure a)
  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ fmap (<*>) f <*> a

{-| 25.6 Exercises: Compose instances -} 

-- 1. Write the Compose Foldable instance.

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = foldMap (\ga -> foldMap (\a -> f a) ga) fga

-- 2. Write the Compose Traverable instance.

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative h => (a -> h b) -> Compose f g a -> h (Compose f g b)
  traverse ahb = sequenceA . fmap ahb

{-| And now for something completely different -}

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- 1.

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a c) = Deux (f a) (g c)

-- 2.

data Const a b = Const a

instance Bifunctor Const where
  bimap f g (Const a) = Const (f a)

-- 3.

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

-- 4.

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f g (SuperDrei a b) = SuperDrei a (f b)

-- 5.

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap f g (SemiDrei a) = SemiDrei a

-- 6.

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

-- 7.

-- (already included in Prelude)
-- data Either a b = Left a | Right b

instance Bifunctor Either where
  bimap f g (Left a) = Left $ f a
  bimap f g (Right b) = Right $ g b

{-|
  random, but looking at the definition for join:

  join :: Monad m => m (m a) -> m a

  I'm feeling the same thing that stymied me when I was trying to write the Applicative instance for
  Compose above. The structure containing structure of it all. I tried to look back and see how much
  we had done re: that in the book so far and it looks like not much, especially in abstract
  type-land. So I think that's the leap that makes Monads tougher to visualize. Structure of structure.
  It's not two structures, it's nested structure. So that's where we need to do stuff like
  fmap a <*> into the top structure...
-}

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (>>=) :: IdentityT m a
        -> (a -> IdentityT m b)
        -> IdentityT m b
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f


{-|
  So the "extra type information" we needed and got from nailing down one of the
  Monad instances in the composition of Monads, here IdentityT, is NOT just the pattern match
  (IdentityT ma) in the bind definition, like I thought.
  It's the *fold* that `runIdentityT` affords us, to address the `f (g (f b))` problem.
-}