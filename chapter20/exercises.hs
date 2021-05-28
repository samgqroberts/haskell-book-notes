{-| Write Foldable instances for the following datatypes -}

-- 1.
data Constant a b = Constant b deriving (Show, Eq)

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

-- 2.
data Two a b = Two a b deriving Show

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

-- 3.
data Three a b c = Three a b c deriving Show

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

-- 4.
data Three' a b = Three' a b b deriving Show

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b'

-- 5.
data Four' a b  = Four' a b b b deriving Show

instance Foldable (Four' a) where
  foldMap f (Four' a b b' b'') = f b''

{-| Thinking cap time. Write a filter function for Foldable types using the foldMap function -}

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)