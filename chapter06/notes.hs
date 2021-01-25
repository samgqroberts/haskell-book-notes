{-|
  Exercises: Eq instances
-}

-- 1.
data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn i) (TisAn i') = i == i'

-- 2.
data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two i1 i2) (Two i1' i2') = i1 == i1' && i2 == i2'

-- 3.
data StringOrInt =
  TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt i) (TisAnInt i') = i == i'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _ _ = False

-- 4.
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair v1 v2) (Pair v1' v2') = v1 == v1' && v2 == v2'

-- 5.
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

-- 6.
data Which a =
  ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

-- 7.
data EitherOr a b =
  Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False

-- from a type class declaration, how do you know which methods are
--   required to provide an instance impl for?

{-| Exercises: Will they work?
  1. 5
  2. LT
  3. type error
  4. False
-}