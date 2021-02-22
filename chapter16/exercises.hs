{-# LANGUAGE FlexibleInstances #-}

{-| Chapter exercises -}

-- 1. no, because it's not a higher-kinded type

-- 2. yes

-- 3. yes

-- 4. i don't think so, because f is * -> *, making Mu * -> (* -> *)

-- 5. no because it's not a higher-kinded type

{-| rearrange so Functor instance works -}

-- 1.

data Sum b a = First a | Second b

-- 2.

data Company a c b = DeepBlue a c | Something b

-- 3.

data More b a = L a b a | R b a b

{-| Write Functor instances for the following datatypes -}

-- 1.

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2.

data K a b = K a -- is Const

instance Functor (K a) where
  fmap f (K a) = K a

-- 3.

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

-- newtype K a b = K a (defined earlier)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))

-- 4.

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

-- 5.

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 6.

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7.

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa $ fmap f gb

-- 8.

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga $ fmap f gt

-- 9.

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) $ fmap f as

-- 10.

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats gla gla' gla'') = MoreGoats (fmap f gla) (fmap f gla') (fmap f gla'')

-- 11.

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s $ f a
  fmap f (Read stoa) = Read $ f . stoa