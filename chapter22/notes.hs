{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
import Data.Char

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop


{-| Short Exercise: Warming up
-}

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  a <- cap
  b <- rev
  return (a, b)

-- question: is there a reason fmap x y is x . y and not y . x?
-- answer: fmap :: (a -> b) -> f a -> f b
--         where f is (->) r, so fmap :: (a -> b) -> (r -> a) -> (r -> b)
--         x . y is the only way to line up the a's.

itoa :: Integer -> [Char]
itoa 2 = "number: " ++ show 2

atom :: [Char] -> Maybe [Char]
atom = Just

{-| Exercise: Ask
-}

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b)
    -> Reader r a
    -> Reader r b
  fmap f (Reader ra) =
    Reader $ \r -> f (ra r)

ask :: Reader a a
ask = Reader id

{-| Exercise: Reading comprehension
-}

-- 1. Write liftA2 yourself. Think about it in terms of abstracting out the difference
--    between getDogR and getDogR', if that helps.

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

-- 2. Write the following function. Again, it is simpler than it looks

asks :: (r -> a) -> Reader r a
asks f = Reader f

-- 3. Implement the Applicative for Reader.

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ rab <*> ra

-- thought: Reader is an exploration in "what can we do with multiple functions that take the same type of argument"

{-| Exercise: Reader Monad
-}

-- 1. Implement the Reader Monad

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb (ra r)) r

-- thought: so the `join` for Reader doesn't do anything special, just flattens

-- remember: the fields in record types declare functions that implicitly have the type as the
--           first argument.
--   eg. in `newtype Reader r a = Reader { runReader :: r -> a }` the runReader field declares
--       a function of type `runReader :: Reader r a -> r -> a`

-- 2. Rewrite the monadic getDogRM to use your Reader datatype.

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
    , dogName :: DogName
    , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName :: DogName
    , dogsAddress :: Address
  } deriving (Eq, Show)

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

getDogRM' :: Person -> Dog
getDogRM' = runReader $ Reader dogName >>= (\d -> Reader (Dog d . address))