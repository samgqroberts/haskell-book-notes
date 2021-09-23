module ReaderPractice where

import Control.Applicative
import Data.Maybe

{-| A warm up stretch -}

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a
-- variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

-- thought: what the heck are they asking for x1, x2, and x3...?
--          ohhh I was thinking "xs" was the original list x, not the fn we made

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]

  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]

  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z

  print "hi"
  print $ sequenceA [(>3), (<8), even] 7
  -- thought: so... because haskell applies typeclasses to (->), here sequenceA can appear to take
  --          one OR two arguments...

  -- 1. Fold the Boolean conjunction operator over the list of results of sequA (applied to some value)
  print $ foldr (&&) True $ sequA 4

  -- 2. Apply sequA to s' -- you'll need fromMaybe
  print $ sequA (fromMaybe 1 s')

  -- 3. Apply bolt to ys -- you'll need fromMaybe
  print $ bolt (fromMaybe 1 ys)
