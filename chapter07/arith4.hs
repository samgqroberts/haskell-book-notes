module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- 5. 
roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

-- 6.
roundTrip' :: (Show a, Read b) => a -> b
roundTrip' a = read (show a)

main = do
  print (roundTrip' (4 :: Int))
  print (id 4)