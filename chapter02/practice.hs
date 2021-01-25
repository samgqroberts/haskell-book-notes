module Mult1 where

mult1 = x * y
  where x = 5
        y = 6

ex1Let = let x = 3; y = 1000 in x * 3 + y
ex1Where = x * 3 + y
   where x = 3
         y = 1000

ex2Let = let y = 10; x = 10 * 5 + y in x * 5
ex2Where = x * 5
   where y = 10
         x = 10 * 5 + y

ex3Let =
  let x = 7
      y = negate x
      z = y * 10
  in z / x + y
ex3Where =
  z / x + y
  where x = 7
        y = negate x
        z = y * 10