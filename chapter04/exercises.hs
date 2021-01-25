awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- 1. `length` would have type signature `[a] -> Int`

{- 2.
  a) 5
  b) 3
  c) 2
  d) 5
-}

-- 3. `6 / length [1, 2, 3]` will fail, because `length` returns
--    an `Int`, which won't be type inferred into some type that
--    instances Fractional

-- 4. 6 `div` length [1, 2, 3]

-- 5. a `Bool`, `True`

-- 6. `False :: Bool`

{- 7.
  a) `True`
  b) won't work - list elements need to be same type
  c) 5
  d) False
  e) won't work - 9 isn't a bool
-}

-- 8.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

-- 9.
myAbs :: Integer -> Integer
myAbs x = if x < 0 then x * (-1) else x

-- 10.
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

-- Correcting syntax
-- 1.
x = (+)
fn xs = w `x` 1
     where w = length xs

-- 2.
y = y

-- 3.
f3 (a, b) = a

-- Match the function names to their types
-- 1. c
-- 2. b
-- 3. a
-- 4. d