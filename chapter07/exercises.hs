{-| Multiple choice
1. d
2. b) Char -> [String]
3. d
4. b
5. a
-}

{-| Let's write code
-}

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

-- 1.
-- a)
tensDigitUsingDivMod :: Integral a => a -> a
tensDigitUsingDivMod = snd . (flip divMod) 10 . fst . (flip divMod) 10
-- b) yes
-- c)
hunsD = snd . (flip divMod) 10 . fst . (flip divMod) 100

-- 2.
foldBool1 :: a -> a -> Bool -> a
foldBool1 x y b = case b of
  False -> x
  True -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b = if b then y else x

-- 3.
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- 4. -> in arith4.hs