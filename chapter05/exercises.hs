{-# LANGUAGE NoMonomorphismRestriction #-}

{-| Multiple choice
1. c
2. a
3. b (and a? eh i guess not, but in a sense cuz all functions are unary)
4. c
-}

{-| Determine the type

-}

ex1a = (* 9) 6 -- Num a => a
ex1b = head [(0, "doge"),(1, "kitteh")] -- Num a => (a, [Char])
ex1c = head [(0 :: Integer, "doge"), (1, "kitteh")] -- (Integer, [Char])
ex1d = if False then True else False -- Bool
ex1e = length [1, 2, 3, 4, 5] -- Int
ex1f = (length [1, 2, 3, 4]) > (length "TACOCAT") -- Bool

-- 2. Num a => a
-- 3. Num a => a -> a
-- 4. Fractional a => a
-- 5. [Char]

{-| Does it compile?
1. won't compile, because bigNum doesn't accept any args
2. all fine
3. c won't compile because b doesn't accept an argument, same with d until u fix c
4. won't compile in a repl, or a file
-}

{-| Type variable or specific type constructor?
2. zed is fully, Zed is concrete, Blah is concrete
3. a is fully, b is constrained to Enum, C is concrete
4. f and g are full, C is concrete
-}

{-| Write a type signature
1. [a] -> a
2. Ord a => a -> a -> Bool
3. (a, b) -> b
-}

{-| Given a type, write a function
1. i x = x
2. c x y = x
3. yes
4. c' x y = y
5. reverse, or tail
6. co bToC aToB a = bToC (aToB a)
7. a _ x = x
8. a' xToY x = xToY x
-}

{-| Fix it

-}

{-| Type-Kwon-Do
1. h x = g $ f x
2. e x = w $ q x
3. xform (x, y) = (xz x, yz y)
4. munge xToY yToWZTuple x = fst (snd $ xToY x)
-}