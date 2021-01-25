{-| Multiple choice
1. c
2. b
3. a
4. c
5. a
-}

{-| Does it type check?
1. no, because there is no instance of Show for Person
2. no, because no instance of Eq
3. a) Blah or Woot
   b) typeerror because expected type is Mood
   c) no instance of Ord
4. no, because s1 doesn't have a 3rd argument to the sentence constructor
-}

{-| Given the datatype declaration, what can we do?
1. No, because you have to provide Rocks and Yeah, not just their innards
2. yep
3. sure, because Papu derives Eq
4. no because Papu doesn't derive Ord / doesn't have an instance
-}

{-| Match the types
1. no, because 1 needs Num, not just unconstrained
2. no, because 1.0 needs fractional, not just Num
3. yes, because 1.0 needs fractional and there it is
4. yes, because RealFrac implies Fractional
5. yes, because Ord is more specific than unconstrained
6. yes, because Int is more specific than unconstrained
7. no, because concrete return type of Int may not match given unconstrained type
8. no, because concrete return type of Int may not match given Num
9. yes, because nothing violates it being an Int
10. yes
11. no because Char more specific
-}

{-| Type-Kwon-Do Two: Electric typealoo
-}

-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = aToB a == b

-- 2.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB i a = aToB a + fromInteger i
