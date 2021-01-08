{-# LANGUAGE FlexibleInstances #-}

{-| Exercises: Dog types
1. Doggies is a type constructor
2. * -> *
3. *
4. Doggies Int
5. Doggies Integer
6. Doggies String
7. Both
8. a -> DogueDeBordeaux a
9. DogueDeBordeaux String
-}

data Price =
  Price Integer deriving (Eq, Show)

data Size =
  Size Integer deriving (Eq, Show)

data Manufacturer =
  Mini
  | Mazda
  | Tata
    deriving (Eq, Show)

data Airline =
  PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

{-| Exercises: Vehicles -}
-- 1. Vehicle
-- 2.
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

--3.
getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

--4. Bottom, baby
--5. done

{-| Exercises: Cardinality
1. 1
2. 3 
3. 2 ^ 16 = 65536
4.
*Main> minBound :: Int
-9223372036854775808
*Main> maxBound :: Int
9223372036854775807
so cardinality of Int is 9223372036854775808 + 9223372036854775807 + 1
  = 18446744073709551616
Integer is unbounded, meaning
*Main> maxBound :: Integer

<interactive>:22:1: error:
    • No instance for (Bounded Integer)
        arising from a use of ‘maxBound’
    • In the expression: maxBound :: Integer
      In an equation for ‘it’: it = maxBound :: Integer
5. number of bits to represent
-}


{-| Exercises: For example -}
data Example = MakeExample deriving Show
{- 1. 
*Main> :t MakeExample 
MakeExample :: Example
*Main> :t Example

<interactive>:1:1: error: Data constructor not in scope: Example
-}
{- 2.
*Main> :info Example
data Example = MakeExample      -- Defined at notes.hs:80:1
instance [safe] Show Example -- Defined at notes.hs:80:37
-}
-- 3.
data Example' = MakeExample' Int deriving Show
-- the data constructor now accepts an argument:
-- *Main> :type MakeExample'
-- MakeExample' :: Int -> Example'

{-| Exercises: Logic goats -}
class TooMany a where
  tooMany :: a -> Bool

-- 1.
instance TooMany (Int, String) where
  tooMany (i, _) = i > 42

-- 2.
instance TooMany (Int, Int) where
  tooMany (n, n') = n + n > 42

-- 3.
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, n') = tooMany (n + n')

{-| Exercises: Pity the bool
1. 2
2. 256 + 2 = 258
-}

{-| Exercises: How does your garden grow?
1.
data Garden
  = Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
 -}

{-| Exercise: Programmers -}

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer { os = x, lang = y } | x <- allOperatingSystems, y <- allLanguages ]

{-| Exercises: The Quad
1. 4 + 4 = 8
2. 4 * 4 = 16
3. 4 ^ 4 = 256
4. 2 * 2 * 2 = 8
5. 2 ^ (2 * 2) = 2 ^ 4 = 16
6. 4 ^ (2 * 4) = 4 ^ 8 = 65536
-}

{-| Write map for BinaryTree -}
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
       1
       (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
       2
       (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
    then print "yup OK!"
    else error "test failed!"

{-| Convert binary trees to lists -}
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : preorder left ++ preorder right 

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
       2
       (Node Leaf 3 Leaf)

testPreorder :: IO()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO()
testPostorder =
  if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears."

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

{-| Write foldr for BinaryTree -}
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b Leaf = b
foldTree f b (Node left a right) = foldTree f (foldTree f (f a b) left) right
