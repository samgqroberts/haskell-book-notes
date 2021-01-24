module WordNumberTest where

import Test.Hspec
import Test.QuickCheck
import WordNumber (digitToWord, digits, wordNumber)
import Data.List (sort)

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"
  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]
  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
  describe "Using QuickCheck" $ do
    it "1." $ do
      property $ \x -> x == halfIdentity (x :: Double)
    it "2." $ do
      property $ \x -> listOrdered (sort (x :: [Int]))
    it "3. plusAssociative" $ do
      property $ \x y z -> plusAssociative (x :: Int) (y :: Int) (z :: Int)
    it "3. plusCommutative" $ do
      property $ \x y -> plusCommutative (x :: Int) (y :: Int)
    it "4. multAssociative" $ do
      property $ \x y z -> multAssociative (x :: Int) (y :: Int) (z :: Int)
    it "4. multCommutative" $ do
      property $ \x y -> multCommutative (x :: Int) (y :: Int)
    it "5. quot rem" $ do
      property $ \x y -> y == 0 || (quot (x :: Int) (y :: Int)) * y + (rem x y) == x
    it "5. div mod" $ do
      property $ \x y -> y == 0 || (div (x :: Int) (y :: Int)) * y + (mod x y) == x

half x = x /2
halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x

multAssociative x y z = x * (y * z) == (x * y) * z
multCommutative x y = x * y == y * x