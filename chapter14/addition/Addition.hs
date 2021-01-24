module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is\
       \ 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
    it "x + 1 is always\
       \ greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
  describe "multByRec" $ do
    it "15 multiplied by 3 is 45" $ do
      multByRec 15 3 `shouldBe` 45
    it "22 multiplied by 5 is 110" $ do
      multByRec 22 5 `shouldBe` 110

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

multByRec :: (Integral a) => a -> a -> a
multByRec x 1 = x
multByRec x y = x + multByRec x (y - 1)