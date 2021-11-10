{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

main :: IO ()
main = do
  let parseFraction' = parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

{-| Exercise: Try try -}

-- Make a parser that can parse either decimals or fractions

type FracOrDec = Either Rational Integer

parseFod :: Parser FracOrDec
parseFod = do
  v <- (try (Left <$> parseFraction)) <|> (Right <$> integer)
  return v

-- If my plan after this book is to try making a little language compiler in Haskell as an exercise,
--   this chapter might be the most valuable chapter in the book (so far at least)