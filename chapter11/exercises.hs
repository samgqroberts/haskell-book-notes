import Data.Char
import Data.List

{-| Multiple Choice
1. a
2. c
3. b
4. c
-}

{-| As-Patterns -}
-- 1.
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf ts@(t:ts') (x:xs)
  | t == x = isSubseqOf ts' xs
  | otherwise = isSubseqOf ts xs
-- 2.
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map go . words
  where go s@(h:t) = (s, toUpper h : t)

{-| Language exercises -}
-- 1.
capitalizeWord :: String -> String
capitalizeWord s@(h:t) = toUpper h : t
-- 2.
capitalizeParagraph :: String -> String
capitalizeParagraph s = go s True
  where go "" _ = ""
        go (h:t) cap
          | h == '.' = h : go t True
          | h == ' ' = h : go t cap
          | otherwise = (if cap then toUpper h else h) : go t False

{-| Phone exercise -}

data ButtonFunction = Capitalize | Character [Char]
data Button = Button Char ButtonFunction
newtype DaPhone = DaPhone [Button]

phone = DaPhone
  [ Button '1' (Character []), Button '2' (Character "ABC"), Button '3' (Character "DEF")
  , Button '4' (Character "GHI"), Button '5' (Character "JKL"), Button '6' (Character "MNO")
  , Button '7' (Character "PQRS"), Button '8' (Character "TUV"), Button '9' (Character "WXYZ")
  , Button '*' Capitalize, Button '0' (Character " "), Button '#' (Character ".,")
  ]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

convo :: [String]
convo =
  ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol OK. Have u ever tasted alcohol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "OK. Do u think I am pretty Lol",
  "Lol ya",
  "Just making sure rofl ur turn"]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p@(DaPhone buttons) c
  | isUpper c = case find isCapitalizeButton buttons of
                       Nothing -> error "Phone does not have a capitalization button!"
                       Just (Button key _) -> (key, 1) : reverseTaps p (toLower c)
  | otherwise = case find (\(_, taps) -> taps > 0) (map tapsToGetChar buttons) of
                       Nothing -> error ("Phone does not have a key for character '" ++ [c] ++ "'!")
                       Just ((Button key _), taps) -> [(key, taps)]
    where tapsToGetChar b@(Button _ (Character chars)) = case elemIndex (toUpper c) chars of
                                                         Nothing -> (b, 0)
                                                         Just index -> (b, index + 1)
          tapsToGetChar b@(Button _ _) = (b, 0)
          isCapitalizeButton (Button _ Capitalize) = True
          isCapitalizeButton (Button _ _) = False

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = foldr (\a b -> reverseTaps phone a ++ b) []

-- 3.
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, p) b -> p + b) 0

-- 4.
mostPopularLetter :: String -> Char
mostPopularLetter s = maximumBy ((_, count) (_, count') => compare count count') $ go s []
  where go "" occurrences = occurrences
        go (h:t) occurrences = case find ((c, _) -> h == c) occurrences of
                                    Nothing -> 

mostPopularLetterAndCost :: String -> (Char, Presses)
mostPopularLetterAndCost =