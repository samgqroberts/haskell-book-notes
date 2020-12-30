module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
           \ symmetry?"
sentences = firstSen ++ secondSen
         ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines "" = []
myLines ('\n' : s) = myLines s
myLines s = takeWhile (/= '\n') s : myLines (dropWhile (/= '\n') s)

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn char (head : tail)
  | char == head = splitOn char tail
  | otherwise = takeWhile (/= char) s : splitOn char (dropWhile (/= char) s)
      where s = head : tail

myLines' = splitOn '\n'

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences
           == shouldEqual)