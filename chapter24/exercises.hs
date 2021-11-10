{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ
import Control.Applicative
import Text.Trifecta
import Data.Char(digitToInt)

-- 1. Write a parser for semantic versions

-- Relevant to precedence/ordering,
-- cannot sort numbers like strings
data NumberOrString =
    NOSS String
  | NOSI Integer deriving (Show, Eq, Ord)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show, Eq)

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal :: Parser Major
  char '.'
  minor <- decimal :: Parser Minor
  char '.'
  patch <- decimal :: Parser Patch
  release <- (try (eof >> pure []) <|> parseRelease)
  metadata <- (try (eof >> pure []) <|> parseMetadata)
  return $ SemVer major minor patch release metadata
  where
    parseRelease :: Parser Release
    parseRelease = char '-' >> parseNosDotList
    parseMetadata :: Parser Metadata
    parseMetadata = char '+' >>  parseNosDotList
    parseNosDotList :: Parser [NumberOrString]
    parseNosDotList = do
      first <- parseNos
      remaining <- many $ char '.' >> parseNos
      return $ first : remaining
    parseNos :: Parser NumberOrString
    parseNos = (NOSI <$> try integer) <|> (NOSS <$> some letter)
  
instance Ord SemVer where
  compare sv sv' =
    compare (asList sv) (asList sv')
    where asList (SemVer ma mi pa re me) = [NOSI ma, NOSI mi, NOSI pa] ++ re ++ me

ex1 = "2.1.1"
ex2 = "1.0.0-x.7.z.92"
ex3 = "1.0.0-gamma+002"
ex4 = "1.0.0-beta+oof.sha.41af286"
big = SemVer 2 1 1 [] []
little = SemVer 2 1 0 [] []

p = parseString parseSemVer mempty
pex1 = p ex1
pex2 = p ex2
pex3 = p ex3
pex4 = p ex4

semverTest :: IO ()
semverTest = do
  print $ pex1
  print $ pex2
  print $ pex3
  print $ pex4
  print $ big > little

-- 2. Write a parser for positive integer values. Don't reuse digit or integer functions.

parseDigit :: Parser Char
parseDigit = oneOf "1234567890" <?> "parseDigit"

base10Integer :: Parser Integer
base10Integer = accumulate 0 0 . (<$>) toInteger . (<$>) digitToInt . reverse <$> some parseDigit
  where
    accumulate :: Integer -> Integer -> [Integer] -> Integer
    accumulate e x [] = x
    accumulate e x (d:ds) = accumulate (e + 1) (x + 10^e * d) ds
-- or this works, but they hint to use arithmetic so I don't think they wanted us to use `read`
-- base10Integer = read <$> some parseDigit

posintTest :: IO ()
posintTest = do
  let p = parseString parseDigit mempty
      p' = parseString base10Integer mempty
  print $ p "123"
  print $ p "abc"
  print $ p' "123abc"
  print $ p' "abc"

-- 3. Extend the base10Integer parser to handle negative and positive integers

base10Integer' :: Parser Integer
base10Integer' = char '-' >> (* (-1)) <$> base10Integer <|> base10Integer

intTest :: IO ()
intTest = do
  print $ parseString base10Integer' mempty "-123abc"

-- 4. Write a parser for US/Canada phone numbers with varying formats

-- aka area code
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int
data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  npa <- parseNPA
  e <- parseExchange
  ln <- parseLineNumber
  return $ PhoneNumber npa e ln
  where
    threeDigits = read <$> count 3 digit
    parseNPA :: Parser NumberingPlanArea
    parseNPA =
      let
        oneDash = string "1-" >> threeDigits
        withParens = char '(' *> threeDigits <* char ')'
      in
        try oneDash <|> (try withParens <|> try threeDigits)
    parseExchange :: Parser Exchange
    parseExchange =
      try (char ' ' >> threeDigits) <|> (try (char '-' >> threeDigits) <|> try threeDigits)
    parseLineNumber :: Parser LineNumber
    parseLineNumber =
      try (char '-' >> threeDigits) <|> threeDigits

phoneTest :: IO ()
phoneTest = do
  let p = \x -> print $ parseString parsePhone mempty x
  p "123-456-7890"
  p "1234567890"
  p "(123) 456-7890"
  p "1-123-456-7890"

-- 5. Write a parser for a log file format and sum the time spent in each activity

logEx :: String
logEx = [r|
-- wheee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

logEx' :: String
logEx' = [r|
-- wheee a comment
# 2025-02-05
08:00 Breakfast
|]

type Task = String
type Hour = Integer
type Minute = Integer
data Time = Time Hour Minute deriving (Eq, Ord, Show)
data Entry = Entry Time Task deriving (Eq, Ord, Show)
type Year = Integer
type Month = Integer
type Day = Integer
data Date = Date Year Month Day deriving (Eq, Ord, Show)
data DailyLog = DailyLog Date [Entry] deriving (Eq, Ord, Show)
type Log = [DailyLog]

parseDailyLog :: Parser DailyLog
parseDailyLog = do
  date <- parseDate
  entries <- some parseEntry
  return $ DailyLog date entries

parseDate :: Parser Date
parseDate = do
  char '#'
  char ' '
  year <- integer
  char '-'
  month <- integer
  char '-'
  day <- integer
  nextLine
  return $ Date year month day

parseEntry :: Parser Entry
parseEntry = do
  time <- parseTime
  char ' '
  task <- parseTask
  return $ Entry time task

parseTime :: Parser Time
parseTime = do
  hour <- integer
  char ':'
  minute <- integer
  return $ Time hour minute

parseTask :: Parser Task
parseTask = some nextCharCommentAware

nextCharCommentAware :: Parser Char
nextCharCommentAware = (commentStart >> (skipMany (noneOf "\n")) >> fail "") <|> noneOf "\n" 

commentStart :: Parser String
commentStart = string "--"

nextLine :: Parser ()
nextLine = do
  skipMany $ noneOf "\n"
  skipSome $ char '\n'
  return ()

skipCommentsAndNewlines :: Parser ()
skipCommentsAndNewlines = skipMany ((char '\n' >> pure ()) <|> (commentStart >> (skipMany (noneOf "\n")))) >> pure ()

parseLog :: Parser Log
parseLog =
  some $ skipCommentsAndNewlines >> parseDailyLog

-- honestly I think a good way to visualize Monads and sequencing and binding and all that is to
--   think about the order of operations in two directions. Left to right, like we're used to thinking
--   esp. in function application, then bottom-to-top, where Monads get "stacked" and produce 1 Monad
--   combinatorially. There's still going to be an order to that stacking.

logTest = do
  let p = \x -> print $ parseString parseLog mempty x
  p logEx'
  p logEx

