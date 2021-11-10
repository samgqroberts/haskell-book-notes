import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop
oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop

  pNL "one:"
  testParse one

  pNL "one':"
  testParse one'

  pNL "oneTwo:"
  testParse oneTwo

  pNL "oneTwo':"
  testParse oneTwo'

{-| Exercises: Parsing practice -}

-- 1. Make `one` and `oneTwo` fail because they don't exhaust the input stream

oneF :: Parser ()
oneF = one >> eof
oneF' = one >>= (\_ -> eof)

-- Parser a ~:: String -> Maybe (a, String), but in a Monad-y way

oneTwoF :: Parser ()
oneTwoF = oneTwo >> eof

-- 2. Use string to make a parser that parses "1", "12", and "123"

p123 :: [Char] -> Result String
p123 chars =  parseString (string chars) mempty "123"

-- 3. Try writing a Parser that does what string does, but using char

string' :: String -> Parser String
string' s = case s of
              (x:xs) -> char x >> (string' xs)
              _ -> stop

{-| Exercise: Unit of success -}

example = parseString (integer >> eof) mempty "123"
-- Success ()

-- Rewrite example so it returns the integer that it parses instead of Success ().

unitOfSuccessParser :: Parser Integer
unitOfSuccessParser = do
  i <- integer
  eof
  return i

unitOfSuccess = parseString unitOfSuccessParser mempty "123"

-- just for me, to more clearly understand `do` notations
unitOfSuccessParser' :: Parser Integer
unitOfSuccessParser' = integer >>= (\i -> eof >> return i)
unitOfSuccess' = parseString unitOfSuccessParser' mempty "123"

-- honestly, at this point in my education anyway, it seems like for a language / ecosystem
--   that values explicit definition and meaning so much, typeclasses introduce a surprising
--   amount of spooky action at a distance. it reminds me a lot of the "who the hell knows
--   what's going to happen here" of OOP with large inheritance chains.
--   it'd probably be necessary for me to have an ide that contextualizes everything for me
--   clearly and automatically.