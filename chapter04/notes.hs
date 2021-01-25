{-|
exercises: mood swing
1. Mood
2. Blah, Woot
3. needs to be `changeMood :: Mood -> Mood`
-}
-- 4. 
data Mood = Blah | Woot deriving Show
changeMood Blah = Woot
changeMood _ = Blah

-- so 1 is a "data constructor" for the Int datatype?

{-|
  Exercises: Find the mistakes
  1. need to capitalize true
  2. need == instead of =
  3. already correct
  4. need double quotes around the strings
  5. can't concatenate the different types
-}
