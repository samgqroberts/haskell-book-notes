import Test.QuickCheck

{-| Exercise: Optional Monoid -}
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

-- i had to instance Semigroup but the book didn't tell me to, otherwise i got:
-- notes.hs:7:10: error:
--     • Could not deduce (Semigroup (Optional a))
--         arising from the superclasses of an instance declaration
--       from the context: Monoid a
--         bound by the instance declaration at notes.hs:7:10-40
--     • In the instance declaration for ‘Monoid (Optional a)’
--   |
-- 7 | instance Monoid a => Monoid (Optional a) where
--   |     
instance Semigroup a => Semigroup (Optional a) where
  (<>) (Only a) (Only a') = Only (a <> a')
  (<>) (Only a) _ = Only a
  (<>) _ (Only a) = Only a
  (<>) _ _ = Nada

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) (Only a') = Only (a `mappend` a')
  mappend (Only a) _ = Only a
  mappend _ (Only a) = Only a
  mappend _ _ = Nada

{-| Madness -}

type Exclamation = String
type Adverb = String
type Noun = String
type Adjective = String

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
   mconcat [e, "! he said ", adv, " as he jumped into his car ",
     noun, " and drove off with his ", adj, " wife."]

{-| Exercise: Maybe another Monoid -}

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' (Only a)) (First' (Only a')) = First' (Only a)
  (<>) (First' (Only a)) _ = First' (Only a)
  (<>) _ (First' (Only a')) = First' (Only a')
  (<>) _ _ = mempty

instance Monoid (First' a) where
  mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
monoidLeftIdentity a = (mempty <> a) == a
monoidRightIdentity a = (a <> mempty) == a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return (Only a)), (1, return Nada)]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    return $ First' a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
