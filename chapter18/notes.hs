import Control.Monad (join)

{-| The answer is the exercise -}

-- Write bind in terms of fmap and join

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m

-- Monoids bring mappend
-- Functors bring fmap
-- Applicatives combine fmap and mappend
-- Monads bring join, to fmap, mappend, then join all together?

-- would anyone ever use liftA or liftM when it's just fmap?
-- would anyone use liftM2 when it's just liftA2?

{-|
The equivalence of these two fns helps illuminate something about Algebraic Effects in Unison.
The <- creates a whole new function that takes in `name` and runs the rest of the `do` block
  once `getLine` finishes executing and has an answer.
Algebraic effects similarly create whole new functions (aka the "continuation") with the rest of the block
  when an ability fn is called.
it even uses the `<-` operator.
and they're calling the continuation "k".

  bindingAndSequencing :: IO ()
  bindingAndSequencing = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn ("y helo thar: " ++ name)

  bindingAndSequencing' :: IO ()
    bindingAndSequencing' =
    putStrLn "name pls:" >>
    getLine >>=
    \name ->
    putStrLn ("y helo thar: " ++ name)
-}

{-| Short Exercise: Either Monad -}

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First a) _ = First a
  (<*>) (Second f) (First a) = First a
  (<*>) (Second f) (Second b) = Second (f b)

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _ = First a
  (>>=) (Second b) k = k b
