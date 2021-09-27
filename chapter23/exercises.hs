{-# LANGUAGE InstanceSigs #-}

{-| Write the following functions. -}

newtype State s a =
  State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State $ (\t -> (f (fst t), snd t)) . g

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State (\s -> (a, s))

  (<*>) :: State s (a -> b)
        -> State s a
        -> State s b
  (State f) <*> (State g) =
    -- f :: s -> (a -> b, s)
    -- g :: s -> (a, s)
    State (\s ->
      let t = f s
          t' = g $ snd t
      in
          (fst t $ fst t', snd t'))
  
instance Monad (State s) where
  return = pure

  (>>=) :: State s a
        -> (a -> State s b)
        -> State s b
  (State f) >>= g =
    -- f :: s -> (a, s)
    State (\s ->
      let fout = f s
          gout = g $ fst fout
      in
          runState gout $ snd fout)

-- 1. Construct a State where the state is also the value you return

get :: State s s
get = State $ \s -> (s, s)

-- 2. Construct a State where the resulting state is the argument provided,
--    and the value defaults to unit

put :: s -> State s ()
put s = State $ \_ -> ((), s)

-- 3. Run the State with s and get the state that results

exec :: State s a -> s -> s
exec (State sa) s = snd $ sa s

-- 4. Run the State with s and get the value that results

eval :: State s a -> s -> a
eval (State sa) = fst . sa

-- 5. Write a function that applies a function to create a new State

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

-- issue: one of the test evaluations the book includes is `runState f >> f 0`, but
--        that doesn't seem to work...