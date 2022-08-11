{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

{-|
The `innerMost`, `second'`, and `final'` examples are great for wrapping the head
around fmapping / app-ing through structure.

-- expects some structure around a -> b
-- in the example: List Maybe Identity is the structure
-- so first arg is List (Maybe (Identity a)) -> List (Maybe (Identity b))
final' = (<*>)

-- expects some structure around some structure around a -> b
-- List Maybe is the entire structure, leaving Identity a -> Identity b as the "a" and "b"
-- so: List (Maybe (Identity a -> Identity b))
second' = fmap (<*>)

-- expects some structure around some structure around some structure around a -> b
-- List Maybe Identity is entire structure, leaving just a -> b as the a -> b
-- so: List (Maybe (Identity (a -> b)))
innerMost = (fmap . fmap) (<*>)
-}

{-| Also, like, are we supposed to look at final', second', and innerMost and
  grok them? Intuitively, or even near to intuitively? I can stare at them for a few
  minutes and maybe start to see the logic at a deeper level then just reading words
  on a screen, but that doesn't get near intuitively. How should I gather that ability?
  Probably hands-on, goal-oriented experience, like how I've gathered that ability with
  other concepts / languages. Pretty ready to graduate from this book I guess is what
  I'm saying. -}

{-| is `forall` not in base haskell, only in certain extensions like ScopedTypeVariables? -}

instance (Applicative m) => Applicative (MaybeT m) where
  pure x =
    MaybeT (pure (pure x))

  (<*>) :: forall a b. MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT fab) <*> (MaybeT mma) =
    MaybeT $ (fmap (<*>) fab) <*> mma
    where
      x :: m (Maybe (a -> b))
      x = fab
      y :: m (Maybe a -> Maybe b)
      y = fmap (<*>) fab

{-| It helps to constantly keep the actual data type we're working with in mind.
    A MaybeT instance contains a monad wrapping a Maybe wrapping a polymorphic `a`.
    monad wrapping a Maybe wrapping an `a`.
    Some `a` with composed monads on top of it.
    We're just staring at type signatures all the time, which sometimes might give us
    a false sense of how the data is actually structured. For `MaybeT m a` we have to
    remember the `m` doesn't directly contain an `a`, even though the signature might
    lead us to believe that. -}

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f =
    MaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just y -> runMaybeT (f y)

{-| I'd love for the accessors like runMaybeT to be called "get" something, but "get" what?
    You're not getting a MaybeT, so "getMaybeT" doesn't work.
    And you probably want "MaybeT" in the name because this goes into the global namespace.
    What about "accessMaybeT"? It is an "accessor" after all. -}

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

{-| I wonder why the left-side type of Either comes before the monad in the type signature,
    and whether that impacts anything -}

{-| Exercises: EitherT -}

-- 1. Write the Functor instance for EitherT

instance Functor m => Functor (EitherT e m) where
  -- using `fmap (fmap f)` makes slightly more intuitive sense to me than `(fmap . fmap) f`
  fmap f (EitherT meea) = EitherT $ fmap (fmap f) meea

{-| This does get easier for me the more I do it, the more I fmap and (<*>) through structure -}

-- 2. Write the Applicative instance for EitherT

instance (Applicative m) => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure = EitherT . pure . pure

  (<*>) :: forall a b. EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT emab) <*> (EitherT ema) =
    EitherT $ fmap (<*>) emab <*> ema
    where
      x :: m (Either e (a -> b))
      x = emab
      y :: m (Either e a -> Either e b)
      y = fmap (<*>) emab

{-| This and the chapter before could have been called "Dealing with two levels of structure",
    since that's mainly the hard part here and Monad Transformers is just the first time we've
    really seen that.
    Actually, I guess "Composing Types" isn't a bad name for that last chapter. -}

instance (Monad m) => Monad (EitherT e m) where
  return = pure

  (>>=) :: forall a b. EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT ema) >>= f =
    EitherT $ do
      -- ma :: Either e a
      ma <- ema
      case ma of
        Left e -> return (Left e)
        -- a :: a
        Right a -> runEitherT (f a)
    where
      x :: m (Either e a)
      x = ema

{-| I'm feeling more fluent getting the types of partial sections of expressions by using
    the where clause and checking gchid.
    How to do that within a do block tho?
      figured it out - use `let` in a do block. -}

-- 4. Write the swapEitherT helper function for EitherT

-- transformer version of swapEither
swapEitherT :: forall a e m. (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) =
  EitherT $ fmap swapEither ema
  where
    x :: m (Either e a)
    x = ema
    swapEither :: Either e a -> Either a e
    swapEither eea =
      case eea of
        Left e -> Right e
        Right a -> Left a

{-| does forall in ScopedTypeVariables just establish a scope for type variables? -}

-- 5. Write the transformer variant of the either catamorphism

eitherT :: forall a b c m. Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT amc bmc (EitherT meab) =
  do
    eab <- meab
    case eab of
      Left a -> amc a
      Right b -> bmc b
  where
    x :: m (Either a b)
    x = meab

{-| Remember: "catamorphism" is a generalization of the word "fold" / "reduce" -}

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

{-| Wait... Reader is defined as:
  newtype Reader r a = Reader { runReader :: r -> a }
  and with the other transformers we took the data type and wrapped that entire thing
  in a monad... So wouldn't wrapping the entire thing look like `m (r -> a)`?

  Later on the books says:
  A necessary byproduct of how transformers work is that the
  additional structure m is always wrapped around our value. One
  thing to note is that it’s only wrapped around things we can
  have, not things we need, such as with ReaderT.

  This doesn't really help clarify, just points it out.
-}

instance (Functor m) => Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT rma) = ReaderT $ (fmap f) . rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure

  (<*>) :: forall a b. ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT rmab) <*> (ReaderT rma) =
    ReaderT $ ((<*>) . rmab) <*> rma

{-| Okay at this point it's pretty clear that the functor / applicative / monad instances of
    transformers follow a very similar pattern to each other, regardless of the data type.
    for fmap you go `(fmap . fmap) f x`, though in my instance here I used function composition
    directly instead of fmapping into the function.
    for (<*>) you go `ThingT $ fmap (<*>) tab <*> ta`, though again here I used function composition
    instead of fmap. -}

instance (Monad m) => Monad (ReaderT r m) where
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (f a) r

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

{-| Exercises: StateT -}

-- 1. Functor instance

{-| The pattern I JUST laid out doesn't seem to be working here.
    Probably because we actually have a third bit of structure, the tuple in the return value. -}

{-| Note: adding `m` to the forall made the implementation forget that `m` is a Functor.
    Probably because it established a new scope. -}

instance (Functor m) => Functor (StateT s m) where
  fmap :: forall a b. (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sm) =
    StateT $ fmap (fmap (\t -> (f $ fst t, snd t))) sm
    where
      x :: s -> m (a, s)
      x = sm
      y :: s -> m (b, s)
      y = fmap (fmap (\t -> (f $ fst t, snd t))) sm
  
instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: forall a b. StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smab) <*> (StateT sma) =
    StateT $ \s -> do
      abs <- smab s
      as <- sma $ snd abs
      return (fst abs $ fst as, snd as)
    where
      x :: s -> m (a -> b, s)
      x = smab
      y :: s -> m (a, s)
      y = sma

{-| tip: keep undefined as the last expression in a do block as you write it
    to make sure that each line you write type-checks without having to have
    the return value ready yet -}

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (>>=) :: forall a b. StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f =
    StateT $ \s -> do
      as <- sma s
      let
        stsmb :: s -> m (b, s)
        stsmb = runStateT $ f (fst as)
      stsmb $ snd as
    where
      x :: s -> m (a, s)
      x = sma

{-| Book says "Lists in Haskell are as much a control structure as a data structure".
    By control structure they mean controlling the flow of logic, like if / for statements.
    No for statements, only reductions / maps, and lists can contain functions, so there's
    your control flow equivalency. -}
