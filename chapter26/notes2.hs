{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad

{-| Exercise: Wrap it up

Turn readerUnwrap from the previous example back into embedded through the use of
  the data constructors for each transformer.
-}

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

embedded' :: Monad a => MaybeT (ExceptT String (ReaderT () a)) Int
embedded' = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

backToEmbedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
backToEmbedded =
  -- MaybeT . ExceptT . ReaderT $ readerUnwrap
  MaybeT . ExceptT . ReaderT $ y
  where
    x :: () -> Either String (Maybe Int)
    x = const (Right (Just 1))
    y :: () -> IO (Either String (Maybe Int))
    y = fmap pure x
    -- z :: ReaderT () (Either String) (Maybe Int)
    -- z = fmap pure $ ReaderT x 

{-| the imagery for the phrase 'lift over' never really made intuitive sense for me.
  it's more like "shove into" right?
  i wonder what they were visualizing when 'lift over' was chosen as the verbiage.
  if i lift a rock over a fence, my rock is now up, over, and on the other side of
    the fence. it didn't interact with the fence.
  perhaps the basic usage is talking about structure within structure, and lifting
    structure up into a more outer structure, like lifting IO into ActionM in the scotty
    example in the book.
    So like if we had `fmap (+1) [1, 2]`, the verbiage is we're "lifting (+) over [1, 2]".
      ... still doesn't make sense to me.
  Wait maybe lift over and lift into mean different things. 
-}

{-| Exercises: Lift more -}

-- 1.

{- (from notes 1) -}

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT meea) = EitherT $ fmap (fmap f) meea

instance (Applicative m) => Applicative (EitherT e m) where
  pure = EitherT . pure . pure

  (EitherT emab) <*> (EitherT ema) =
    EitherT $ fmap (<*>) emab <*> ema

instance (Monad m) => Monad (EitherT e m) where
  return = pure

  (EitherT ema) >>= f =
    EitherT $ do
      ma <- ema
      case ma of
        Left e -> return (Left e)
        Right a -> runEitherT (f a)

{- (end from notes 1) -}

instance MonadTrans (EitherT e) where
  lift :: (Monad m) => m a -> EitherT e m a
  lift ma =
    EitherT $ liftM Right ma


-- 2.

{- (from notes 1) -}

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT sm) =
    StateT $ fmap (fmap (\t -> (f $ fst t, snd t))) sm
  
instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  (StateT smab) <*> (StateT sma) =
    StateT $ \s -> do
      abs <- smab s
      as <- sma $ snd abs
      return (fst abs $ fst as, snd as)

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (StateT sma) >>= f =
    StateT $ \s -> do
      as <- sma s
      let
        stsmb = runStateT $ f (fst as)
      stsmb $ snd as

{- (end from notes 1) -}

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    return (a, s)