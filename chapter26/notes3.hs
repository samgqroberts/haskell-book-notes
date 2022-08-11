{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.Trans
import Control.Monad.IO.Class

{-| Exercises: Some (MonaIO) instances -}

-- 1. MaybeT

{-|
It's honestly kind of annoying that the book is structured in such a way that between different
non-end-of-chapter exercises you have to have different import contexts.
For example here if I have a previously-required import for Control.Monad.Trans.Maybe I can't
complete the exercise for writing the MonadIO instance for MaybeT, because it's a duplicate
instance declaration.
However, if I don't import that, then MaybeT isn't even defined anymore. I now have to
copy over the newtype declaration from two notes ago, along with the building-block
instance declarations.
-}

{-| copied from notes 1 -}

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))

  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (fmap (<*>) fab) <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (MaybeT ma) >>= f =
    MaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just y -> runMaybeT (f y)

{-| end copied from notes 1 -}

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO :: forall a.  IO a -> MaybeT m a
  liftIO = MaybeT . fmap Just . liftIO

-- 2. ReaderT

{-| copied from notes 1 -}

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap f) . rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure

  (ReaderT rmab) <*> (ReaderT rma) = ReaderT $ ((<*>) . rmab) <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure

  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (f a) r

{-| end copied from notes 1 -}

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO = ReaderT . const . liftIO


-- 3. StateT

{-| copied from notes 1 -}

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

{-| end copied from notes 1 -}

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO :: IO a -> StateT s m a
  liftIO ioa =
    StateT $ (\s -> fmap (\a -> (a, s)) a)
    where
      a = liftIO ioa