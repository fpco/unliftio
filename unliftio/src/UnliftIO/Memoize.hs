{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Memoize the results of actions. In other words: actions
-- will be run once, on demand, and their results saved.
--
-- Exceptions semantics: if a synchronous exception is thrown while performing
-- the computation, that result will be saved and rethrown each time
-- 'runMemoized' is called subsequently.'
--
-- @since 0.2.8.0
module UnliftIO.Memoize
  ( Memoized
  , runMemoized
  , memoizeRef
  , memoizeMVar
  ) where

import Control.Applicative as A
import Control.Monad (join)
import Control.Monad.IO.Unlift
import UnliftIO.Exception
import UnliftIO.IORef
import UnliftIO.MVar

-- | A \"run once\" value, with results saved. Extract the value with
-- 'runMemoized'. For single-threaded usage, you can use 'memoizeRef' to
-- create a value. If you need guarantees that only one thread will run the
-- action at a time, use 'memoizeMVar'.
--
-- Note that this type provides a 'Show' instance for convenience, but not
-- useful information can be provided.
--
-- @since 0.2.8.0
newtype Memoized a = Memoized (IO a)
  deriving (Functor, A.Applicative, Monad)
instance Show (Memoized a) where
  show _ = "<<Memoized>>"

-- | Extract a value from a 'Memoized', running an action if no cached value is
-- available.
--
-- @since 0.2.8.0
runMemoized :: MonadIO m => Memoized a -> m a
runMemoized (Memoized m) = liftIO m
{-# INLINE runMemoized #-}

-- | Create a new 'Memoized' value using an 'IORef' under the surface. Note that
-- the action may be run in multiple threads simultaneously, so this may not be
-- thread safe (depending on the underlying action). Consider using
-- 'memoizeMVar'.
--
-- @since 0.2.8.0
memoizeRef :: MonadUnliftIO m => m a -> m (Memoized a)
memoizeRef action = withRunInIO $ \run -> do
  ref <- newIORef Nothing
  pure $ Memoized $ do
    mres <- readIORef ref
    res <-
      case mres of
        Just res -> pure res
        Nothing -> do
          res <- tryAny $ run action
          writeIORef ref $ Just res
          pure res
    either throwIO pure res

-- | Same as 'memoizeRef', but uses an 'MVar' to ensure that an action is
-- only run once, even in a multithreaded application.
--
-- @since 0.2.8.0
memoizeMVar :: MonadUnliftIO m => m a -> m (Memoized a)
memoizeMVar action = withRunInIO $ \run -> do
  var <- newMVar Nothing
  pure $ Memoized $ join $ modifyMVar var $ \mres -> do
    res <- maybe (tryAny $ run action) pure mres
    pure (Just res, either throwIO pure res)
