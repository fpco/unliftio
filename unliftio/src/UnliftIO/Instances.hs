-- | Orphans instances
--
-- @since 0.1.0.0
module UnliftIO.Instances () where

import Control.Monad.IO.Unlift
import Control.Monad.Logger (LoggingT (..), NoLoggingT (..)) -- FIXME move these instances into monad-logger
import Control.Monad.Trans.Resource.Internal (ResourceT (..))

instance MonadUnliftIO m => MonadUnliftIO (LoggingT m) where
  askUnliftIO = LoggingT $ \f ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip runLoggingT f))
instance MonadUnliftIO m => MonadUnliftIO (NoLoggingT m) where
  askUnliftIO = NoLoggingT $
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . runNoLoggingT))
instance MonadUnliftIO m => MonadUnliftIO (ResourceT m) where
  askUnliftIO = ResourceT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip unResourceT r))
