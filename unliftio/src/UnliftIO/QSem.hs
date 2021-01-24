-- | Unlifted "Control.Concurrent.QSem".
--
-- @since 0.2.14
module UnliftIO.QSem
  ( QSem
  , newQSem
  , waitQSem
  , signalQSem
  , withQSem
  ) where

import Control.Concurrent.QSem (QSem)
import Control.Monad.IO.Unlift
import UnliftIO.Exception
import qualified Control.Concurrent.QSem as Q

-- | Lifted 'Q.newQSem'.
--
-- @since 0.2.14
newQSem :: MonadIO m => Int -> m QSem
newQSem = liftIO . Q.newQSem

-- | Lifted 'Q.waitQSem'.
--
-- @since 0.2.14
waitQSem :: MonadIO m => QSem -> m ()
waitQSem = liftIO . Q.waitQSem

-- | Lifted 'Q.signalQSem'.
--
-- @since 0.2.14
signalQSem :: MonadIO m => QSem -> m ()
signalQSem = liftIO . Q.signalQSem

-- | 'withQSem' is an exception-safe wrapper for performing the
-- provided operation while holding a unit of value from the semaphore.
-- It ensures the semaphore cannot be leaked if there are exceptions.
--
-- @since 0.2.14
{-# INLINE withQSem #-}
withQSem :: MonadUnliftIO m => QSem -> m a -> m a
withQSem x io = withRunInIO $ \run ->
  bracket_ (waitQSem x) (signalQSem x) (run io)
