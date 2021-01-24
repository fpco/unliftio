-- | Unlifted "Control.Concurrent.QSemN".
--
-- @since 0.2.14
module UnliftIO.QSemN
  ( QSemN
  , newQSemN
  , waitQSemN
  , signalQSemN
  , withQSemN
  ) where

import Control.Concurrent.QSemN (QSemN)
import Control.Monad.IO.Unlift
import UnliftIO.Exception
import qualified Control.Concurrent.QSemN as Q

-- | Lifted 'Q.newQSemN'.
--
-- @since 0.2.14
newQSemN :: MonadIO m => Int -> m QSemN
newQSemN = liftIO . Q.newQSemN

-- | Lifted 'Q.waitQSemN'.
--
-- @since 0.2.14
waitQSemN :: MonadIO m => QSemN -> Int -> m ()
waitQSemN x = liftIO . Q.waitQSemN x

-- | Lifted 'Q.signalQSemN'.
--
-- @since 0.2.14
signalQSemN :: MonadIO m => QSemN -> Int -> m ()
signalQSemN x = liftIO . Q.signalQSemN x

-- | 'withQSemN' is an exception-safe wrapper for performing the
-- provided operation while holding N unit of value from the semaphore.
-- It ensures the semaphore cannot be leaked if there are exceptions.
--
-- @since 0.2.14
{-# INLINE withQSemN #-}
withQSemN :: MonadUnliftIO m => QSemN -> Int -> m a -> m a
withQSemN x n io = withRunInIO $ \run ->
  bracket_ (waitQSemN x n) (signalQSemN x n) (run io)
