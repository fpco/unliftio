{-# LANGUAGE CPP #-}
-- | Lifted version of "Control.Concurrent.STM"
--
-- @since 0.2.1.0
module UnliftIO.STM
  ( -- * Core
    STM.STM
  , atomically
  , retrySTM
  , checkSTM

    -- * TVar
  , STM.TVar
  , newTVarIO
  , readTVarIO
  , STM.newTVar
  , STM.readTVar
  , STM.writeTVar
  , STM.modifyTVar
  , STM.modifyTVar'
  , STM.swapTVar
  , registerDelay
  , mkWeakTVar

    -- * TMVar
  , STM.TMVar
  , STM.newTMVar
  , STM.newEmptyTMVar
  , newTMVarIO
  , newEmptyTMVarIO
  , STM.takeTMVar
  , STM.putTMVar
  , STM.readTMVar
  , STM.tryReadTMVar
  , STM.swapTMVar
  , STM.tryTakeTMVar
  , STM.tryPutTMVar
  , STM.isEmptyTMVar
  , mkWeakTMVar

    -- * TChan
  , STM.TChan
  , STM.newTChan
  , newTChanIO
  , STM.newBroadcastTChan
  , newBroadcastTChanIO
  , STM.dupTChan
  , STM.cloneTChan
  , STM.readTChan
  , STM.tryReadTChan
  , STM.peekTChan
  , STM.tryPeekTChan
  , STM.writeTChan
  , STM.unGetTChan
  , STM.isEmptyTChan

    -- * TQueue
  , STM.TQueue
  , STM.newTQueue
  , newTQueueIO
  , STM.readTQueue
  , STM.tryReadTQueue
  , STM.peekTQueue
  , STM.tryPeekTQueue
  , STM.writeTQueue
  , STM.unGetTQueue
  , STM.isEmptyTQueue

    -- * TBQueue
  , STM.TBQueue
  , STM.newTBQueue
  , newTBQueueIO
  , STM.readTBQueue
  , STM.tryReadTBQueue
  , STM.peekTBQueue
  , STM.tryPeekTBQueue
  , STM.writeTBQueue
  , STM.unGetTBQueue
  , STM.isEmptyTBQueue
  , STM.isFullTBQueue
  ) where

import Control.Concurrent.STM (STM, TVar, TMVar, TChan, TQueue, TBQueue)
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Unlift
import System.Mem.Weak (Weak)
import Numeric.Natural (Natural)

-- | Lifted version of 'STM.atomically'
--
-- @since 0.2.1.0
atomically :: MonadIO m => STM a -> m a
atomically = liftIO . STM.atomically

-- | Renamed 'STM.retry' for unqualified export
--
-- @since 0.2.1.0
retrySTM :: STM a
retrySTM = STM.retry

-- | Renamed 'STM.check' for unqualified export
--
-- @since 0.2.1.0
checkSTM :: Bool -> STM ()
checkSTM = STM.check

-- | Lifted version of 'STM.newTVarIO'
--
-- @since 0.2.1.0
newTVarIO :: MonadIO m => a -> m (TVar a)
newTVarIO = liftIO . STM.newTVarIO

-- | Lifted version of 'STM.readTVarIO'
--
-- @since 0.2.1.0
readTVarIO :: MonadIO m => TVar a -> m a
readTVarIO = liftIO . STM.readTVarIO

-- | Lifted version of 'STM.registerDelay'
--
-- @since 0.2.1.0
registerDelay :: MonadIO m => Int -> m (TVar Bool)
registerDelay = liftIO . STM.registerDelay

-- | Lifted version of 'STM.mkWeakTVar'
--
-- @since 0.2.1.0
mkWeakTVar :: MonadUnliftIO m => TVar a -> m () -> m (Weak (TVar a))
mkWeakTVar var final = withRunInIO $ \run -> STM.mkWeakTVar var (run final)

-- | Lifted version of 'STM.newTMVarIO'
--
-- @since 0.2.1.0
newTMVarIO :: MonadIO m => a -> m (TMVar a)
newTMVarIO = liftIO . STM.newTMVarIO

-- | Lifted version of 'STM.newEmptyTMVarIO'
--
-- @since 0.2.1.0
newEmptyTMVarIO :: MonadIO m => m (TMVar a)
newEmptyTMVarIO = liftIO STM.newEmptyTMVarIO

-- | Lifted version of 'STM.mkWeakTMVar'
--
-- @since 0.2.1.0
mkWeakTMVar :: MonadUnliftIO m => TMVar a -> m () -> m (Weak (TMVar a))
mkWeakTMVar var final = withRunInIO $ \run -> STM.mkWeakTMVar var (run final)

-- | Lifted version of 'STM.newTChanIO'
--
-- @since 0.2.1.0
newTChanIO :: MonadIO m => m (TChan a)
newTChanIO = liftIO STM.newTChanIO

-- | Lifted version of 'STM.newBroadcastTChanIO'
--
-- @since 0.2.1.0
newBroadcastTChanIO :: MonadIO m => m (TChan a)
newBroadcastTChanIO = liftIO STM.newBroadcastTChanIO

-- | Lifted version of 'STM.newTQueueIO'
--
-- @since 0.2.1.0
newTQueueIO :: MonadIO m => m (TQueue a)
newTQueueIO = liftIO STM.newTQueueIO

-- | Lifted version of 'STM.newTBQueueIO'
--
-- @since 0.2.1.0
#if MIN_VERSION_stm(2, 5, 0)
newTBQueueIO :: MonadIO m => Natural -> m (TBQueue a)
#else
newTBQueueIO :: MonadIO m => Int -> m (TBQueue a)
#endif
newTBQueueIO = liftIO . STM.newTBQueueIO
