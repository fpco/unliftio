-- | Unlifted "Control.Concurrent.MVar"
--
-- @since 0.1.0.0
module UnliftIO.MVar
  ( MVar
  , newEmptyMVar
  , newMVar
  , takeMVar
  , putMVar
  , readMVar
  , swapMVar
  , tryTakeMVar
  , tryPutMVar
  , isEmptyMVar
  , withMVar
  , withMVarMasked
  , modifyMVar
  , modifyMVar_
  , modifyMVarMasked
  , modifyMVarMasked_
  , tryReadMVar
  , mkWeakMVar
  ) where

import System.Mem.Weak (Weak)
import Control.Concurrent.MVar (MVar)
import Control.Monad.IO.Unlift
import qualified Control.Concurrent.MVar as M

-- | Lifted 'M.newEmptyMVar'
--
-- @since 0.1.0.0
newEmptyMVar :: MonadIO m => m (MVar a)
newEmptyMVar = liftIO M.newEmptyMVar

-- | Lifted 'M.newMVar'
--
-- @since 0.1.0.0
newMVar :: MonadIO m => a -> m (MVar a)
newMVar = liftIO . M.newMVar

-- | Lifted 'M.takeMVar'
--
-- @since 0.1.0.0
takeMVar :: MonadIO m => MVar a -> m a
takeMVar = liftIO . M.takeMVar

-- | Lifted 'M.putMVar'
--
-- @since 0.1.0.0
putMVar :: MonadIO m => MVar a -> a -> m ()
putMVar var = liftIO . M.putMVar var

-- | Lifted 'M.readMVar'
--
-- @since 0.1.0.0
readMVar :: MonadIO m => MVar a -> m a
readMVar = liftIO . M.readMVar

-- | Lifted 'M.swapMVar'
--
-- @since 0.1.0.0
swapMVar :: MonadIO m => MVar a -> a -> m a
swapMVar var = liftIO . M.swapMVar var

-- | Lifted 'M.tryTakeMVar'
--
-- @since 0.1.0.0
tryTakeMVar :: MonadIO m => MVar a -> m (Maybe a)
tryTakeMVar = liftIO . M.tryTakeMVar

-- | Lifted 'M.tryPutMVar'
--
-- @since 0.1.0.0
tryPutMVar :: MonadIO m => MVar a -> a -> m Bool
tryPutMVar var = liftIO . M.tryPutMVar var

-- | Lifted 'M.isEmptyMVar'
--
-- @since 0.1.0.0
isEmptyMVar :: MonadIO m => MVar a -> m Bool
isEmptyMVar = liftIO . M.isEmptyMVar

-- | Lifted 'M.tryReadMVar'
--
-- @since 0.1.0.0
tryReadMVar :: MonadIO m => MVar a -> m (Maybe a)
tryReadMVar = liftIO . M.tryReadMVar

-- | Unlifted 'M.withMVar'
--
-- @since 0.1.0.0
withMVar :: MonadUnliftIO m => MVar a -> (a -> m b) -> m b
withMVar var f = withRunInIO $ \run -> M.withMVar var (run . f)

-- | Unlifted 'M.withMVarMasked'
--
-- @since 0.1.0.0
withMVarMasked :: MonadUnliftIO m => MVar a -> (a -> m b) -> m b
withMVarMasked var f = withRunInIO $ \run -> M.withMVarMasked var (run . f)

-- | Unlifted 'M.modifyMVar_'
--
-- @since 0.1.0.0
modifyMVar_ :: MonadUnliftIO m => MVar a -> (a -> m a) -> m ()
modifyMVar_ var f = withRunInIO $ \run -> M.modifyMVar_ var (run . f)

-- | Unlifted 'M.modifyMVar'
--
-- @since 0.1.0.0
modifyMVar :: MonadUnliftIO m => MVar a -> (a -> m (a, b)) -> m b
modifyMVar var f = withRunInIO $ \run -> M.modifyMVar var (run . f)

-- | Unlifted 'M.modifyMVarMasked_'
--
-- @since 0.1.0.0
modifyMVarMasked_ :: MonadUnliftIO m => MVar a -> (a -> m a) -> m ()
modifyMVarMasked_ var f = withRunInIO $ \run -> M.modifyMVarMasked_ var (run . f)

-- | Unlifted 'M.modifyMVarMasked'
--
-- @since 0.1.0.0
modifyMVarMasked :: MonadUnliftIO m => MVar a -> (a -> m (a, b)) -> m b
modifyMVarMasked var f = withRunInIO $ \run -> M.modifyMVarMasked var (run . f)

-- | Unlifted 'M.mkWeakMVar'
--
-- @since 0.1.0.0
mkWeakMVar :: MonadUnliftIO m => MVar a -> m () -> m (Weak (MVar a))
mkWeakMVar var f = withRunInIO $ \run -> M.mkWeakMVar var (run f)
