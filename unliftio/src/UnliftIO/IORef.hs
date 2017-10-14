-- | Unlifted "Data.IORef".
--
-- @since 0.1.0.0
module UnliftIO.IORef
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  , modifyIORef
  , modifyIORef'
  , atomicModifyIORef
  , atomicModifyIORef'
  , atomicWriteIORef
  , mkWeakIORef
  ) where

import Data.IORef (IORef)
import qualified Data.IORef as I
import Control.Monad.IO.Unlift
import System.Mem.Weak (Weak)

-- | Lifted 'I.newIORef'.
--
-- @since 0.1.0.0
newIORef :: MonadIO m => a -> m (IORef a)
newIORef = liftIO . I.newIORef

-- | Lifted 'I.readIORef'.
--
-- @since 0.1.0.0
readIORef :: MonadIO m => IORef a -> m a
readIORef = liftIO . I.readIORef

-- | Lifted 'I.writeIORef'.
--
-- @since 0.1.0.0
writeIORef :: MonadIO m => IORef a -> a -> m ()
writeIORef ref = liftIO . I.writeIORef ref

-- | Lifted 'I.modifyIORef'.
--
-- @since 0.1.0.0
modifyIORef :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyIORef ref = liftIO . I.modifyIORef ref

-- | Lifted 'I.modifyIORef''.
--
-- @since 0.1.0.0
modifyIORef' :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyIORef' ref = liftIO . I.modifyIORef' ref

-- | Lifted 'I.atomicModifyIORef'.
--
-- @since 0.1.0.0
atomicModifyIORef :: MonadIO m => IORef a -> (a -> (a, b)) -> m b
atomicModifyIORef ref = liftIO . I.atomicModifyIORef ref

-- | Lifted 'I.atomicModifyIORef''.
--
-- @since 0.1.0.0
atomicModifyIORef' :: MonadIO m => IORef a -> (a -> (a, b)) -> m b
atomicModifyIORef' ref = liftIO . I.atomicModifyIORef' ref

-- | Lifted 'I.atomicWriteIORef'.
--
-- @since 0.1.0.0
atomicWriteIORef :: MonadIO m => IORef a -> a -> m ()
atomicWriteIORef ref = liftIO . I.atomicWriteIORef ref

-- | Unlifted 'I.mkWeakIORef'.
--
-- @since 0.1.0.0
mkWeakIORef :: MonadUnliftIO m => IORef a -> m () -> m (Weak (IORef a))
mkWeakIORef ref final = withRunInIO $ \run -> I.mkWeakIORef ref (run final)
