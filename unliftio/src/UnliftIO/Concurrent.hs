{-# LANGUAGE RankNTypes #-}
-- | Unlifted "Control.Concurrent".
--
-- This module is not reexported by "UnliftIO",
-- use it only if "UnliftIO.Async" is not enough.
--
-- @since 0.1.1.0
module UnliftIO.Concurrent
  (
    -- * Concurrent Haskell
    ThreadId,

    -- * Basic concurrency operations
    myThreadId, forkIO, forkWithUnmask, forkFinally, killThread, throwTo,

    -- ** Threads with affinity
    forkOn, forkOnWithUnmask, getNumCapabilities, setNumCapabilities,
    threadCapability,

    -- * Scheduling
    yield,

    -- ** Waiting
    threadDelay, threadWaitRead, threadWaitWrite,

    -- * Communication abstractions
    module UnliftIO.MVar, module UnliftIO.Chan,

    -- * Bound Threads
    C.rtsSupportsBoundThreads, forkOS, isCurrentThreadBound, runInBoundThread,
    runInUnboundThread,

    -- * Weak references to ThreadIds
    mkWeakThreadId
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Posix.Types (Fd)
import System.Mem.Weak (Weak)
import Control.Concurrent (ThreadId)
import qualified Control.Concurrent as C
import Control.Monad.IO.Unlift
import UnliftIO.MVar
import UnliftIO.Chan
import UnliftIO.Exception (throwTo, SomeException)

-- | Lifted version of 'C.myThreadId'.
--
-- @since 0.1.1.0
myThreadId :: MonadIO m => m ThreadId
myThreadId = liftIO C.myThreadId
{-# INLINABLE myThreadId #-}

-- | Unlifted version of 'C.forkIO'.
--
-- @since 0.1.1.0
forkIO :: MonadUnliftIO m => m () -> m ThreadId
forkIO m = withRunInIO $ \run -> C.forkIO $ run m
{-# INLINABLE forkIO #-}

-- | Unlifted version of 'C.forkIOWithUnmask'.
--
-- @since 0.1.1.0
forkWithUnmask :: MonadUnliftIO m => ((forall a. m a -> m a) -> m ()) -> m ThreadId
forkWithUnmask m =
  withRunInIO $ \run -> C.forkIOWithUnmask $ \unmask -> run $ m $ liftIO . unmask . run
{-# INLINABLE forkWithUnmask #-}

-- | Unlifted version of 'C.forkFinally'.
--
-- @since 0.1.1.0
forkFinally :: MonadUnliftIO m => m a -> (Either SomeException a -> m ()) -> m ThreadId
forkFinally m1 m2 = withRunInIO $ \run -> C.forkFinally (run m1) $ run . m2
{-# INLINABLE forkFinally #-}

-- | Lifted version of 'C.killThread'.
--
-- @since 0.1.1.0
killThread :: MonadIO m => ThreadId -> m ()
killThread = liftIO . C.killThread
{-# INLINABLE  killThread #-}

-- | Unlifted version of 'C.forkOn'.
--
-- @since 0.1.1.0
forkOn :: MonadUnliftIO m => Int -> m () -> m ThreadId
forkOn i m = withRunInIO $ \run -> C.forkOn i $ run m
{-# INLINABLE forkOn #-}

-- | Unlifted version of 'C.forkOnWithUnmask'.
--
-- @since 0.1.1.0
forkOnWithUnmask :: MonadUnliftIO m => Int -> ((forall a. m a -> m a) -> m ()) -> m ThreadId
forkOnWithUnmask i m =
  withRunInIO $ \run -> C.forkOnWithUnmask i $ \unmask -> run $ m $ liftIO . unmask . run
{-# INLINABLE forkOnWithUnmask #-}

-- | Lifted version of 'C.getNumCapabilities'.
--
-- @since 0.1.1.0
getNumCapabilities :: MonadIO m => m Int
getNumCapabilities = liftIO C.getNumCapabilities
{-# INLINABLE getNumCapabilities #-}

-- | Lifted version of 'C.setNumCapabilities'.
--
-- @since 0.1.1.0
setNumCapabilities :: MonadIO m => Int -> m ()
setNumCapabilities = liftIO . C.setNumCapabilities
{-# INLINABLE setNumCapabilities #-}

-- | Lifted version of 'C.threadCapability'.
--
-- @since 0.1.1.0
threadCapability :: MonadIO m => ThreadId -> m (Int, Bool)
threadCapability = liftIO . C.threadCapability
{-# INLINABLE threadCapability #-}

-- | Lifted version of 'C.yield'.
--
-- @since 0.1.1.0
yield :: MonadIO m => m ()
yield = liftIO C.yield
{-# INLINABLE yield #-}

-- | Lifted version of 'C.threadDelay'.
--
-- @since 0.1.1.0
threadDelay :: MonadIO m => Int -> m ()
threadDelay = liftIO .  C.threadDelay
{-# INLINABLE threadDelay #-}

-- | Lifted version of 'C.threadWaitRead'.
--
-- @since 0.1.1.0
threadWaitRead :: MonadIO m => Fd -> m ()
threadWaitRead = liftIO . C.threadWaitRead
{-# INLINABLE threadWaitRead #-}

-- | Lifted version of 'C.threadWaitWrite'.
--
-- @since 0.1.1.0
threadWaitWrite :: MonadIO m => Fd -> m ()
threadWaitWrite = liftIO . C.threadWaitWrite
{-# INLINABLE threadWaitWrite #-}

-- | Unflifted version of 'C.forkOS'.
--
-- @since 0.1.1.0
forkOS :: MonadUnliftIO m => m () -> m ThreadId
forkOS m = withRunInIO $ \run -> C.forkOS $ run m
{-# INLINABLE forkOS #-}

-- | Lifted version of 'C.isCurrentThreadBound'.
--
-- @since 0.1.1.0
isCurrentThreadBound :: MonadIO m => m Bool
isCurrentThreadBound = liftIO C.isCurrentThreadBound
{-# INLINABLE isCurrentThreadBound #-}

-- | Unlifted version of 'C.runInBoundThread'.
--
-- @since 0.1.1.0
runInBoundThread :: MonadUnliftIO m => m a -> m a
runInBoundThread m = withRunInIO $ \run -> C.runInBoundThread $ run m
{-# INLINABLE runInBoundThread #-}

-- | Unlifted version of 'C.runInUnboundThread'.
--
-- @since 0.1.1.0
runInUnboundThread :: MonadUnliftIO m => m a -> m a
runInUnboundThread m = withRunInIO $ \run -> C.runInUnboundThread $ run m
{-# INLINABLE runInUnboundThread #-}

-- | Lifted version of 'C.mkWeakThreadId'.
--
-- @since 0.1.1.0
mkWeakThreadId :: MonadIO m => ThreadId -> m (Weak ThreadId)
mkWeakThreadId = liftIO . C.mkWeakThreadId
{-# INLINABLE mkWeakThreadId #-}
