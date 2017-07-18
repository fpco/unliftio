-- | Lifted "Control.Concurrent.Chan"
--
-- @since 0.1.0.0
module UnliftIO.Chan
  ( Chan
  , newChan
  , writeChan
  , readChan
  , dupChan
  , getChanContents
  , writeList2Chan
  ) where

import Control.Monad.IO.Unlift
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as C

-- | Lifted 'C.newChan'
--
-- @since 0.1.0.0
newChan :: MonadIO m => m (Chan a)
newChan = liftIO C.newChan

-- | Lifted 'C.writeChan'
--
-- @since 0.1.0.0
writeChan :: MonadIO m => Chan a -> a -> m ()
writeChan c = liftIO . C.writeChan c

-- | Lifted 'C.readChan'
--
-- @since 0.1.0.0
readChan :: MonadIO m => Chan a -> m a
readChan = liftIO . C.readChan

-- | Lifted 'C.dupChan'
--
-- @since 0.1.0.0
dupChan :: MonadIO m => Chan a -> m (Chan a)
dupChan = liftIO . C.dupChan

-- | Lifted 'C.getChanContents'
--
-- @since 0.1.0.0
getChanContents :: MonadIO m => Chan a -> m [a]
getChanContents = liftIO . C.getChanContents

-- | Lifted 'C.writeList2Chan'
--
-- @since 0.1.0.0
writeList2Chan :: MonadIO m => Chan a -> [a] -> m ()
writeList2Chan c = liftIO . C.writeList2Chan c
