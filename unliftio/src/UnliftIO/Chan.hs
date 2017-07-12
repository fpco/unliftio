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
writeChan :: Chan a -> a -> IO ()
writeChan c = liftIO . C.writeChan c

-- | Lifted 'C.readChan'
--
-- @since 0.1.0.0
readChan :: Chan a -> IO a
readChan = liftIO . C.readChan

-- | Lifted 'C.dupChan'
--
-- @since 0.1.0.0
dupChan :: Chan a -> IO (Chan a)
dupChan = liftIO . C.dupChan

-- | Lifted 'C.getChanContents'
--
-- @since 0.1.0.0
getChanContents :: Chan a -> IO [a]
getChanContents = liftIO . C.getChanContents

-- | Lifted 'C.writeList2Chan'
--
-- @since 0.1.0.0
writeList2Chan :: Chan a -> [a] -> IO ()
writeList2Chan c = liftIO . C.writeList2Chan c
