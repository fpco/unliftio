-- | Unlifted "System.IO".
--
-- @since 0.1.0.0
module UnliftIO.IO
  ( IOMode (..)
  , Handle
  , IO.stdin
  , IO.stdout
  , IO.stderr
  , withFile
  , withBinaryFile
  , hClose
  , hFlush
  , hFileSize
  , hSetFileSize
  , hIsEOF
  , IO.BufferMode (..)
  , hSetBuffering
  , hGetBuffering
  , hSeek
  , IO.SeekMode
  , hTell
  , hIsOpen
  , hIsClosed
  , hIsReadable
  , hIsWritable
  , hIsSeekable
  , hIsTerminalDevice
  , hSetEcho
  , hGetEcho
  , hWaitForInput
  , hReady
  ) where

import qualified System.IO as IO
import System.IO (Handle, IOMode (..))
import Control.Monad.IO.Unlift

-- | Unlifted version of 'IO.withFile'.
--
-- @since 0.1.0.0
withFile :: MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m a) -> m a
withFile fp mode inner = withRunInIO $ \run -> IO.withFile fp mode $ run . inner

-- | Unlifted version of 'IO.withBinaryFile'.
--
-- @since 0.1.0.0
withBinaryFile :: MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m a) -> m a
withBinaryFile fp mode inner = withRunInIO $ \run -> IO.withBinaryFile fp mode $ run . inner

-- | Lifted version of 'IO.hClose'
--
-- @since 0.2.1.0
hClose :: MonadIO m => Handle -> m ()
hClose = liftIO . IO.hClose

-- | Lifted version of 'IO.hFlush'
--
-- @since 0.2.1.0
hFlush :: MonadIO m => Handle -> m ()
hFlush = liftIO . IO.hFlush

-- | Lifted version of 'IO.hFileSize'
--
-- @since 0.2.1.0
hFileSize :: MonadIO m => Handle -> m Integer
hFileSize = liftIO . IO.hFileSize

-- | Lifted version of 'IO.hSetFileSize'
--
-- @since 0.2.1.0
hSetFileSize :: MonadIO m => Handle -> Integer -> m ()
hSetFileSize h = liftIO . IO.hSetFileSize h

-- | Lifted version of 'IO.hIsEOF'
--
-- @since 0.2.1.0
hIsEOF :: MonadIO m => Handle -> m Bool
hIsEOF = liftIO . IO.hIsEOF

-- | Lifted version of 'IO.hSetBuffering'
--
-- @since 0.2.1.0
hSetBuffering :: MonadIO m => Handle -> IO.BufferMode -> m ()
hSetBuffering h = liftIO . IO.hSetBuffering h

-- | Lifted version of 'IO.hGetBuffering'
--
-- @since 0.2.1.0
hGetBuffering :: MonadIO m => Handle -> m IO.BufferMode
hGetBuffering = liftIO . IO.hGetBuffering

-- | Lifted version of 'IO.hSeek'
--
-- @since 0.2.1.0
hSeek :: MonadIO m => Handle -> IO.SeekMode -> Integer -> m ()
hSeek h s = liftIO . IO.hSeek h s

-- | Lifted version of 'IO.hTell'
--
-- @since 0.2.1.0
hTell :: MonadIO m => Handle -> m Integer
hTell = liftIO . IO.hTell

-- | Lifted version of 'IO.hIsOpen'
--
-- @since 0.2.1.0
hIsOpen :: MonadIO m => Handle -> m Bool
hIsOpen = liftIO . IO.hIsOpen

-- | Lifted version of 'IO.hIsClosed'
--
-- @since 0.2.1.0
hIsClosed :: MonadIO m => Handle -> m Bool
hIsClosed = liftIO . IO.hIsClosed

-- | Lifted version of 'IO.hIsReadable'
--
-- @since 0.2.1.0
hIsReadable :: MonadIO m => Handle -> m Bool
hIsReadable = liftIO . IO.hIsReadable

-- | Lifted version of 'IO.hIsWritable'
--
-- @since 0.2.1.0
hIsWritable :: MonadIO m => Handle -> m Bool
hIsWritable = liftIO . IO.hIsWritable

-- | Lifted version of 'IO.hIsSeekable'
--
-- @since 0.2.1.0
hIsSeekable :: MonadIO m => Handle -> m Bool
hIsSeekable = liftIO . IO.hIsSeekable

-- | Lifted version of 'IO.hIsTerminalDevice'
--
-- @since 0.2.1.0
hIsTerminalDevice :: MonadIO m => Handle -> m Bool
hIsTerminalDevice = liftIO . IO.hIsTerminalDevice

-- | Lifted version of 'IO.hSetEcho'
--
-- @since 0.2.1.0
hSetEcho :: MonadIO m => Handle -> Bool -> m ()
hSetEcho h = liftIO . IO.hSetEcho h

-- | Lifted version of 'IO.hGetEcho'
--
-- @since 0.2.1.0
hGetEcho :: MonadIO m => Handle -> m Bool
hGetEcho = liftIO . IO.hGetEcho

-- | Lifted version of 'IO.hWaitForInput'
--
-- @since 0.2.1.0
hWaitForInput :: MonadIO m => Handle -> Int -> m Bool
hWaitForInput h = liftIO . IO.hWaitForInput h

-- | Lifted version of 'IO.hReady'
--
-- @since 0.2.1.0
hReady :: MonadIO m => Handle -> m Bool
hReady = liftIO . IO.hReady
