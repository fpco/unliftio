-- | Unlifted "System.Process".
--
-- @since 0.2.4.0

module UnliftIO.Process (
  -- * Running sub-processes
  CreateProcess(..), CmdSpec(..), StdStream(..), ProcessHandle, createProcess, createProcess_, P.shell, P.proc

  -- ** Simpler functions for common tasks
  , callProcess, callCommand, spawnProcess, spawnCommand, readCreateProcess, readProcess, readCreateProcessWithExitCode, readProcessWithExitCode, withCreateProcess

  -- ** Related utilities
  , P.showCommandForUser

  -- * Process completion
  , waitForProcess, getProcessExitCode, terminateProcess, interruptProcessGroupOf

  -- * Interprocess communication
  , createPipe, createPipeFd
  ) where

import Control.Monad.IO.Unlift
import System.Exit
import System.IO
import System.Posix.Internals
import System.Process
  ( CmdSpec(..)
  , CreateProcess(..)
  , ProcessHandle
  , StdStream(..)
  )
import qualified System.Process as P

-- | Lifted 'P.createProcess'.
--
-- @since 0.2.4.0
{-# INLINE createProcess #-}
createProcess ::
     MonadIO m
  => CreateProcess
  -> m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess = liftIO . P.createProcess

-- | Lifted 'P.createProcess_'.
--
-- @since 0.2.4.0
{-# INLINE createProcess_ #-}
createProcess_ ::
     MonadIO m
  => String
  -> CreateProcess
  -> m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess_ msg proc_ = liftIO (P.createProcess_ msg proc_)

-- | Lifted 'P.callProcess'.
--
-- @since 0.2.4.0
{-# INLINE callProcess #-}
callProcess :: MonadIO m => FilePath -> [String] -> m ()
callProcess cmd args = liftIO (P.callProcess cmd args)

-- | Lifted 'P.callCommand'.
--
-- @since 0.2.4.0
{-# INLINE callCommand #-}
callCommand :: MonadIO m => String -> m ()
callCommand = liftIO . P.callCommand

-- | Lifted 'P.spawnProcess'.
--
-- @since 0.2.4.0
{-# INLINE spawnProcess #-}
spawnProcess :: MonadIO m => FilePath -> [String] -> m ProcessHandle
spawnProcess cmd args = liftIO (P.spawnProcess cmd args)

-- | Lifted 'P.spawnCommand'.
--
-- @since 0.2.4.0
{-# INLINE spawnCommand #-}
spawnCommand :: MonadIO m => String -> m ProcessHandle
spawnCommand = liftIO . P.spawnCommand

-- | Lifted 'P.readCreateProcess'.
--
-- @since 0.2.4.0
{-# INLINE readCreateProcess #-}
readCreateProcess :: MonadIO m => CreateProcess -> String -> m String
readCreateProcess cp input = liftIO (P.readCreateProcess cp input)

-- | Lifted 'P.readProcess'.
--
-- @since 0.2.4.0
{-# INLINE readProcess #-}
readProcess :: MonadIO m => FilePath -> [String] -> String -> m String
readProcess cmd args input = liftIO (P.readProcess cmd args input)

-- | Lifted 'P.readCreateProcessWithExitCode'.
--
-- @since 0.2.4.0
{-# INLINE readCreateProcessWithExitCode #-}
readCreateProcessWithExitCode ::
     MonadIO m => CreateProcess -> String -> m (ExitCode, String, String)
readCreateProcessWithExitCode cp input =
  liftIO (P.readCreateProcessWithExitCode cp input)

-- | Lifted 'P.readProcessWithExitCode'.
--
-- @since 0.2.4.0
{-# INLINE readProcessWithExitCode #-}
readProcessWithExitCode ::
     MonadIO m => FilePath -> [String] -> String -> m (ExitCode, String, String)
readProcessWithExitCode cmd args input =
  liftIO (P.readProcessWithExitCode cmd args input)

-- | Unlifted 'P.withCreateProcess'.
--
-- @since 0.2.4.0
{-# INLINE withCreateProcess #-}
withCreateProcess ::
     MonadUnliftIO m
  => CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> m a)
  -> m a
withCreateProcess c action =
  withRunInIO
    (\u ->
       P.withCreateProcess
         c
         (\stdin_h stdout_h stderr_h proc_h ->
            u (action stdin_h stdout_h stderr_h proc_h)))

-- | Lifted 'P.waitForProcess'.
--
-- @since 0.2.4.0
{-# INLINE waitForProcess #-}
waitForProcess :: MonadIO m => ProcessHandle -> m ExitCode
waitForProcess = liftIO . P.waitForProcess

-- | Lifted 'P.getProcessExitCode'.
--
-- @since 0.2.4.0
{-# INLINE getProcessExitCode #-}
getProcessExitCode :: MonadIO m => ProcessHandle -> m (Maybe ExitCode)
getProcessExitCode = liftIO . P.getProcessExitCode

-- | Lifted 'P.terminateProcess'.
--
-- @since 0.2.4.0
{-# INLINE terminateProcess #-}
terminateProcess :: MonadIO m => ProcessHandle -> m ()
terminateProcess = liftIO . P.terminateProcess

-- | Lifted 'P.interruptProcessGroupOf'.
--
-- @since 0.2.4.0
{-# INLINE interruptProcessGroupOf #-}
interruptProcessGroupOf :: MonadIO m => ProcessHandle -> m ()
interruptProcessGroupOf = liftIO . interruptProcessGroupOf

-- | Lifted 'P.createPipe'.
--
-- @since 0.2.4.0
{-# INLINE createPipe #-}
createPipe :: MonadIO m => m (Handle, Handle)
createPipe = liftIO P.createPipe

-- | Lifted 'P.createPipeFd'.
--
-- @since 0.2.4.0
{-# INLINE createPipeFd #-}
createPipeFd :: MonadIO m => m (FD, FD)
createPipeFd = liftIO P.createPipeFd
