{-# LANGUAGE CPP #-}
{-|

== Rationale

This module offers functions to handle files that offer better durability and/or
atomicity.

== When to use functions in this module?

Given the usage of this functions comes at a cost in performance, it is important to
consider what are the use cases that are ideal for each of the functions.

=== Not Durable and not Atomic

For this use case, you want to use the regular functions:

* 'withBinaryFile'
* 'writeBinaryFile'

The regular use case for this scenario happens when your program is dealing with outputs
that are never going to be consumed again by your program. For example, imagine you have a
program that generates sales reports for the last month, this is a report that can be
generated quickly; you don't really care if the output file gets corrupted or lost at one
particular execution of your program given that is cheap to execute the data export
program a second time. In other words, your program doesn't /rely/ on the data contained
in this file in order to work.

=== Atomic but not Durable

 Imagine a scenario where your program builds a temporary file that serves as an
intermediate step to a bigger task, like Object files (@.o@) in a compilation process. The
program will use an existing @.o@ file if it is present, or it will build one from scratch
if it is not. The file is not really required, but if it is present, it *must* be valid
and consistent. In this situation, you care about atomicity, but not durability. You can
use the functions for such scenario:

* 'withBinaryFileAtomic'
* 'writeBinaryFileAtomic'

__Note__ - there is a peculiar difference between regular file writing functionality and
the one that is done atomically. Even if the orignal file is removed while it is being
modified, because of atomicity, it will be restored with all modifications, if any. The
reason for this is because a copy of the file was made prior to modifications and at the
end the existing is atomically replaced. An important consequence of this fact is that
whenever the folder containing the file which is being modified is removed, all bets are
off and all atomic functions will result in an exception.

=== Durable but not Atomic

For this use case, you want to use the functions:

* 'withBinaryFileDurable'
* 'writeBinaryFileDurable'

The regular use case for this scenario happens when your program deals with file
modifications that must be guaranteed to be durable, but you don't care that changes are
consistent. If you use this function, more than likely your program is ensuring
consistency guarantees through other means, for example, SQLite uses the Write Ahead Log
(WAL) algorithm to ensure changes are atomic at an application level.

=== Durable and Atomic

For this use case, you can use the functions:

* 'withBinaryFileDurableAtomic'
* 'writeBinaryFileDurableAtomic'

The regular use case for this scenario happens when you want to ensure that after a
program is executed, the modifications done to a file are guaranteed to be saved, and also
that changes are rolled-back in case there is a failure (e.g.  hard reboot, shutdown,
etc).

-}
module UnliftIO.IO.File
  ( writeBinaryFile
  , writeBinaryFileAtomic
  , writeBinaryFileDurable
  , writeBinaryFileDurableAtomic
  , withBinaryFile
  , withBinaryFileAtomic
  , withBinaryFileDurable
  , withBinaryFileDurableAtomic
  , ensureFileDurable
  , renameFileDurable
  )
  where

import Data.ByteString as B (ByteString, writeFile)
import Control.Monad.IO.Unlift
import UnliftIO.IO (Handle, IOMode(..), withBinaryFile)
import UnliftIO.Directory (renameFile)

#if WINDOWS


ensureFileDurable = (`seq` pure ())

writeBinaryFileDurable = writeBinaryFile
writeBinaryFileDurableAtomic = writeBinaryFile
writeBinaryFileAtomic = writeBinaryFile

withBinaryFileDurable = withBinaryFile
withBinaryFileDurableAtomic = withBinaryFile
withBinaryFileAtomic = withBinaryFile

renameFileDurable = renameFile

#else

import qualified Data.ByteString as B (hPut)
import qualified UnliftIO.IO.File.Posix as Posix

ensureFileDurable = Posix.ensureFileDurable

writeBinaryFileDurable fp bytes =
  liftIO $ withBinaryFileDurable fp WriteMode (`B.hPut` bytes)
writeBinaryFileDurableAtomic fp bytes =
  liftIO $ withBinaryFileDurableAtomic fp WriteMode (`B.hPut` bytes)
writeBinaryFileAtomic fp bytes =
  liftIO $ withBinaryFileAtomic fp WriteMode (`B.hPut` bytes)

withBinaryFileDurable = Posix.withBinaryFileDurable
withBinaryFileDurableAtomic = Posix.withBinaryFileDurableAtomic
withBinaryFileAtomic = Posix.withBinaryFileAtomic

renameFileDurable = Posix.renameFileDurable
#endif

-- | When a file is renamed, it is necessary to execute @fsync()@ on the
-- directory that contains the file now and afterwards on the directory where
-- the file was before, so that the rename is durable.
--
-- Remark: This is also atomic if both locations of the file are on the same
-- filesystem. However, it could happen that the operation leads to data loss,
-- if a crash happens after the rename and before the first fsync finishes. This
-- is because on an async filesystem the write of the old directory might
-- already written to disk and the change on the new directory is not. It the
-- function call returns, the change is durable. Nevertheless, this will not
-- happen on filesystems using journaling, that is, allmost all modern filesystems.
--
-- === Cross-Platform support 
--
-- This function is a noop on Windows platforms.
--
-- @since 0.2.14
renameFileDurable :: MonadIO m => FilePath -> FilePath -> m ()
-- Implementation is at the top of the module

-- | After a file is closed, this function opens it again and executes @fsync()@
-- internally on both the file and the directory that contains it. Note that this function
-- is intended to work around the non-durability of existing file APIs, as opposed to
-- being necessary for the API functions provided in this module.
--
-- [The effectiveness of calling this function is
-- debatable](https://stackoverflow.com/questions/37288453/calling-fsync2-after-close2/50158433#50158433),
-- as it relies on internal implementation details at the Kernel level that might
-- change. We argue that, despite this fact, calling this function may bring benefits in
-- terms of durability.
--
-- This function does not provide the same guarantee as if you would open and modify a
-- file using `withBinaryFileDurable` or `writeBinaryFileDurable`, since they ensure that
-- the @fsync()@ is called before the file is closed, so if possible use those instead.
--
-- === Cross-Platform support
--
-- This function is a noop on Windows platforms.
--
-- @since 0.2.12
ensureFileDurable :: MonadIO m => FilePath -> m ()
-- Implementation is at the top of the module


-- | Similar to 'writeBinaryFile', but it also ensures that changes executed to the file
-- are guaranteed to be durable. It internally uses @fsync()@ and makes sure it
-- synchronizes the file on disk.
--
-- === Cross-Platform support
--
-- This function behaves the same as 'RIO.writeBinaryFile' on Windows platforms.
--
-- @since 0.2.12
writeBinaryFileDurable :: MonadIO m => FilePath -> ByteString -> m ()
-- Implementation is at the top of the module

-- | Similar to 'writeBinaryFile', but it also guarantes that changes executed to the file
-- are durable, also, in case of failure, the modified file is never going to get
-- corrupted. It internally uses @fsync()@ and makes sure it synchronizes the file on
-- disk.
--
-- === Cross-Platform support
--
-- This function behaves the same as 'writeBinaryFile' on Windows platforms.
--
-- @since 0.2.12
writeBinaryFileDurableAtomic :: MonadIO m => FilePath -> ByteString -> m ()
-- Implementation is at the top of the module

-- | Same as 'writeBinaryFileDurableAtomic', except it does not guarantee durability.
--
-- === Cross-Platform support
--
-- This function behaves the same as 'writeBinaryFile' on Windows platforms.
--
-- @since 0.2.12
writeBinaryFileAtomic :: MonadIO m => FilePath -> ByteString -> m ()
-- Implementation is at the top of the module

-- | Opens a file with the following guarantees:
--
-- * It successfully closes the file in case of an asynchronous exception
--
-- * It reliably saves the file in the correct directory; including edge case situations
--   like a different device being mounted to the current directory, or the current
--   directory being renamed to some other name while the file is being used.
--
-- * It ensures durability by executing an @fsync()@ call before closing the file handle
--
-- === Cross-Platform support
--
-- This function behaves the same as 'System.IO.withBinaryFile' on Windows platforms.
--
-- @since 0.2.12
withBinaryFileDurable ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
-- Implementation is at the top of the module

-- | Opens a file with the following guarantees:
--
-- * It successfully closes the file in case of an asynchronous exception
--
-- * It reliably saves the file in the correct directory; including edge case situations
--   like a different device being mounted to the current directory, or the current
--   directory being renamed to some other name while the file is being used.
--
-- * It ensures durability by executing an @fsync()@ call before closing the file handle
--
-- * It keeps all changes in a temporary file, and after it is closed it atomically moves
--   the temporary file to the original filepath, in case of catastrophic failure, the
--   original file stays unaffected.
--
-- If you do not need durability but only atomicity, use `withBinaryFileAtomic` instead,
-- which is faster as it does not perform @fsync()@.
--
-- __Important__ - Make sure not to close the `Handle`, it will be closed for you,
-- otherwise it will result in @invalid argument (Bad file descriptor)@ exception.
--
-- === Performance Considerations
--
-- When using a writable but non-truncating 'IOMode' (i.e. 'ReadWriteMode' and
-- 'AppendMode'), this function performs a copy operation of the specified input file to
-- guarantee the original file is intact in case of a catastrophic failure (no partial
-- writes). This approach may be prohibitive in scenarios where the input file is expected
-- to be large in size.
--
-- === Cross-Platform support
--
-- This function behaves the same as 'System.IO.withBinaryFile' on Windows platforms.
--
-- @since 0.2.12
withBinaryFileDurableAtomic ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
-- Implementation is at the top of the module


-- | Perform an action on a new or existing file at the destination file path. If
-- previously the file existed at the supplied file path then:
--
-- * in case of `WriteMode` it will be overwritten
--
-- * upon `ReadWriteMode` or `AppendMode` files contents will be copied over into a
-- temporary file, thus making sure no corruption can happen to an existing file upon any
-- failures, even catastrophic one, yet its contents are availble for modification.
--
-- * There is nothing atomic about `ReadMode`, so no special treatment there.
--
-- It is similar to `withBinaryFileDurableAtomic`, but without the durability part. It
-- means that all modification can still disappear after it has been succesfully written
-- due to some extreme event like an abrupt power loss, but the contents will not be
-- corrupted in case when the file write did not end successfully.
--
-- The same performance caveats apply as for `withBinaryFileDurableAtomic` due to making a
-- copy of the content of existing files during non-truncating writes.
--
-- __Important__ - Do not close the handle, otherwise it will result in @invalid argument
-- (Bad file descriptor)@ exception
--
-- __Note__ - on Linux operating system and only with supported file systems an anonymous
-- temporary file will be used while working on the file (see @O_TMPFILE@ in @man
-- openat@). In case when such feature is not available or not supported a temporary file
-- ".target-file-nameXXX.ext.tmp", where XXX is some random number, will be created
-- alongside the target file in the same directory
--
-- @since 0.2.12
withBinaryFileAtomic ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
-- Implementation is at the top of the module


-- | Lifted version of `B.writeFile`
--
-- @since 0.2.12
writeBinaryFile :: MonadIO m => FilePath -> ByteString -> m ()
writeBinaryFile fp = liftIO . B.writeFile fp
