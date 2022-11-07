{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
module UnliftIO.IO.File.Posix
  ( withBinaryFileDurable
  , withBinaryFileDurableAtomic
  , withBinaryFileAtomic
  , ensureFileDurable
  )
  where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad (forM_, guard, unless, void, when)
import Control.Monad.IO.Unlift
import Data.Bits (Bits, (.|.))
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Typeable (cast)
import Foreign (allocaBytes)
import Foreign.C (CInt(..), throwErrnoIfMinus1, throwErrnoIfMinus1Retry,
                  throwErrnoIfMinus1Retry_)
import GHC.IO.Device (IODeviceType(RegularFile))
import qualified GHC.IO.Device as Device
import GHC.IO.Exception (IOErrorType(UnsupportedOperation))
import qualified GHC.IO.FD as FD
import qualified GHC.IO.Handle.FD as HandleFD
import qualified GHC.IO.Handle.Types as HandleFD (Handle(..), Handle__(..))
import System.Directory (removeFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (Handle, IOMode(..), SeekMode(..), hGetBuf, hPutBuf,
                  openBinaryTempFile)
import System.IO.Error (ioeGetErrorType, isAlreadyExistsError,
                        isDoesNotExistError)
import qualified System.Posix.Files as Posix
import System.Posix.Internals (CFilePath, c_close, c_safe_open, withFilePath)
import System.Posix.Types (CMode(..), Fd(..), FileMode)
import UnliftIO.Exception
import UnliftIO.IO
import UnliftIO.MVar

-- NOTE: System.Posix.Internal doesn't re-export this constants so we have to
-- recreate-them here

newtype CFlag =
  CFlag CInt
  deriving (Eq, Show, Bits)

foreign import ccall unsafe "HsBase.h __hscore_o_rdonly" o_RDONLY :: CFlag
foreign import ccall unsafe "HsBase.h __hscore_o_wronly" o_WRONLY :: CFlag
foreign import ccall unsafe "HsBase.h __hscore_o_rdwr"   o_RDWR   :: CFlag
foreign import ccall unsafe "HsBase.h __hscore_o_append" o_APPEND :: CFlag
foreign import ccall unsafe "HsBase.h __hscore_o_creat"  o_CREAT  :: CFlag
foreign import ccall unsafe "HsBase.h __hscore_o_noctty" o_NOCTTY :: CFlag

-- After here, we have our own imports

-- On non-Linux operating systems that do not support `O_TMPFILE` the value of
-- `o_TMPFILE` will be 0, which is then used to fallback onto a different
-- implementation of temporary files.
foreign import ccall unsafe "file-posix.c unliftio_o_tmpfile" o_TMPFILE :: CFlag


-- | Whenever Operating System does not support @O_TMPFILE@ flag and anonymous
-- temporary files then `o_TMPFILE` flag will be set to @0@
o_TMPFILE_not_supported :: CFlag
o_TMPFILE_not_supported = CFlag 0

newtype CAt = CAt
  { unCAt :: CInt
  } deriving (Eq, Show, Bits)

foreign import ccall unsafe "file-posix.c unliftio_at_fdcwd" at_FDCWD :: CAt
foreign import ccall unsafe "file-posix.c unliftio_at_symlink_follow" at_SYMLINK_FOLLOW :: CAt
foreign import ccall unsafe "file-posix.c unliftio_s_irusr" s_IRUSR :: CMode
foreign import ccall unsafe "file-posix.c unliftio_s_iwusr" s_IWUSR :: CMode

c_open :: CFilePath -> CFlag -> CMode -> IO CInt
c_open fp (CFlag flags) = c_safe_open fp flags

foreign import ccall safe "fcntl.h openat"
  c_safe_openat :: CInt -> CFilePath -> CInt -> CMode -> IO CInt

c_openat :: DirFd -> CFilePath -> CFlag -> CMode -> IO CInt
c_openat (DirFd (Fd fd)) fp (CFlag flags) = c_safe_openat fd fp flags

foreign import ccall safe "fcntl.h renameat"
  c_safe_renameat :: CInt -> CFilePath -> CInt -> CFilePath -> IO CInt

c_renameat :: DirFd -> CFilePath -> DirFd -> CFilePath -> IO CInt
c_renameat (DirFd (Fd fdFrom)) cFpFrom (DirFd (Fd fdTo)) cFpTo =
  c_safe_renameat fdFrom cFpFrom fdTo cFpTo

foreign import ccall safe "unistd.h fsync"
  c_safe_fsync :: CInt -> IO CInt

c_fsync :: Fd -> IO CInt
c_fsync (Fd fd) = c_safe_fsync fd

foreign import ccall safe "unistd.h linkat"
  c_safe_linkat :: CInt -> CFilePath -> CInt -> CFilePath -> CInt -> IO CInt

c_linkat :: CAt -> CFilePath -> Either DirFd CAt -> CFilePath -> CAt -> IO CInt
c_linkat cat oldPath eNewDir newPath (CAt flags) =
  c_safe_linkat (unCAt cat) oldPath newDir newPath flags
  where
    unFd (Fd fd) = fd
    newDir = either (unFd . unDirFd) unCAt eNewDir

std_flags, output_flags, read_flags, write_flags, rw_flags,
    append_flags :: CFlag
std_flags    = o_NOCTTY
output_flags = std_flags    .|. o_CREAT
read_flags   = std_flags    .|. o_RDONLY
write_flags  = output_flags .|. o_WRONLY
rw_flags     = output_flags .|. o_RDWR
append_flags = write_flags  .|. o_APPEND

ioModeToFlags :: IOMode -> CFlag
ioModeToFlags iomode =
  case iomode of
    ReadMode      -> read_flags
    WriteMode     -> write_flags
    ReadWriteMode -> rw_flags
    AppendMode    -> append_flags

newtype DirFd = DirFd
  { unDirFd :: Fd
  } deriving (Eq, Ord, Show)

-- | For `DirFd`s that we obtain from dir paths, keep the path around
-- so we can use it in helpful error messages when failing to create
-- files inside the dir.
data DirFdWithPath = DirFdWithPath
  { dirFdWithPathDirFd :: !DirFd
  , dirFdWithPathPath :: !FilePath
  } deriving (Eq, Ord, Show)

-- | Returns a low-level file descriptor for a directory path. This function
-- exists given the fact that 'openFile' does not work with directories.
--
-- If you use this function, make sure you are working on a masked state,
-- otherwise async exceptions may leave file descriptors open.
openDir :: MonadIO m => FilePath -> m Fd
openDir fp
  -- TODO: Investigate what is the situation with Windows FS in regards to non_blocking
  -- NOTE: File operations _do not support_ non_blocking on various kernels, more
  -- info can be found here: https://ghc.haskell.org/trac/ghc/ticket/15153
 =
  liftIO $
  withFilePath fp $ \cFp ->
    Fd <$>
    throwErrnoIfMinus1Retry
      ("openDir: " ++ fp)
      (c_open cFp (ioModeToFlags ReadMode) 0o660)

-- | Closes a 'Fd' that points to a Directory.
closeDirectory :: MonadIO m => DirFdWithPath -> m ()
closeDirectory (DirFdWithPath (DirFd (Fd dirFd)) dirPath) =
  liftIO $
  throwErrnoIfMinus1Retry_ ("closeDirectory: " ++ dirPath) $ c_close dirFd

-- | Executes the low-level C function fsync on a C file descriptor
fsyncFileDescriptor
  :: MonadIO m
  => String -- ^ Meta-description for error messages
  -> Fd   -- ^ C File Descriptor
  -> m ()
fsyncFileDescriptor name fd =
  liftIO $ void $ throwErrnoIfMinus1 ("fsync - " ++ name) $ c_fsync fd

-- | Call @fsync@ on the file handle. Accepts an arbitary string for error reporting.
fsyncFileHandle :: String -> Handle -> IO ()
fsyncFileHandle errorPrefix hdl = withHandleFd hdl (fsyncFileDescriptor (errorPrefix ++ "/File"))


-- | Call @fsync@ on the opened directory file descriptor. Accepts an arbitary
-- string for error reporting.
fsyncDirectoryFd :: String -> DirFdWithPath -> IO ()
fsyncDirectoryFd errorPrefix (DirFdWithPath (DirFd fd) dirPath) =
  fsyncFileDescriptor (errorPrefix ++ ": " ++ dirPath) fd


-- | Opens a file from a directory, using this function in favour of a regular
-- 'openFile' guarantees that any file modifications are kept in the same
-- directory where the file was opened. An edge case scenario is a mount
-- happening in the directory where the file was opened while your program is
-- running.
--
-- If you use this function, make sure you are working on an masked state,
-- otherwise async exceptions may leave file descriptors open.
--
openFileFromDir :: MonadIO m => DirFdWithPath -> FilePath -> IOMode -> m Handle
openFileFromDir (DirFdWithPath dirFd dirPath) filePath@(takeFileName -> fileName) iomode =
  liftIO $
  withFilePath fileName $ \cFileName ->
    bracketOnError
      (do fileFd <-
            throwErrnoIfMinus1Retry ("openFileFromDir: " ++ dirPath) $
            c_openat dirFd cFileName (ioModeToFlags iomode) 0o666
            {- Can open directory with read only -}
          FD.mkFD
            fileFd
            iomode
            Nothing {- no stat -}
            False {- not a socket -}
            False {- non_blocking -}
           `onException`
            c_close fileFd)
      (liftIO . Device.close . fst)
      (\(fD, fd_type)
         -- we want to truncate() if this is an open in WriteMode, but only if the
         -- target is a RegularFile. ftruncate() fails on special files like
         -- /dev/null.
        -> do
         when (iomode == WriteMode && fd_type == RegularFile) $
           Device.setSize fD 0
         HandleFD.mkHandleFromFD fD fd_type filePath iomode False Nothing)


-- | Similar to `openFileFromDir`, but will open an anonymous (nameless)
-- temporary file in the supplied directory
openAnonymousTempFileFromDir ::
     MonadIO m =>
     Maybe DirFdWithPath
     -- ^ If a file descriptor is given for the directory where the target file is/will be
     -- located in, then it will be used for opening an anonymous file. Otherwise
     -- anonymous will be opened unattached to any file path.
     -> FilePath
     -- ^ File path of the target file that we are working on.
     -> IOMode
     -> m Handle
openAnonymousTempFileFromDir mDirFd filePath iomode =
  liftIO $
  case mDirFd of
    Just (DirFdWithPath dirFd _) ->
      withFilePath "." (openAnonymousWith . c_openat dirFd)
    Nothing ->
      withFilePath (takeDirectory filePath) (openAnonymousWith . c_open)
  where
    fdName = "openAnonymousTempFileFromDir - " ++ filePath
    dirPath = case mDirFd of
      Just (DirFdWithPath _ dirPath) -> dirPath
      Nothing -> takeDirectory filePath
    ioModeToTmpFlags :: IOMode -> CFlag
    ioModeToTmpFlags =
      \case
        ReadMode -> o_RDWR -- It is an error to create a O_TMPFILE with O_RDONLY
        ReadWriteMode -> o_RDWR
        _ -> o_WRONLY
    openAnonymousWith fopen =
      bracketOnError
        (do fileFd <-
              throwErrnoIfMinus1Retry ("openAnonymousTempFileFromDir: " ++ dirPath) $
              fopen (o_TMPFILE .|. ioModeToTmpFlags iomode) (s_IRUSR .|. s_IWUSR)
            FD.mkFD
              fileFd
              iomode
              Nothing {- no stat -}
              False {- not a socket -}
              False {- non_blocking -}
             `onException`
              c_close fileFd)
        (liftIO . Device.close . fst)
        (\(fD, fd_type) ->
           HandleFD.mkHandleFromFD fD fd_type fdName iomode False Nothing)


atomicDurableTempFileRename ::
     DirFdWithPath -> Maybe FileMode -> Handle -> Maybe FilePath -> FilePath -> IO ()
atomicDurableTempFileRename mDirFd mFileMode tmpFileHandle mTmpFilePath filePath = do
  fsyncFileHandle "atomicDurableTempFileCreate" tmpFileHandle
  -- at this point we know that the content has been persisted to the storage it
  -- is safe to do the atomic move/replace
  let eTmpFile = maybe (Left tmpFileHandle) Right mTmpFilePath
  atomicTempFileRename (Just mDirFd) mFileMode eTmpFile filePath
  -- Important to close the handle, so the we can fsync the directory
  hClose tmpFileHandle
  -- file path is updated, now we can fsync the directory
  fsyncDirectoryFd "atomicDurableTempFileCreate" mDirFd


-- | There will be an attempt to atomically convert an invisible temporary file
-- into a target file at the supplied file path. In case when there is already a
-- file at that file path, a new visible temporary file will be created in the
-- same folder and then atomically renamed into the target file path, replacing
-- any existing file. This is necessary since `c_safe_linkat` cannot replace
-- files atomically and we have to fall back onto `c_safe_renameat`. This should
-- not be a problem in practice, since lifetime of such visible file is
-- extremely short and it will be cleaned up regardless of the outcome of the
-- rename.
--
-- It is important to note, that whenever a file descriptor for the containing
-- directory is supplied, renaming and linking will be done in its context,
-- thus allowing to do proper fsyncing if durability is necessary.
--
-- __NOTE__: this function will work only on Linux.
--
atomicTempFileCreate ::
     Maybe DirFdWithPath
  -- ^ Possible handle for the directory where the target file is located. Which
  -- means that the file is already in that directory, just without a name. In other
  -- words it was opened before with `openAnonymousTempFileFromDir`
  -> Maybe FileMode
  -- ^ If file permissions are supplied they will be set on the new file prior
  -- to atomic rename.
  -> Handle
  -- ^ Handle to the anonymous temporary file created with `c_openat` and
  -- `o_TMPFILE`
  -> FilePath
  -- ^ File path for the target file.
  -> IO ()
atomicTempFileCreate mDirFd mFileMode tmpFileHandle filePath =
  withHandleFd tmpFileHandle $ \fd@(Fd cFd) ->
    withFilePath ("/proc/self/fd/" ++ show cFd) $ \cFromFilePath ->
      withFilePath filePathForSyscall $ \cToFilePath -> do
        let fileMode = fromMaybe Posix.stdFileMode mFileMode
        -- work around for the glibc bug: https://sourceware.org/bugzilla/show_bug.cgi?id=17523
        Posix.setFdMode fd fileMode
        let safeLink which to =
              throwErrnoIfMinus1Retry_
                ("atomicFileCreate - c_safe_linkat - " ++ which) $
              -- see `man linkat` and `man openat` for more info
              c_linkat at_FDCWD cFromFilePath cDirFd to at_SYMLINK_FOLLOW
        eExc <-
          tryJust (guard . isAlreadyExistsError) $
          safeLink "anonymous" cToFilePath
        case eExc of
          Right () -> pure ()
          Left () ->
            withBinaryTempFileFor filePath $ \visTmpFile visTmpFileHandle -> do
              hClose visTmpFileHandle
              removeFile visTmpFile
              case mDirFd of
                Nothing -> do
                  withFilePath visTmpFile (safeLink "visible")
                  Posix.rename visTmpFile filePath
                Just (DirFdWithPath dirFd dirPath) -> do
                  let !visTmpFileName = takeFileName visTmpFile
                  withFilePath visTmpFileName $ \cVisTmpFile -> do
                    safeLink "visible" cVisTmpFile
                    throwErrnoIfMinus1Retry_
                        ("atomicFileCreate - c_safe_renameat: " ++ dirPath ++ "/(" ++ visTmpFileName ++ " -> " ++ filePathForSyscall ++ ")") $
                      c_renameat dirFd cVisTmpFile dirFd cToFilePath
  where
    (cDirFd, filePathForSyscall) =
      case mDirFd of
        Nothing -> (Right at_FDCWD, filePath)
        Just (DirFdWithPath dirFd _) -> (Left dirFd, takeFileName filePath)

atomicTempFileRename ::
     Maybe DirFdWithPath
     -- ^ Possible handle for the directory where the target file is located.
  -> Maybe FileMode
  -- ^ If file permissions are supplied they will be set on the new file prior
  -- to atomic rename.
  -> Either Handle FilePath
  -- ^ Temporary file. If a handle is supplied, it means it was opened with
  -- @O_TMPFILE@ flag and thus we are on the Linux OS and can safely call
  -- `atomicTempFileCreate`
  -> FilePath
  -- ^ File path for the target file. Whenever `DirFdWithPath` is supplied, it must be
  -- the containgin directory fo this file, but that invariant is not enforced
  -- within this function.
  -> IO ()
atomicTempFileRename mDirFd mFileMode eTmpFile filePath =
  case eTmpFile of
    Left tmpFileHandle ->
      atomicTempFileCreate mDirFd mFileMode tmpFileHandle filePath
    Right tmpFilePath -> do
      forM_ mFileMode $ \fileMode -> Posix.setFileMode tmpFilePath fileMode
      case mDirFd of
        Nothing -> Posix.rename tmpFilePath filePath
        Just (DirFdWithPath dirFd dirPath) -> do
          let !fileName = takeFileName filePath
          let !tmpFileName = takeFileName tmpFilePath
          withFilePath fileName $ \cToFilePath ->
            withFilePath tmpFileName $ \cTmpFilePath ->
              throwErrnoIfMinus1Retry_ ("atomicFileCreate - c_safe_renameat: " ++ dirPath ++ "/(" ++ tmpFileName ++ " -> " ++ fileName ++ ")") $
              c_renameat dirFd cTmpFilePath dirFd cToFilePath


withDirectory :: MonadUnliftIO m => FilePath -> (DirFdWithPath -> m a) -> m a
withDirectory dirPath =
  bracket
    (do
      fd <- openDir dirPath
      pure $! DirFdWithPath (DirFd fd) dirPath
    )
    closeDirectory

withFileInDirectory ::
     MonadUnliftIO m => DirFdWithPath -> FilePath -> IOMode -> (Handle -> m a) -> m a
withFileInDirectory dirFd filePath iomode =
  bracket (openFileFromDir dirFd filePath iomode) hClose


-- | Create a temporary file for a matching possibly exiting target file that
-- will be replaced in the future. Temporary file is meant to be renamed
-- afterwards, thus it is only deleted upon error.
--
-- __Important__: Temporary file is not removed and file handle is not closed if
-- there was no exception thrown by the supplied action.
withBinaryTempFileFor ::
     MonadUnliftIO m
  => FilePath
  -- ^ "For" file. It may exist or may not.
  -> (FilePath -> Handle -> m a)
  -> m a
withBinaryTempFileFor filePath action =
  bracketOnError
    (liftIO (openBinaryTempFile dirPath tmpFileName))
    (\(tmpFilePath, tmpFileHandle) ->
        hClose tmpFileHandle >> liftIO (tryIO (removeFile tmpFilePath)))
    (uncurry action)
  where
    dirPath = takeDirectory filePath
    fileName = takeFileName filePath
    tmpFileName = "." ++ fileName ++ ".tmp"

-- | Returns `Nothing` if anonymous temporary file is not supported by the OS or
-- the underlying file system can't handle that feature.
withAnonymousBinaryTempFileFor ::
     MonadUnliftIO m
  => Maybe DirFdWithPath
  -- ^ It is possible to open the temporary file in the context of a directory,
  -- in such case supply its file descriptor. i.e. @openat@ will be used instead
  -- of @open@
  -> FilePath
  -- ^ "For" file. The file may exist or may not.
  -> IOMode
  -> (Handle -> m a)
  -> m (Maybe a)
withAnonymousBinaryTempFileFor mDirFd filePath iomode action
  | o_TMPFILE == o_TMPFILE_not_supported = pure Nothing
  | otherwise =
    trySupported $
    bracket (openAnonymousTempFileFromDir mDirFd filePath iomode) hClose action
  where
    trySupported m =
      tryIO m >>= \case
        Right res -> pure $ Just res
        Left exc
          | ioeGetErrorType exc == UnsupportedOperation -> pure Nothing
        Left exc -> throwIO exc

withNonAnonymousBinaryTempFileFor ::
     MonadUnliftIO m
  => Maybe DirFdWithPath
  -- ^ It is possible to open the temporary file in the context of a directory,
  -- in such case supply its file descriptor. i.e. @openat@ will be used instead
  -- of @open@
  -> FilePath
  -- ^ "For" file. The file may exist or may not.
  -> IOMode
  -> (FilePath -> Handle -> m a)
  -> m a
withNonAnonymousBinaryTempFileFor mDirFd filePath iomode action =
  withBinaryTempFileFor filePath $ \tmpFilePath tmpFileHandle -> do
    hClose tmpFileHandle
    case mDirFd of
      Nothing -> withBinaryFile tmpFilePath iomode (action tmpFilePath)
      Just dirFd -> withFileInDirectory dirFd tmpFilePath iomode (action tmpFilePath)

-- | Copy the contents of the file into the handle, but only if that file exists
-- and either `ReadWriteMode` or `AppendMode` is specified. Returned are the
-- file permissions of the original file so it can be set later when original
-- gets overwritten atomically.
copyFileHandle ::
     MonadUnliftIO f => IOMode -> FilePath -> Handle -> f (Maybe FileMode)
copyFileHandle iomode fromFilePath toHandle =
  either (const Nothing) Just <$>
  tryJust
    (guard . isDoesNotExistError)
    (do fileStatus <- liftIO $ Posix.getFileStatus fromFilePath
        -- Whenever we are not overwriting an existing file, we also need a
        -- copy of the file's contents
        unless (iomode == WriteMode) $ do
          withBinaryFile fromFilePath ReadMode (`copyHandleData` toHandle)
          unless (iomode == AppendMode) $ hSeek toHandle AbsoluteSeek 0
        -- Get the copy of source file permissions, but only whenever it exists
        pure $ Posix.fileMode fileStatus)


-- This is a copy of the internal function from `directory-1.3.3.2`. It became
-- available only in directory-1.3.3.0 and is still internal, hence the
-- duplication.
copyHandleData :: MonadIO m => Handle -> Handle -> m ()
copyHandleData hFrom hTo = liftIO $ allocaBytes bufferSize go
  where
    bufferSize = 131072 -- 128 KiB, as coreutils `cp` uses as of May 2014 (see ioblksize.h)
    go buffer = do
      count <- hGetBuf hFrom buffer bufferSize
      when (count > 0) $ do
        hPutBuf hTo buffer count
        go buffer

-- | Thread safe access to the file descriptor in the file handle
withHandleFd :: Handle -> (Fd -> IO a) -> IO a
withHandleFd h cb =
  case h of
    HandleFD.FileHandle _ mv ->
      withMVar mv $ \HandleFD.Handle__{HandleFD.haDevice = dev} ->
        case cast dev of
          Just fd -> cb $ Fd $ FD.fdFD fd
          Nothing -> error "withHandleFd: not a file handle"
    HandleFD.DuplexHandle {} -> error "withHandleFd: not a file handle"

-- | See `ensureFileDurable`
ensureFileDurable :: MonadIO m => FilePath -> m ()
ensureFileDurable filePath =
  liftIO $
  withDirectory (takeDirectory filePath) $ \dirFd ->
    withFileInDirectory dirFd filePath ReadMode $ \fileHandle ->
      liftIO $ do
        fsyncFileHandle "ensureFileDurablePosix" fileHandle
        -- NOTE: Here we are purposefully not fsyncing the directory if the file fails to fsync
        fsyncDirectoryFd "ensureFileDurablePosix" dirFd



-- | See `withBinaryFileDurable`
withBinaryFileDurable ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
withBinaryFileDurable filePath iomode action =
  case iomode of
    ReadMode
      -- We do not need to consider durable operations when we are in a
      -- 'ReadMode', so we can use a regular `withBinaryFile`
     -> withBinaryFile filePath iomode action
    _ {- WriteMode,  ReadWriteMode,  AppendMode -}
     ->
      withDirectory (takeDirectory filePath) $ \dirFd ->
        withFileInDirectory dirFd filePath iomode $ \tmpFileHandle -> do
          res <- action tmpFileHandle
          liftIO $ do
            fsyncFileHandle "withBinaryFileDurablePosix" tmpFileHandle
            -- NOTE: Here we are purposefully not fsyncing the directory if the file fails to fsync
            fsyncDirectoryFd "withBinaryFileDurablePosix" dirFd
          pure res

-- | See `withBinaryFileDurableAtomic`
withBinaryFileDurableAtomic ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
withBinaryFileDurableAtomic filePath iomode action =
  case iomode of
    ReadMode
      -- We do not need to consider an atomic operation when we are in a
      -- 'ReadMode', so we can use a regular `withBinaryFile`
     -> withBinaryFile filePath iomode action
    _ {- WriteMode,  ReadWriteMode,  AppendMode -}
     ->
      withDirectory (takeDirectory filePath) $ \dirFd -> do
        mRes <- withAnonymousBinaryTempFileFor (Just dirFd) filePath iomode $
          durableAtomicAction dirFd Nothing
        case mRes of
          Just res -> pure res
          Nothing ->
            withNonAnonymousBinaryTempFileFor (Just dirFd) filePath iomode $ \tmpFilePath ->
              durableAtomicAction dirFd (Just tmpFilePath)
  where
    durableAtomicAction dirFd mTmpFilePath tmpFileHandle = do
      mFileMode <- copyFileHandle iomode filePath tmpFileHandle
      res <- action tmpFileHandle
      liftIO $
        atomicDurableTempFileRename
          dirFd
          mFileMode
          tmpFileHandle
          mTmpFilePath
          filePath
      pure res

-- | See `withBinaryFileAtomic`
withBinaryFileAtomic ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
withBinaryFileAtomic filePath iomode action =
  case iomode of
    ReadMode
      -- We do not need to consider an atomic operation when we are in a
      -- 'ReadMode', so we can use a regular `withBinaryFile`
     -> withBinaryFile filePath iomode action
    _ {- WriteMode,  ReadWriteMode,  AppendMode -}
     -> do
      mRes <-
        withAnonymousBinaryTempFileFor Nothing filePath iomode $
        atomicAction Nothing
      case mRes of
        Just res -> pure res
        Nothing ->
          withNonAnonymousBinaryTempFileFor Nothing filePath iomode $ \tmpFilePath ->
            atomicAction (Just tmpFilePath)
  where
    atomicAction mTmpFilePath tmpFileHandle = do
      let eTmpFile = maybe (Left tmpFileHandle) Right mTmpFilePath
      mFileMode <- copyFileHandle iomode filePath tmpFileHandle
      res <- action tmpFileHandle
      liftIO $ atomicTempFileRename Nothing mFileMode eTmpFile filePath
      pure res

