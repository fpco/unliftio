{-# LANGUAgE CPP #-}
-- | Temporary file and directory support
--
-- Strongly inspired by/stolen from the temporary package
--
-- @since 0.1.0.0
module UnliftIO.Temporary
  ( withSystemTempFile
  , withSystemTempDirectory
  , withTempFile
  , withTempDirectory
  ) where

import Control.Monad.IO.Unlift
import Control.Monad (void)
import UnliftIO.Exception
import System.Directory
import System.IO (Handle, openTempFile, hClose)
import System.IO.Error
import System.Posix.Internals (c_getpid)
import System.FilePath ((</>))

#ifdef mingw32_HOST_OS
import System.Directory       ( createDirectory )
#else
import qualified System.Posix
#endif

-- | Create and use a temporary file in the system standard temporary directory.
--
-- Behaves exactly the same as 'withTempFile', except that the parent temporary directory
-- will be that returned by 'getCanonicalTemporaryDirectory'.
--
-- @since 0.1.0.0
withSystemTempFile :: MonadUnliftIO m =>
                      String   -- ^ File name template. See 'openTempFile'.
                   -> (FilePath -> Handle -> m a) -- ^ Callback that can use the file
                   -> m a
withSystemTempFile template action = liftIO getCanonicalTemporaryDirectory >>= \tmpDir -> withTempFile tmpDir template action

-- | Create and use a temporary directory in the system standard temporary directory.
--
-- Behaves exactly the same as 'withTempDirectory', except that the parent temporary directory
-- will be that returned by 'getCanonicalTemporaryDirectory'.
--
-- @since 0.1.0.0
withSystemTempDirectory :: MonadUnliftIO m =>
                           String   -- ^ Directory name template. See 'openTempFile'.
                        -> (FilePath -> m a) -- ^ Callback that can use the directory
                        -> m a
withSystemTempDirectory template action = liftIO getCanonicalTemporaryDirectory >>= \tmpDir -> withTempDirectory tmpDir template action


-- | Use a temporary filename that doesn't already exist.
--
-- Creates a new temporary file inside the given directory, making use of the
-- template. The temp file is deleted after use. For example:
--
-- > withTempFile "src" "sdist." $ \tmpFile hFile -> do ...
--
-- The @tmpFile@ will be file in the given directory, e.g.
-- @src/sdist.342@.
--
-- @since 0.1.0.0
withTempFile :: MonadUnliftIO m =>
                FilePath -- ^ Temp dir to create the file in
             -> String   -- ^ File name template. See 'openTempFile'.
             -> (FilePath -> Handle -> m a) -- ^ Callback that can use the file
             -> m a
withTempFile tmpDir template action =
  bracket
    (liftIO (openTempFile tmpDir template))
    (\(name, handle) -> liftIO (hClose handle >> ignoringIOErrors (removeFile name)))
    (uncurry action)

-- | Create and use a temporary directory.
--
-- Creates a new temporary directory inside the given directory, making use
-- of the template. The temp directory is deleted after use. For example:
--
-- > withTempDirectory "src" "sdist." $ \tmpDir -> do ...
--
-- The @tmpDir@ will be a new subdirectory of the given directory, e.g.
-- @src/sdist.342@.
--
-- @since 0.1.0.0
withTempDirectory :: MonadUnliftIO m =>
                     FilePath -- ^ Temp directory to create the directory in
                  -> String   -- ^ Directory name template. See 'openTempFile'.
                  -> (FilePath -> m a) -- ^ Callback that can use the directory
                  -> m a
withTempDirectory targetDir template =
  bracket
    (liftIO (createTempDirectory targetDir template))
    (liftIO . ignoringIOErrors . removeDirectoryRecursive)

-- | Return the absolute and canonical path to the system temporary
-- directory.
--
-- >>> setCurrentDirectory "/home/feuerbach/"
-- >>> setEnv "TMPDIR" "."
-- >>> getTemporaryDirectory
-- "."
-- >>> getCanonicalTemporaryDirectory
-- "/home/feuerbach"
getCanonicalTemporaryDirectory :: IO FilePath
getCanonicalTemporaryDirectory = getTemporaryDirectory >>= canonicalizePath

-- | Create a temporary directory. See 'withTempDirectory'.
createTempDirectory
  :: FilePath -- ^ Temp directory to create the directory in
  -> String -- ^ Directory name template
  -> IO FilePath
createTempDirectory dir template = do
  pid <- c_getpid
  findTempName pid
  where
    findTempName x = do
      let dirpath = dir </> template ++ show x
      r <- try $ mkPrivateDir dirpath
      case r of
        Right _ -> return dirpath
        Left  e | isAlreadyExistsError e -> findTempName (x+1)
                | otherwise              -> ioError e


mkPrivateDir :: String -> IO ()
#ifdef mingw32_HOST_OS
mkPrivateDir s = createDirectory s
#else
mkPrivateDir s = System.Posix.createDirectory s 0o700
#endif

ignoringIOErrors :: MonadUnliftIO m => m () -> m ()
ignoringIOErrors = void . tryIO
