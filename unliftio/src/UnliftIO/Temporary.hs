{-# LANGUAgE CPP #-}
-- | Temporary file and directory support.
--
-- Strongly inspired by\/stolen from the <https://github.com/feuerbach/temporary> package.
--
-- @since 0.1.0.0
--
-- === __Copyright notice:__
--
-- The following copyright notice is taken from <https://github.com/feuerbach/temporary>
-- and is reproduced here as part of license terms of that package, of which this module is
-- a derivate work.
--
-- @
-- Copyright
--   (c) 2003-2006, Isaac Jones
--   (c) 2005-2009, Duncan Coutts
--   (c) 2008, Maximilian Bolingbroke
--   ... and other contributors
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted
-- provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice, this list of
--       conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright notice, this list of
--       conditions and the following disclaimer in the documentation and/or other materials
--       provided with the distribution.
--     * Neither the name of Maximilian Bolingbroke nor the names of other contributors may be used to
--       endorse or promote products derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
-- IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
-- FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
-- IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- @
module UnliftIO.Temporary
  ( withSystemTempFile
  , withSystemTempDirectory
  , withTempFile
  , withTempDirectory
  ) where

import Control.Monad.IO.Unlift
import Control.Monad (liftM)
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
                        -> (FilePath -> m a) -- ^ Callback that can use the directory.
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
                FilePath -- ^ Temp dir to create the file in.
             -> String   -- ^ File name template. See 'openTempFile'.
             -> (FilePath -> Handle -> m a) -- ^ Callback that can use the file.
             -> m a
withTempFile tmpDir template action =
  bracket
    (liftIO (openTempFile tmpDir template))
    (\(name, handle') -> liftIO (hClose handle' >> ignoringIOErrors (removeFile name)))
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
                     FilePath -- ^ Temp directory to create the directory in.
                  -> String   -- ^ Directory name template. See 'openTempFile'.
                  -> (FilePath -> m a) -- ^ Callback that can use the directory.
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
  :: FilePath -- ^ Temp directory to create the directory in.
  -> String -- ^ Directory name template.
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
ignoringIOErrors = liftM (const ()) . tryIO -- yes, it's just void, but for pre-AMP GHCs
