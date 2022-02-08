{-# LANGUAGE CPP #-}
-- | Unlifted "System.Directory".
--
-- @since 0.2.6.0

module UnliftIO.Directory (
    -- * Actions on directories
    createDirectory
  , createDirectoryIfMissing
#if MIN_VERSION_directory(1,3,1)
  , createFileLink
  , createDirectoryLink
  , removeDirectoryLink
  , getSymbolicLinkTarget
#endif
  , removeDirectory
  , removeDirectoryRecursive
#if MIN_VERSION_directory(1,2,7)
  , removePathForcibly
#endif
  , renameDirectory
#if MIN_VERSION_directory(1,2,5)
  , listDirectory
#endif
  , getDirectoryContents

  -- ** Current working directory
  , getCurrentDirectory
  , setCurrentDirectory
#if MIN_VERSION_directory(1,2,3)
  , withCurrentDirectory
#endif

  -- * Pre-defined directories
  , getHomeDirectory
#if MIN_VERSION_directory(1,2,3)
  , XdgDirectory(..)
  , getXdgDirectory
#endif
#if MIN_VERSION_directory(1,3,2)
  , XdgDirectoryList(..)
  , getXdgDirectoryList
#endif
  , getAppUserDataDirectory
  , getUserDocumentsDirectory
  , getTemporaryDirectory

  -- * Actions on files
  , removeFile
  , renameFile
#if MIN_VERSION_directory(1,2,7)
  , renamePath
#endif
  , copyFile
#if MIN_VERSION_directory(1,2,6)
  , copyFileWithMetadata
#endif
  , canonicalizePath
#if MIN_VERSION_directory(1,2,2)
  , makeAbsolute
#endif
  , makeRelativeToCurrentDirectory
  , findExecutable
#if MIN_VERSION_directory(1,2,2)
  , findExecutables
#endif
#if MIN_VERSION_directory(1,2,4)
  , findExecutablesInDirectories
#endif
  , findFile
#if MIN_VERSION_directory(1,2,1)
  , findFiles
#endif
#if MIN_VERSION_directory(1,2,6)
  , findFileWith
#endif
#if MIN_VERSION_directory(1,2,1)
  , findFilesWith
#endif
#if MIN_VERSION_directory(1,2,4)
  , exeExtension
#endif
#if MIN_VERSION_directory(1,2,7)
  , getFileSize
#endif

  -- * Existence tests
#if MIN_VERSION_directory(1,2,7)
  , doesPathExist
#endif
  , doesFileExist
  , doesDirectoryExist

#if MIN_VERSION_directory(1,3,0)
  -- * Symbolic links
  , pathIsSymbolicLink
#endif

  -- * Permissions
  , Permissions
  , emptyPermissions
  , readable
  , writable
  , executable
  , searchable
  , setOwnerReadable
  , setOwnerWritable
  , setOwnerExecutable
  , setOwnerSearchable
  , getPermissions
  , setPermissions
  , copyPermissions

  -- * Timestamps
#if MIN_VERSION_directory(1,2,3)
  , getAccessTime
#endif
  , getModificationTime
#if MIN_VERSION_directory(1,2,3)
  , setAccessTime
  , setModificationTime
#endif
  ) where

import Control.Monad.IO.Unlift
import Data.Time.Clock
import qualified System.Directory as D
import System.Directory
  ( Permissions
#if MIN_VERSION_directory(1,2,3)
  , XdgDirectory(..)
#endif
#if MIN_VERSION_directory(1,3,2)
  , XdgDirectoryList(..)
#endif
  , emptyPermissions
#if MIN_VERSION_directory(1,2,4)
  , exeExtension
#endif
  , executable
  , readable
  , searchable
  , setOwnerExecutable
  , setOwnerReadable
  , setOwnerSearchable
  , setOwnerWritable
  , writable
  )

-- | Lifted 'D.createDirectory'.
--
-- @since 0.2.6.0
{-# INLINE createDirectory #-}
createDirectory :: MonadIO m => FilePath -> m ()
createDirectory = liftIO . D.createDirectory

-- | Lifted 'D.createDirectoryIfMissing'.
--
-- @since 0.2.6.0
{-# INLINE createDirectoryIfMissing #-}
createDirectoryIfMissing :: MonadIO m => Bool -> FilePath -> m ()
createDirectoryIfMissing create_parents path0 =
  liftIO (D.createDirectoryIfMissing create_parents path0)

#if MIN_VERSION_directory(1,3,1)
-- | Lifted 'D.createFileLink'.
-- directory package version should be >= 1.3.1.
-- @since 0.2.16.0
{-# INLINE createFileLink #-}
createFileLink
  :: MonadIO m
  => FilePath  -- ^ path to the target file
  -> FilePath  -- ^ path of the link to be created
  -> m ()
createFileLink targetPath linkPath =
  liftIO (D.createFileLink targetPath linkPath)

-- | Lifted 'D.createDirectoryLink'.
--
-- @since 0.2.21.0
createDirectoryLink :: MonadIO m => FilePath -> FilePath -> m ()
createDirectoryLink targetPath linkPath =
  liftIO (D.createDirectoryLink targetPath linkPath)

-- | Lifted 'D.removeDirectoryLink'.
--
-- @since 0.2.21.0
removeDirectoryLink :: MonadIO m => FilePath -> m ()
removeDirectoryLink linkPath =
  liftIO (D.removeDirectoryLink linkPath)

-- | Lifted 'D.getSymbolicLinkTarget'.
--
-- @since 0.2.21.0
getSymbolicLinkTarget :: MonadIO m => FilePath -> m FilePath
getSymbolicLinkTarget linkPath =
  liftIO (D.getSymbolicLinkTarget linkPath)
#endif

-- | Lifted 'D.removeDirectory'.
--
-- @since 0.2.6.0
{-# INLINE removeDirectory #-}
removeDirectory :: MonadIO m => FilePath -> m ()
removeDirectory = liftIO . D.removeDirectory

-- | Lifted 'D.removeDirectoryRecursive'.
--
-- @since 0.2.6.0
{-# INLINE removeDirectoryRecursive #-}
removeDirectoryRecursive :: MonadIO m => FilePath -> m ()
removeDirectoryRecursive = liftIO . D.removeDirectoryRecursive

#if MIN_VERSION_directory(1,2,7)
-- | Lifted 'D.removePathForcibly'.
--
-- @since 0.2.6.0
{-# INLINE removePathForcibly #-}
removePathForcibly :: MonadIO m => FilePath -> m ()
removePathForcibly = liftIO . D.removePathForcibly
#endif

-- | Lifted 'D.renameDirectory'.
--
-- @since 0.2.6.0
{-# INLINE renameDirectory #-}
renameDirectory :: MonadIO m => FilePath -> FilePath -> m ()
renameDirectory opath npath = liftIO (D.renameDirectory opath npath)

#if MIN_VERSION_directory(1,2,5)
-- | Lifted 'D.listDirectory'.
--
-- @since 0.2.6.0
{-# INLINE listDirectory #-}
listDirectory :: MonadIO m => FilePath -> m [FilePath]
listDirectory = liftIO . D.listDirectory
#endif

-- | Lifted 'D.getDirectoryContents'.
--
-- @since 0.2.6.0
{-# INLINE getDirectoryContents #-}
getDirectoryContents :: MonadIO m => FilePath -> m [FilePath]
getDirectoryContents = liftIO . D.getDirectoryContents

-- | Lifted 'D.getCurrentDirectory'.
--
-- @since 0.2.6.0
{-# INLINE getCurrentDirectory #-}
getCurrentDirectory :: MonadIO m => m FilePath
getCurrentDirectory = liftIO D.getCurrentDirectory

-- | Lifted 'D.setCurrentDirectory'.
--
-- @since 0.2.6.0
{-# INLINE setCurrentDirectory #-}
setCurrentDirectory :: MonadIO m => FilePath -> m ()
setCurrentDirectory = liftIO . D.setCurrentDirectory

#if MIN_VERSION_directory(1,2,3)
-- | Unlifted 'D.withCurrentDirectory'.
--
-- @since 0.2.6.0
{-# INLINE withCurrentDirectory #-}
withCurrentDirectory :: MonadUnliftIO m => FilePath -> m a -> m a
withCurrentDirectory dir action =
  withRunInIO (\u -> D.withCurrentDirectory dir (u action))
#endif

-- | Lifted 'D.getHomeDirectory'.
--
-- @since 0.2.6.0
{-# INLINE getHomeDirectory #-}
getHomeDirectory :: MonadIO m => m FilePath
getHomeDirectory = liftIO D.getHomeDirectory

#if MIN_VERSION_directory(1,2,3)
-- | Lifted 'D.getXdgDirectory'.
--
-- @since 0.2.6.0
{-# INLINE getXdgDirectory #-}
getXdgDirectory :: MonadIO m => XdgDirectory -> FilePath -> m FilePath
getXdgDirectory xdgDir suffix = liftIO (D.getXdgDirectory xdgDir suffix)
#endif

#if MIN_VERSION_directory(1,3,2)
-- | Lifted 'D.getXdgDirectoryList'.
--
-- @since 0.2.21.0
getXdgDirectoryList :: MonadIO m => XdgDirectoryList -> m [FilePath]
getXdgDirectoryList xdgDirectoryList =
  liftIO (D.getXdgDirectoryList xdgDirectoryList)
#endif

-- | Lifted 'D.getAppUserDataDirectory'.
--
-- @since 0.2.6.0
{-# INLINE getAppUserDataDirectory #-}
getAppUserDataDirectory :: MonadIO m => FilePath -> m FilePath
getAppUserDataDirectory = liftIO . D.getAppUserDataDirectory

-- | Lifted 'D.getUserDocumentsDirectory'.
--
-- @since 0.2.6.0
{-# INLINE getUserDocumentsDirectory #-}
getUserDocumentsDirectory :: MonadIO m => m FilePath
getUserDocumentsDirectory = liftIO D.getUserDocumentsDirectory

-- | Lifted 'D.getTemporaryDirectory'.
--
-- @since 0.2.6.0
{-# INLINE getTemporaryDirectory #-}
getTemporaryDirectory :: MonadIO m => m FilePath
getTemporaryDirectory = liftIO D.getTemporaryDirectory

-- | Lifted 'D.removeFile'.
--
-- @since 0.2.6.0
{-# INLINE removeFile #-}
removeFile :: MonadIO m => FilePath -> m ()
removeFile = liftIO . D.removeFile

-- | Lifted 'D.renameFile'.
--
-- @since 0.2.6.0
{-# INLINE renameFile #-}
renameFile :: MonadIO m => FilePath -> FilePath -> m ()
renameFile opath npath = liftIO (D.renameFile opath npath)

#if MIN_VERSION_directory(1,2,7)
-- | Lifted 'D.renamePath'.
--
-- @since 0.2.6.0
{-# INLINE renamePath #-}
renamePath :: MonadIO m => FilePath -> FilePath -> m ()
renamePath opath npath = liftIO (D.renamePath opath npath)
#endif

-- | Lifted 'D.copyFile'.
--
-- @since 0.2.6.0
{-# INLINE copyFile #-}
copyFile :: MonadIO m => FilePath -> FilePath -> m ()
copyFile fromFPath toFPath = liftIO (D.copyFile fromFPath toFPath)

#if MIN_VERSION_directory(1,2,6)
-- | Lifted 'D.copyFileWithMetadata'.
--
-- @since 0.2.6.0
{-# INLINE copyFileWithMetadata #-}
copyFileWithMetadata :: MonadIO m => FilePath -> FilePath -> m ()
copyFileWithMetadata src dst = liftIO (D.copyFileWithMetadata src dst)
#endif

-- | Lifted 'D.canonicalizePath'.
--
-- @since 0.2.6.0
{-# INLINE canonicalizePath #-}
canonicalizePath :: MonadIO m => FilePath -> m FilePath
canonicalizePath = liftIO . D.canonicalizePath

#if MIN_VERSION_directory(1,2,2)
-- | Lifted 'D.makeAbsolute'.
--
-- @since 0.2.6.0
{-# INLINE makeAbsolute #-}
makeAbsolute :: MonadIO m => FilePath -> m FilePath
makeAbsolute = liftIO . D.makeAbsolute
#endif

-- | Lifted 'D.makeRelativeToCurrentDirectory'.
--
-- @since 0.2.6.0
{-# INLINE makeRelativeToCurrentDirectory #-}
makeRelativeToCurrentDirectory :: MonadIO m => FilePath -> m FilePath
makeRelativeToCurrentDirectory = liftIO . D.makeRelativeToCurrentDirectory

-- | Lifted 'D.findExecutable'.
--
-- @since 0.2.6.0
{-# INLINE findExecutable #-}
findExecutable :: MonadIO m => String -> m (Maybe FilePath)
findExecutable = liftIO . D.findExecutable

#if MIN_VERSION_directory(1,2,2)
-- | Lifted 'D.findExecutables'.
--
-- @since 0.2.6.0
{-# INLINE findExecutables #-}
findExecutables :: MonadIO m => String -> m [FilePath]
findExecutables = liftIO . D.findExecutables
#endif

#if MIN_VERSION_directory(1,2,4)
-- | Lifted 'D.findExecutablesInDirectories'.
--
-- @since 0.2.6.0
{-# INLINE findExecutablesInDirectories #-}
findExecutablesInDirectories ::
     MonadIO m => [FilePath] -> String -> m [FilePath]
findExecutablesInDirectories path binary =
  liftIO (D.findExecutablesInDirectories path binary)
#endif

-- | Lifted 'D.findFile'.
--
-- @since 0.2.6.0
{-# INLINE findFile #-}
findFile :: MonadIO m => [FilePath] -> String -> m (Maybe FilePath)
findFile ds name = liftIO (D.findFile ds name)

#if MIN_VERSION_directory(1,2,1)
-- | Lifted 'D.findFiles'.
--
-- @since 0.2.6.0
{-# INLINE findFiles #-}
findFiles :: MonadIO m => [FilePath] -> String -> m [FilePath]
findFiles ds name = liftIO (D.findFiles ds name)
#endif

#if MIN_VERSION_directory(1,2,6)
-- | Unlifted 'D.findFileWith'.
--
-- @since 0.2.6.0
{-# INLINE findFileWith #-}
findFileWith ::
     MonadUnliftIO m
  => (FilePath -> m Bool)
  -> [FilePath]
  -> String
  -> m (Maybe FilePath)
findFileWith f ds name = withRunInIO (\u -> D.findFileWith (u . f) ds name)
#endif

#if MIN_VERSION_directory(1,2,1)
-- | Unlifted 'D.findFilesWith'.
--
-- @since 0.2.6.0
{-# INLINE findFilesWith #-}
findFilesWith ::
     MonadUnliftIO m
  => (FilePath -> m Bool)
  -> [FilePath]
  -> String
  -> m [FilePath]
findFilesWith f ds name = withRunInIO (\u -> D.findFilesWith (u . f) ds name)
#endif

#if MIN_VERSION_directory(1,2,7)
-- | Lifted 'D.getFileSize'.
--
-- @since 0.2.6.0
{-# INLINE getFileSize #-}
getFileSize :: MonadIO m => FilePath -> m Integer
getFileSize = liftIO . D.getFileSize
#endif

#if MIN_VERSION_directory(1,2,7)
-- | Lifted 'D.doesPathExist'.
--
-- @since 0.2.6.0
{-# INLINE doesPathExist #-}
doesPathExist :: MonadIO m => FilePath -> m Bool
doesPathExist = liftIO . D.doesPathExist
#endif

-- | Lifted 'D.doesFileExist'.
--
-- @since 0.2.6.0
{-# INLINE doesFileExist #-}
doesFileExist :: MonadIO m => FilePath -> m Bool
doesFileExist = liftIO . D.doesFileExist

-- | Lifted 'D.doesDirectoryExist'.
--
-- @since 0.2.6.0
{-# INLINE doesDirectoryExist #-}
doesDirectoryExist :: MonadIO m => FilePath -> m Bool
doesDirectoryExist = liftIO . D.doesDirectoryExist

#if MIN_VERSION_directory(1,3,0)
-- | Lifted 'D.pathIsSymbolicLink'.
--
-- @since 0.2.6.0
{-# INLINE pathIsSymbolicLink #-}
pathIsSymbolicLink :: MonadIO m => FilePath -> m Bool
pathIsSymbolicLink = liftIO . D.pathIsSymbolicLink
#endif

-- | Lifted 'D.getPermissions'.
--
-- @since 0.2.6.0
{-# INLINE getPermissions #-}
getPermissions :: MonadIO m => FilePath -> m Permissions
getPermissions = liftIO . D.getPermissions

-- | Lifted 'D.setPermissions'.
--
-- @since 0.2.6.0
{-# INLINE setPermissions #-}
setPermissions :: MonadIO m => FilePath -> Permissions -> m ()
setPermissions name p = liftIO (D.setPermissions name p)

-- | Lifted 'D.copyPermissions'.
--
-- @since 0.2.6.0
{-# INLINE copyPermissions #-}
copyPermissions :: MonadIO m => FilePath -> FilePath -> m ()
copyPermissions source dest = liftIO (D.copyPermissions source dest)

#if MIN_VERSION_directory(1,2,3)
-- | Lifted 'D.getAccessTime'.
--
-- @since 0.2.6.0
{-# INLINE getAccessTime #-}
getAccessTime :: MonadIO m => FilePath -> m UTCTime
getAccessTime = liftIO . D.getAccessTime
#endif

-- | Lifted 'D.getModificationTime'.
--
-- @since 0.2.6.0
{-# INLINE getModificationTime #-}
getModificationTime :: MonadIO m => FilePath -> m UTCTime
getModificationTime = liftIO . D.getModificationTime

#if MIN_VERSION_directory(1,2,3)
-- | Lifted 'D.setAccessTime'.
--
-- @since 0.2.6.0
{-# INLINE setAccessTime #-}
setAccessTime :: MonadIO m => FilePath -> UTCTime -> m ()
setAccessTime path atime = liftIO (D.setAccessTime path atime)

-- | Lifted 'D.setModificationTime'.
--
-- @since 0.2.6.0
setModificationTime :: MonadIO m => FilePath -> UTCTime -> m ()
setModificationTime path mtime = liftIO (D.setModificationTime path mtime)
#endif
