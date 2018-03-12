-- | Unlifted "System.Environment".
--
-- @since 0.2.4.0
module UnliftIO.Environment
  ( getArgs
  , getProgName
  , getExecutablePath
  , getEnv
  , lookupEnv
  , setEnv
  , unsetEnv
  , withArgs
  , withProgName
  , getEnvironment
  ) where

import Control.Monad.IO.Unlift
import qualified System.Environment as E

-- | Lifted 'E.getArgs'.
--
-- @since 0.2.4.0
{-# INLINE getArgs #-}
getArgs :: MonadIO m => m [String]
getArgs = liftIO E.getArgs

-- | Lifted 'E.getProgName'.
--
-- @since 0.2.4.0
{-# INLINE getProgName #-}
getProgName :: MonadIO m => m String
getProgName = liftIO E.getProgName

-- | Lifted 'E.getExecutablePath'.
--
-- @since 0.2.4.0
{-# INLINE getExecutablePath #-}
getExecutablePath :: MonadIO m => m FilePath
getExecutablePath = liftIO E.getExecutablePath

-- | Lifted 'E.getEnv'.
--
-- @since 0.2.4.0
{-# INLINE getEnv #-}
getEnv :: MonadIO m => String -> m String
getEnv = liftIO . E.getEnv

-- | Lifted 'E.lookupEnv'.
--
-- @since 0.2.4.0
{-# INLINE lookupEnv #-}
lookupEnv :: MonadIO m => String -> m (Maybe String)
lookupEnv = liftIO . E.lookupEnv

-- | Lifted 'E.setEnv'.
--
-- @since 0.2.4.0
{-# INLINE setEnv #-}
setEnv :: MonadIO m => String -> String -> m ()
setEnv key_ value_ = liftIO (E.setEnv key_ value_)

-- | Lifted 'E.unsetEnv'.
--
-- @since 0.2.4.0
{-# INLINE unsetEnv #-}
unsetEnv :: MonadIO m => String -> m ()
unsetEnv = liftIO . E.unsetEnv

-- | Lifted 'E.withArgs'.
--
-- @since 0.2.4.0
{-# INLINE withArgs #-}
withArgs :: MonadUnliftIO m => [String] -> m a -> m a
withArgs xs act = withRunInIO (\u -> E.withArgs xs (u act))

-- | Lifted 'E.withProgName'.
--
-- @since 0.2.4.0
{-# INLINE withProgName #-}
withProgName :: MonadUnliftIO m => String -> m a -> m a
withProgName nm act = withRunInIO (\u -> E.withProgName nm (u act))

-- | Lifted 'E.getEnvironment'.
--
-- @since 0.2.4.0
{-# INLINE getEnvironment #-}
getEnvironment :: MonadIO m => m [(String, String)]
getEnvironment = liftIO E.getEnvironment
