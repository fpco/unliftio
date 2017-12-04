-- | Orphans instances.
--
-- @since 0.1.0.0
module UnliftIO.Instances () where

import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource.Internal (ResourceT (..))

-- | @since 0.1.0.0
instance MonadUnliftIO m => MonadUnliftIO (ResourceT m) where
  askUnliftIO = ResourceT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip unResourceT r))
