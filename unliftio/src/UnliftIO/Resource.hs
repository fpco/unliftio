-- | Unlifted "Control.Monad.Trans.Resource"
--
-- @since 0.1.0.0
module UnliftIO.Resource
  ( ResourceT
  , runResourceT
  , liftResourceT
  -- FIXME add relevant reexports
  ) where

-- FIXME consider moving this module into resourcet package itself

import qualified Control.Monad.Trans.Resource as Res
import Control.Monad.Trans.Resource.Internal (ResourceT (..))
import Control.Monad.IO.Unlift
import UnliftIO.Instances ()

-- | @since 0.1.0.0
runResourceT :: MonadUnliftIO m => ResourceT m a -> m a
runResourceT m = withRunInIO $ \run -> Res.runResourceT $ Res.transResourceT run m

-- | @since 0.1.0.0
liftResourceT :: MonadIO m => ResourceT IO a -> Res.ResourceT m a
liftResourceT (ResourceT f) = ResourceT $ liftIO . f
