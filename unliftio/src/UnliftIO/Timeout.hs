-- | Unlifted "System.Timeout"
--
-- @since 0.1.0.0
module UnliftIO.Timeout
  ( timeout
  ) where

import qualified System.Timeout as S
import Control.Monad.IO.Unlift

-- | Unlifted 'S.timeout'
--
-- @since 0.1.0.0
timeout :: MonadUnliftIO m => Int -> m a -> m (Maybe a)
timeout x y = withRunInIO $ \run -> S.timeout x $ run y
