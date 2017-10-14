-- | Unlifted "System.IO".
--
-- @since 0.1.0.0
module UnliftIO.IO
  ( IOMode (..)
  , Handle
  , withFile
  , withBinaryFile
  ) where

import qualified System.IO as IO
import System.IO (Handle, IOMode (..))
import Control.Monad.IO.Unlift

-- | Unlifted version of 'IO.withFile'.
--
-- @since 0.1.0.0
withFile :: MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m a) -> m a
withFile fp mode inner = withRunInIO $ \run -> IO.withFile fp mode $ run . inner

-- | Unlifted version of 'IO.withBinaryFile'.
--
-- @since 0.1.0.0
withBinaryFile :: MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m a) -> m a
withBinaryFile fp mode inner = withRunInIO $ \run -> IO.withBinaryFile fp mode $ run . inner
