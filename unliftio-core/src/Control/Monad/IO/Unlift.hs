{-# LANGUAGE RankNTypes #-}
-- | Please see the README.md file for information on using this
-- package.
module Control.Monad.IO.Unlift
  ( MonadUnliftIO (..)
  , UnliftIO (..)
  , askRunInIO
  , withUnliftIO
  , withRunInIO
  , toIO
  , MonadIO (..)
  ) where

import Control.Monad (liftM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Identity (IdentityT (..))

-- | The ability to run any monadic action @m a@ as @IO a@.
--
-- This is more precisely a natural transformation. We need to new
-- datatype (instead of simply using a @forall@) due to lack of
-- support in GHC for impredicative types.
--
-- @since 0.1.0.0
newtype UnliftIO m = UnliftIO { unliftIO :: forall a. m a -> IO a }

-- | Monads which allow their actions to be run in 'IO'.
--
-- While 'MonadIO' allows an 'IO' action to be lifted into another
-- monad, this class captures the opposite concept: allowing you to
-- capture the monadic context. Note that, in order to meet the laws
-- given below, the intuition is that a monad must have no monadic
-- state, but may have monadic context. This essentially limits
-- 'MonadUnliftIO' to 'ReaderT' and 'IdentityT' transformers on top of
-- 'IO'.
--
-- Laws. For any value @u@ returned by 'askUnliftIO', it must meet the
-- monad transformer laws as reformulated for @MonadUnliftIO@:
--
-- * @unliftIO u . return = return@
--
-- * @unliftIO u (m >>= f) = unliftIO u m >>= unliftIO u . f@
--
-- The third is a currently nameless law which ensures that the
-- current context is preserved.
--
-- * @askUnliftIO >>= (\u -> liftIO (unliftIO u m)) = m@
--
-- If you have a name for this, please submit it in a pull request for
-- great glory.
--
-- @since 0.1.0.0
class MonadIO m => MonadUnliftIO m where
  -- | Capture the current monadic context, providing the ability to
  -- run monadic actions in 'IO'.
  --
  -- See 'UnliftIO' for an explanation of why we need a helper
  -- datatype here.
  -- @since 0.1.0.0
  askUnliftIO :: m (UnliftIO m)
  -- Would be better, but GHC hates us
  -- askUnliftIO :: m (forall a. m a -> IO a)
instance MonadUnliftIO IO where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = return (UnliftIO id)
instance MonadUnliftIO m => MonadUnliftIO (ReaderT r m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = ReaderT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip runReaderT r))
instance MonadUnliftIO m => MonadUnliftIO (IdentityT m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = IdentityT $
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . runIdentityT))

-- | Same ask 'askUnliftIO', but returns a monomorphic function
-- instead of a polymorphic newtype wrapper. If you only need to apply
-- the transformation on one concrete type, this function can be more
-- convenient.
--
-- @since 0.1.0.0
{-# INLINE askRunInIO #-}
askRunInIO :: MonadUnliftIO m => m (m a -> IO a)
askRunInIO = liftM unliftIO askUnliftIO

-- | Convenience function for capturing the monadic context and running
-- an 'IO' action. The 'UnliftIO' newtype wrapper is rarely needed, so
-- prefer 'withRunInIO' to this function.
--
-- @since 0.1.0.0
{-# INLINE withUnliftIO #-}
withUnliftIO :: MonadUnliftIO m => (UnliftIO m -> IO a) -> m a
withUnliftIO inner = askUnliftIO >>= liftIO . inner

-- | Same as 'withUnliftIO', but uses a polymorphic function
-- without the newtype wrapper.
--
-- @since 0.1.0.0
{-# INLINE withRunInIO #-}
withRunInIO :: MonadUnliftIO m => ((forall a. m a -> IO a) -> IO b) -> m b
withRunInIO inner = withUnliftIO $ \u -> inner (unliftIO u)

-- | Convert an action in @m@ to an action in @IO@.
--
-- @since 0.1.0.0
{-# INLINE toIO #-}
toIO :: MonadUnliftIO m => m a -> m (IO a)
toIO m = withRunInIO $ \run -> return $ run m
