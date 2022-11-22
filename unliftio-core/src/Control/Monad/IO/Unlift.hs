{-# LANGUAGE RankNTypes #-}
-- | Please see the README.md file for information on using this
-- package at <https://www.stackage.org/package/unliftio-core>.
module Control.Monad.IO.Unlift
  ( MonadUnliftIO (..)
  , UnliftIO (..)
  , askUnliftIO
  , askRunInIO
  , withUnliftIO
  , toIO
  , wrappedWithRunInIO
  , MonadIO (..)
  ) where

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
-- Laws. For any function @run@ provided by 'withRunInIO', it must meet the
-- monad transformer laws as reformulated for @MonadUnliftIO@:
--
-- * @run . return = return@
--
-- * @run (m >>= f) = run m >>= run . f@
--
-- Instances of @MonadUnliftIO@ must also satisfy the following laws:
--
-- [Identity law] @withRunInIO (\\run -> run m) = m@
-- [Inverse law]  @withRunInIO (\\_ -> m) = liftIO m@
--
-- As an example of an invalid instance, a naive implementation of
-- @MonadUnliftIO (StateT s m)@ might be
--
-- @
-- withRunInIO inner =
--   StateT $ \\s ->
--     withRunInIO $ \\run ->
--       inner (run . flip evalStateT s)
-- @
--
-- This breaks the identity law because the inner @run m@ would throw away
-- any state changes in @m@.
--
-- @since 0.1.0.0
class MonadIO m => MonadUnliftIO m where
  -- | Convenience function for capturing the monadic context and running an 'IO'
  -- action with a runner function. The runner function is used to run a monadic
  -- action @m@ in @IO@.
  --
  -- @since 0.1.0.0
  withRunInIO :: ((forall a. m a -> IO a) -> IO b) -> m b

instance MonadUnliftIO IO where
  {-# INLINE withRunInIO #-}
  withRunInIO inner = inner id

instance MonadUnliftIO m => MonadUnliftIO (ReaderT r m) where
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    ReaderT $ \r ->
    withRunInIO $ \run ->
    inner (run . flip runReaderT r)

instance MonadUnliftIO m => MonadUnliftIO (IdentityT m) where
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    IdentityT $
    withRunInIO $ \run ->
    inner (run . runIdentityT)

-- | Capture the current monadic context, providing the ability to
-- run monadic actions in 'IO'.
--
-- See 'UnliftIO' for an explanation of why we need a helper
-- datatype here.
--
-- Prior to version 0.2.0.0 of this library, this was a method in the
-- 'MonadUnliftIO' type class. It was moved out due to
-- <https://github.com/fpco/unliftio/issues/55>.
--
-- @since 0.1.0.0
askUnliftIO :: MonadUnliftIO m => m (UnliftIO m)
askUnliftIO = withRunInIO (\run -> return (UnliftIO run))
{-# INLINE askUnliftIO #-}
-- Would be better, but GHC hates us
-- askUnliftIO :: m (forall a. m a -> IO a)


-- | Same as 'askUnliftIO', but returns a monomorphic function
-- instead of a polymorphic newtype wrapper. If you only need to apply
-- the transformation on one concrete type, this function can be more
-- convenient.
--
-- @since 0.1.0.0
{-# INLINE askRunInIO #-}
askRunInIO :: MonadUnliftIO m => m (m a -> IO a)
-- withRunInIO return would be nice, but GHC 7.8.4 doesn't like it
askRunInIO = withRunInIO (\run -> (return (\ma -> run ma)))

-- | Convenience function for capturing the monadic context and running
-- an 'IO' action. The 'UnliftIO' newtype wrapper is rarely needed, so
-- prefer 'withRunInIO' to this function.
--
-- @since 0.1.0.0
{-# INLINE withUnliftIO #-}
withUnliftIO :: MonadUnliftIO m => (UnliftIO m -> IO a) -> m a
withUnliftIO inner = askUnliftIO >>= liftIO . inner

-- | Convert an action in @m@ to an action in @IO@.
--
-- @since 0.1.0.0
{-# INLINE toIO #-}
toIO :: MonadUnliftIO m => m a -> m (IO a)
toIO m = withRunInIO $ \run -> return $ run m

{- | A helper function for implementing @MonadUnliftIO@ instances.
Useful for the common case where you want to simply delegate to the
underlying transformer.

Note: You can derive 'MonadUnliftIO' for newtypes without this helper function
in @unliftio-core@ 0.2.0.0 and later.

@since 0.1.2.0
==== __Example__

> newtype AppT m a = AppT { unAppT :: ReaderT Int (ResourceT m) a }
>   deriving (Functor, Applicative, Monad, MonadIO)
>
> -- Same as `deriving newtype (MonadUnliftIO)`
> instance MonadUnliftIO m => MonadUnliftIO (AppT m) where
>   withRunInIO = wrappedWithRunInIO AppT unAppT
-}
{-# INLINE wrappedWithRunInIO #-}
wrappedWithRunInIO :: MonadUnliftIO n
                   => (n b -> m b)
                   -- ^ The wrapper, for instance @IdentityT@.
                   -> (forall a. m a -> n a)
                   -- ^ The inverse, for instance @runIdentityT@.
                   -> ((forall a. m a -> IO a) -> IO b)
                   -- ^ The actual function to invoke 'withRunInIO' with.
                   -> m b
wrappedWithRunInIO wrap unwrap inner = wrap $ withRunInIO $ \run ->
  inner $ run . unwrap
