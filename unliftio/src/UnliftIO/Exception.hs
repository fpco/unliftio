{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
-- | Unlifted "Control.Exception", with extra async exception safety
-- and more helper functions.
module UnliftIO.Exception
  ( -- * Throwing
    throwIO
  , throwString
  , StringException (..)
  , throwTo
  , impureThrow
  , fromEither
  , fromEitherIO
  , fromEitherM
    -- * Catching (with recovery)
  , catch
  , catchIO
  , catchAny
  , catchDeep
  , catchAnyDeep
  , catchJust

  , handle
  , handleIO
  , handleAny
  , handleDeep
  , handleAnyDeep
  , handleJust

  , try
  , tryIO
  , tryAny
  , tryDeep
  , tryAnyDeep
  , tryJust

  , Handler(..)
  , catches
  , catchesDeep

    -- * Cleanup (no recovery)
  , onException
  , bracket
  , bracket_
  , finally
  , withException
  , bracketOnError
  , bracketOnError_

    -- * Coercion to sync and async
  , SyncExceptionWrapper (..)
  , toSyncException
  , AsyncExceptionWrapper (..)
  , toAsyncException

    -- * Check exception type
  , isSyncException
  , isAsyncException
    -- * Masking
  , mask
  , uninterruptibleMask
  , mask_
  , uninterruptibleMask_
    -- * Evaluation
  , evaluate
  , evaluateDeep
    -- * Reexports
  , Exception (..)
  , Typeable
  , SomeException (..)
  , SomeAsyncException (..)
  , IOException
  , EUnsafe.assert
#if !MIN_VERSION_base(4,8,0)
  , displayException
#endif
  ) where

import Control.Concurrent (ThreadId)
import Control.Monad (liftM)
import Control.Monad.IO.Unlift
import Control.Exception (Exception (..), SomeException (..), IOException, SomeAsyncException (..))
import qualified Control.Exception as EUnsafe
import Control.DeepSeq (NFData (..), ($!!))
import Data.Typeable (Typeable, cast)

#if MIN_VERSION_base(4,9,0)
import GHC.Stack (prettySrcLoc)
import GHC.Stack.Types (HasCallStack, CallStack, getCallStack)
#endif

-- | Unlifted 'EUnsafe.catch', but will not catch asynchronous exceptions
--
-- @since 0.1.0.0
catch :: (MonadUnliftIO m, Exception e) => m a -> (e -> m a) -> m a
catch f g = withUnliftIO $ \u -> unliftIO u f `EUnsafe.catch` \e ->
  if isSyncException e
    then unliftIO u (g e)
    -- intentionally rethrowing an async exception synchronously,
    -- since we want to preserve async behavior
    else EUnsafe.throwIO e

-- | 'catch' specialized to only catching 'IOException's
--
-- @since 0.1.0.0
catchIO :: MonadUnliftIO m => m a -> (IOException -> m a) -> m a
catchIO = catch

-- | 'catch' specialized to catch all synchronous exception
--
-- @since 0.1.0.0
catchAny :: MonadUnliftIO m => m a -> (SomeException -> m a) -> m a
catchAny = catch

-- | Same as 'catch', but fully force evaluation of the result value
-- to find all impure exceptions.
--
-- @since 0.1.0.0
catchDeep :: (MonadUnliftIO m, Exception e, NFData a)
          => m a -> (e -> m a) -> m a
catchDeep m = catch (m >>= evaluateDeep)

-- | 'catchDeep' specialized to catch all synchronous exception
--
-- @since 0.1.1.0
catchAnyDeep :: (NFData a, MonadUnliftIO m) => m a -> (SomeException -> m a) -> m a
catchAnyDeep = catchDeep

-- | 'catchJust' is like 'catch' but it takes an extra argument which
-- is an exception predicate, a function which selects which type of
-- exceptions we're interested in.
--
-- @since 0.1.0.0
catchJust :: (MonadUnliftIO m, Exception e) => (e -> Maybe b) -> m a -> (b -> m a) -> m a
catchJust f a b = a `catch` \e -> maybe (liftIO (throwIO e)) b $ f e

-- | Flipped version of 'catch'
--
-- @since 0.1.0.0
handle :: (MonadUnliftIO m, Exception e) => (e -> m a) -> m a -> m a
handle = flip catch

-- | 'C.handle' specialized to only catching 'IOException's
--
-- @since 0.1.0.0
handleIO :: MonadUnliftIO m => (IOException -> m a) -> m a -> m a
handleIO = handle

-- | Flipped version of 'catchAny'
--
-- @since 0.1.0.0
handleAny :: MonadUnliftIO m => (SomeException -> m a) -> m a -> m a
handleAny = handle

-- | Flipped version of 'catchDeep'
--
-- @since 0.1.1.0
handleDeep :: (MonadUnliftIO m, Exception e, NFData a) => (e -> m a) -> m a -> m a
handleDeep = flip catchDeep

-- | Flipped version of 'catchAnyDeep'
--
-- @since 0.1.0.0
handleAnyDeep :: (MonadUnliftIO m, NFData a) => (SomeException -> m a) -> m a -> m a
handleAnyDeep = flip catchAnyDeep

-- | Flipped 'catchJust'.
--
-- @since 0.1.0.0
handleJust :: (MonadUnliftIO m, Exception e) => (e -> Maybe b) -> (b -> m a) -> m a -> m a
handleJust f = flip (catchJust f)

-- | Unlifted 'EUnsafe.try', but will not catch asynchronous exceptions
--
-- @since 0.1.0.0
try :: (MonadUnliftIO m, Exception e) => m a -> m (Either e a)
try f = catch (liftM Right f) (return . Left)

-- | 'try' specialized to only catching 'IOException's
--
-- @since 0.1.0.0
tryIO :: MonadUnliftIO m => m a -> m (Either IOException a)
tryIO = try

-- | 'try' specialized to catch all synchronous exceptions
--
-- @since 0.1.0.0
tryAny :: MonadUnliftIO m => m a -> m (Either SomeException a)
tryAny = try

-- | Same as 'try', but fully force evaluation of the result value
-- to find all impure exceptions.
--
-- @since 0.1.0.0
tryDeep :: (MonadUnliftIO m, Exception e, NFData a) => m a -> m (Either e a)
tryDeep f = catch (liftM Right (f >>= evaluateDeep)) (return . Left)

-- | 'tryDeep' specialized to catch all synchronous exceptions
--
-- @since 0.1.1.0
tryAnyDeep :: (MonadUnliftIO m, NFData a) => m a -> m (Either SomeException a)
tryAnyDeep = tryDeep

-- | A variant of 'try' that takes an exception predicate to select
-- which exceptions are caught.
--
-- @since 0.1.0.0
tryJust :: (MonadUnliftIO m, Exception e) => (e -> Maybe b) -> m a -> m (Either b a)
tryJust f a = catch (Right `liftM` a) (\e -> maybe (throwIO e) (return . Left) (f e))

-- | Generalized version of 'EUnsafe.Handler'
--
-- @since 0.1.0.0
data Handler m a = forall e . Exception e => Handler (e -> m a)

-- | Internal
catchesHandler :: MonadIO m => [Handler m a] -> SomeException -> m a
catchesHandler handlers e = foldr tryHandler (liftIO (EUnsafe.throwIO e)) handlers
    where tryHandler (Handler handler) res
              = case fromException e of
                Just e' -> handler e'
                Nothing -> res

-- | Same as upstream 'EUnsafe.catches', but will not catch
-- asynchronous exceptions
--
-- @since 0.1.0.0
catches :: MonadUnliftIO m => m a -> [Handler m a] -> m a
catches io handlers = io `catch` catchesHandler handlers

-- | Same as 'catches', but fully force evaluation of the result value
-- to find all impure exceptions.
--
-- @since 0.1.0.0
catchesDeep :: (MonadUnliftIO m, NFData a) => m a -> [Handler m a] -> m a
catchesDeep io handlers = (io >>= evaluateDeep) `catch` catchesHandler handlers

-- | Lifted version of 'EUnsafe.evaluate'
evaluate :: MonadIO m => a -> m a
evaluate = liftIO . EUnsafe.evaluate

-- | Deeply evaluate a value using 'evaluate' and 'NFData'.
--
-- @since 0.1.0.0
evaluateDeep :: (MonadIO m, NFData a) => a -> m a
evaluateDeep = (evaluate $!!)

-- | Async safe version of 'EUnsafe.bracket'
--
-- @since 0.1.0.0
bracket :: MonadUnliftIO m => m a -> (a -> m b) -> (a -> m c) -> m c
bracket before after thing = withUnliftIO $ \u -> EUnsafe.mask $ \restore -> do
  x <- unliftIO u before
  res1 <- EUnsafe.try $ restore $ unliftIO u $ thing x
  case res1 of
    Left (e1 :: SomeException) -> do
      -- explicitly ignore exceptions from after. We know that
      -- no async exceptions were thrown there, so therefore
      -- the stronger exception must come from thing
      --
      -- https://github.com/fpco/safe-exceptions/issues/2
      _ :: Either SomeException b <-
          EUnsafe.try $ EUnsafe.uninterruptibleMask_ $ unliftIO u $ after x
      EUnsafe.throwIO e1
    Right y -> do
      _ <- EUnsafe.uninterruptibleMask_ $ unliftIO u $ after x
      return y

-- | Async safe version of 'EUnsafe.bracket_'
--
-- @since 0.1.0.0
bracket_ :: MonadUnliftIO m => m a -> m b -> m c -> m c
bracket_ before after thing = bracket before (const after) (const thing)

-- | Async safe version of 'EUnsafe.bracketOnError'
--
-- @since 0.1.0.0
bracketOnError :: MonadUnliftIO m => m a -> (a -> m b) -> (a -> m c) -> m c
bracketOnError before after thing = withUnliftIO $ \u -> EUnsafe.mask $ \restore -> do
  x <- unliftIO u before
  res1 <- EUnsafe.try $ restore $ unliftIO u $ thing x
  case res1 of
    Left (e1 :: SomeException) -> do
      -- ignore the exception, see bracket for explanation
      _ :: Either SomeException b <-
        EUnsafe.try $ EUnsafe.uninterruptibleMask_ $ unliftIO u $ after x
      EUnsafe.throwIO e1
    Right y -> return y

-- | A variant of 'bracketOnError' where the return value from the first
-- computation is not required.
--
-- @since 0.1.0.0
bracketOnError_ :: MonadUnliftIO m => m a -> m b -> m c -> m c
bracketOnError_ before after thing = bracketOnError before (const after) (const thing)

-- | Async safe version of 'EUnsafe.finally'
--
-- @since 0.1.0.0
finally :: MonadUnliftIO m => m a -> m b -> m a
finally thing after = withUnliftIO $ \u -> EUnsafe.uninterruptibleMask $ \restore -> do
  res1 <- EUnsafe.try $ restore $ unliftIO u thing
  case res1 of
    Left (e1 :: SomeException) -> do
      -- see bracket for explanation
      _ :: Either SomeException b <- EUnsafe.try $ unliftIO u after
      EUnsafe.throwIO e1
    Right x -> do
      _ <- unliftIO u after
      return x

-- | Like 'onException', but provides the handler the thrown
-- exception.
--
-- @since 0.1.0.0
withException :: (MonadUnliftIO m, Exception e)
              => m a -> (e -> m b) -> m a
withException thing after = withUnliftIO $ \u -> EUnsafe.uninterruptibleMask $ \restore -> do
    res1 <- EUnsafe.try $ restore $ unliftIO u thing
    case res1 of
        Left e1 -> do
            -- see explanation in bracket
            _ :: Either SomeException b <- EUnsafe.try $ unliftIO u $ after e1
            EUnsafe.throwIO e1
        Right x -> return x

-- | Async safe version of 'EUnsafe.onException'
--
-- @since 0.1.0.0
onException :: MonadUnliftIO m => m a -> m b -> m a
onException thing after = withException thing (\(_ :: SomeException) -> after)

-- | Synchronously throw the given exception
--
-- @since 0.1.0.0
throwIO :: (MonadIO m, Exception e) => e -> m a
throwIO = liftIO . EUnsafe.throwIO . toSyncException

-- | Wrap up an asynchronous exception to be treated as a synchronous
-- exception
--
-- This is intended to be created via 'toSyncException'
--
-- @since 0.1.0.0
data SyncExceptionWrapper = forall e. Exception e => SyncExceptionWrapper e
    deriving Typeable
instance Show SyncExceptionWrapper where
    show (SyncExceptionWrapper e) = show e
instance Exception SyncExceptionWrapper where
#if MIN_VERSION_base(4,8,0)
    displayException (SyncExceptionWrapper e) = displayException e
#endif

-- | Convert an exception into a synchronous exception
--
-- For synchronous exceptions, this is the same as 'toException'.
-- For asynchronous exceptions, this will wrap up the exception with
-- 'SyncExceptionWrapper'
--
-- @since 0.1.0.0
toSyncException :: Exception e => e -> SomeException
toSyncException e =
    case fromException se of
        Just (SomeAsyncException _) -> toException (SyncExceptionWrapper e)
        Nothing -> se
  where
    se = toException e

-- | Wrap up a synchronous exception to be treated as an asynchronous
-- exception
--
-- This is intended to be created via 'toAsyncException'
--
-- @since 0.1.0.0
data AsyncExceptionWrapper = forall e. Exception e => AsyncExceptionWrapper e
    deriving Typeable
instance Show AsyncExceptionWrapper where
    show (AsyncExceptionWrapper e) = show e
instance Exception AsyncExceptionWrapper where
    toException = toException . SomeAsyncException
    fromException se = do
        SomeAsyncException e <- fromException se
        cast e
#if MIN_VERSION_base(4,8,0)
    displayException (AsyncExceptionWrapper e) = displayException e
#endif

-- | Convert an exception into an asynchronous exception
--
-- For asynchronous exceptions, this is the same as 'toException'.
-- For synchronous exceptions, this will wrap up the exception with
-- 'AsyncExceptionWrapper'
--
-- @since 0.1.0.0
toAsyncException :: Exception e => e -> SomeException
toAsyncException e =
    case fromException se of
        Just (SomeAsyncException _) -> se
        Nothing -> toException (AsyncExceptionWrapper e)
  where
    se = toException e

-- | Check if the given exception is synchronous
--
-- @since 0.1.0.0
isSyncException :: Exception e => e -> Bool
isSyncException e =
    case fromException (toException e) of
        Just (SomeAsyncException _) -> False
        Nothing -> True

-- | Check if the given exception is asynchronous
--
-- @since 0.1.0.0
isAsyncException :: Exception e => e -> Bool
isAsyncException = not . isSyncException
{-# INLINE isAsyncException #-}

#if !MIN_VERSION_base(4,8,0)
-- | A synonym for 'show', specialized to 'Exception' instances.
--
-- Starting with base 4.8, the 'Exception' typeclass has a method @displayException@, used for user-friendly display of exceptions. This function provides backwards compatibility for users on base 4.7 and earlier, so that anyone importing this module can simply use @displayException@.
--
-- @since 0.1.0.0
displayException :: Exception e => e -> String
displayException = show
#endif

-- | Unlifted version of 'EUnsafe.mask'
--
-- @since 0.1.0.0
mask :: MonadUnliftIO m => ((forall a. m a -> m a) -> m b) -> m b
mask f = withUnliftIO $ \u -> EUnsafe.mask $ \unmask ->
  unliftIO u $ f $ liftIO . unmask . unliftIO u

-- | Unlifted version of 'EUnsafe.uninterruptibleMask'
--
-- @since 0.1.0.0
uninterruptibleMask :: MonadUnliftIO m => ((forall a. m a -> m a) -> m b) -> m b
uninterruptibleMask f = withUnliftIO $ \u -> EUnsafe.uninterruptibleMask $ \unmask ->
  unliftIO u $ f $ liftIO . unmask . unliftIO u

-- | Unlifted version of 'EUnsafe.mask_'
--
-- @since 0.1.0.0
mask_ :: MonadUnliftIO m => m a -> m a
mask_ f = withRunInIO $ \run -> EUnsafe.mask_ (run f)

-- | Unlifted version of 'EUnsafe.uninterruptibleMask_'
--
-- @since 0.1.0.0
uninterruptibleMask_ :: MonadUnliftIO m => m a -> m a
uninterruptibleMask_ f = withRunInIO $ \run -> EUnsafe.uninterruptibleMask_ (run f)

-- | A convenience function for throwing a user error. This is useful
-- for cases where it would be too high a burden to define your own
-- exception type.
--
-- This throws an exception of type 'StringException'. When GHC
-- supports it (base 4.9 and GHC 8.0 and onward), it includes a call
-- stack.
--
-- @since 0.1.0.0
#if MIN_VERSION_base(4,9,0)
throwString :: (MonadUnliftIO m, HasCallStack) => String -> m a
throwString s = throwIO (StringException s ?callStack)
#else
throwString :: MonadUnliftIO m => String -> m a
throwString s = throwIO (StringException s ())
#endif

-- | Exception type thrown by 'throwString'.
--
-- Note that the second field of the data constructor depends on
-- GHC/base version. For base 4.9 and GHC 8.0 and later, the second
-- field is a call stack. Previous versions of GHC and base do not
-- support call stacks, and the field is simply unit (provided to make
-- pattern matching across GHC versions easier).
--
-- @since 0.1.5.0
#if MIN_VERSION_base(4,9,0)
data StringException = StringException String CallStack
  deriving Typeable

instance Show StringException where
    show (StringException s cs) = concat
        $ "Control.Exception.Safe.throwString called with:\n\n"
        : s
        : "\nCalled from:\n"
        : map go (getCallStack cs)
      where
        go (x, y) = concat
          [ "  "
          , x
          , " ("
          , prettySrcLoc y
          , ")\n"
          ]
#else
data StringException = StringException String ()
  deriving Typeable

instance Show StringException where
    show (StringException s _) = "Control.Exception.Safe.throwString called with:\n\n" ++ s
#endif
instance Exception StringException

-- | Throw an asynchronous exception to another thread.
--
-- Synchronously typed exceptions will be wrapped into an
-- `AsyncExceptionWrapper`, see
-- <https://github.com/fpco/safe-exceptions#determining-sync-vs-async>
--
-- It's usually a better idea to use the async package, see
-- <https://github.com/fpco/safe-exceptions#quickstart>
--
-- @since 0.1.0.0
throwTo :: (Exception e, MonadIO m) => ThreadId -> e -> m ()
throwTo tid = liftIO . EUnsafe.throwTo tid . toAsyncException

-- | Generate a pure value which, when forced, will synchronously
-- throw the given exception
--
-- Generally it's better to avoid using this function and instead use 'throw',
-- see <https://github.com/fpco/safe-exceptions#quickstart>
--
-- @since 0.1.0.0
impureThrow :: Exception e => e -> a
impureThrow = EUnsafe.throw . toSyncException

-- | Unwrap an 'Either' value, throwing its 'Left' value as a runtime
-- exception via 'throwIO' if present.
--
-- @since 0.1.0.0
fromEither :: (Exception e, MonadIO m) => Either e a -> m a
fromEither = either throwIO return

-- | Same as 'fromEither', but works on an 'IO'-wrapped 'Either'.
--
-- @since 0.1.0.0
fromEitherIO :: (Exception e, MonadIO m) => IO (Either e a) -> m a
fromEitherIO = fromEitherM . liftIO

-- | Same as 'fromEither', but works on an 'm'-wrapped 'Either'.
--
-- @since 0.1.0.0
fromEitherM :: (Exception e, MonadIO m) => m (Either e a) -> m a
fromEitherM = (>>= fromEither)
