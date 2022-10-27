{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
-- | Unlifted "Control.Exception", with extra async exception safety
-- and more helper functions.
--
-- This module works best when your cleanup functions adhere to certain
-- expectations around exception safety and interruptible actions.
-- For more details, see [this exception safety tutorial](https://www.fpcomplete.com/haskell/tutorial/exceptions/).
module UnliftIO.Exception
  ( -- * Throwing
    throwIO
  , throwString
  , StringException (..)
  , stringException
  , throwTo
  , impureThrow
  , fromEither
  , fromEitherIO
  , fromEitherM
  , mapExceptionM

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
  , pureTry
  , pureTryDeep

  , ESafe.Handler (..)
  , catches
  , catchesDeep

    -- * Catching async exceptions (with recovery)
  , catchSyncOrAsync
  , handleSyncOrAsync
  , trySyncOrAsync

    -- * Cleanup (no recovery)
  , onException
  , bracket
  , bracket_
  , finally
  , withException
  , bracketOnError
  , bracketOnError_

    -- * Coercion to sync and async
    -- | In version /0.2.23.0/, these were changed with aliases to the values
    -- from "Control.Exception.Safe" in the @safe-exceptions@ package.
  , ESafe.SyncExceptionWrapper(..)
  , toSyncException
  , ESafe.AsyncExceptionWrapper(..)
  , toAsyncException
  , fromExceptionUnwrap

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
  , EUnsafe.asyncExceptionToException
  , EUnsafe.asyncExceptionFromException
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
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Exception.Safe as ESafe
import Control.Exception.Safe (Handler(..))

#if MIN_VERSION_base(4,9,0)
import GHC.Stack (prettySrcLoc)
import GHC.Stack.Types (HasCallStack, CallStack, getCallStack)
#endif

-- | Catch a synchronous (but not asynchronous) exception and recover from it.
--
-- This is parameterized on the exception type. To catch all synchronous exceptions,
-- use 'catchAny'.
--
-- @since 0.1.0.0
catch
  :: (MonadUnliftIO m, Exception e)
  => m a -- ^ action
  -> (e -> m a) -- ^ handler
  -> m a
catch f g = withRunInIO $ \run -> run f `EUnsafe.catch` \e ->
  if isSyncException e
    then run (g e)
    -- intentionally rethrowing an async exception synchronously,
    -- since we want to preserve async behavior
    else EUnsafe.throwIO e

-- | 'catch' specialized to only catching 'IOException's.
--
-- @since 0.1.0.0
catchIO :: MonadUnliftIO m => m a -> (IOException -> m a) -> m a
catchIO = catch

-- | 'catch' specialized to catch all synchronous exceptions.
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

-- | 'catchDeep' specialized to catch all synchronous exception.
--
-- @since 0.1.0.0
catchAnyDeep :: (NFData a, MonadUnliftIO m) => m a -> (SomeException -> m a) -> m a
catchAnyDeep = catchDeep

-- | 'catchJust' is like 'catch' but it takes an extra argument which
-- is an exception predicate, a function which selects which type of
-- exceptions we're interested in.
--
-- @since 0.1.0.0
catchJust :: (MonadUnliftIO m, Exception e) => (e -> Maybe b) -> m a -> (b -> m a) -> m a
catchJust f a b = a `catch` \e -> maybe (liftIO (throwIO e)) b $ f e

-- | A variant of 'catch' that catches both synchronous and asynchronous exceptions.
--
-- WARNING: This function (and other @*SyncOrAsync@ functions) is for advanced users. Most of the
-- time, you probably want to use the non-@SyncOrAsync@ versions.
--
-- Before attempting to use this function, be familiar with the "Rules for async safe handling"
-- section in
-- [this blog post](https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell/).
--
-- @since 0.2.17
catchSyncOrAsync :: (MonadUnliftIO m, Exception e) => m a -> (e -> m a) -> m a
catchSyncOrAsync f g = withRunInIO $ \run -> run f `EUnsafe.catch` \e -> run (g e)

-- | Flipped version of 'catch'.
--
-- @since 0.1.0.0
handle :: (MonadUnliftIO m, Exception e) => (e -> m a) -> m a -> m a
handle = flip catch

-- | 'handle' specialized to only catching 'IOException's.
--
-- @since 0.1.0.0
handleIO :: MonadUnliftIO m => (IOException -> m a) -> m a -> m a
handleIO = handle

-- | Flipped version of 'catchAny'.
--
-- @since 0.1.0.0
handleAny :: MonadUnliftIO m => (SomeException -> m a) -> m a -> m a
handleAny = handle

-- | Flipped version of 'catchDeep'.
--
-- @since 0.1.0.0
handleDeep :: (MonadUnliftIO m, Exception e, NFData a) => (e -> m a) -> m a -> m a
handleDeep = flip catchDeep

-- | Flipped version of 'catchAnyDeep'.
--
-- @since 0.1.0.0
handleAnyDeep :: (MonadUnliftIO m, NFData a) => (SomeException -> m a) -> m a -> m a
handleAnyDeep = flip catchAnyDeep

-- | Flipped 'catchJust'.
--
-- @since 0.1.0.0
handleJust :: (MonadUnliftIO m, Exception e) => (e -> Maybe b) -> (b -> m a) -> m a -> m a
handleJust f = flip (catchJust f)

-- | A variant of 'handle' that catches both synchronous and asynchronous exceptions.
--
-- See 'catchSyncOrAsync'.
--
-- @since 0.2.17
handleSyncOrAsync :: (MonadUnliftIO m, Exception e) => (e -> m a) -> m a -> m a
handleSyncOrAsync = flip catchSyncOrAsync

-- | Run the given action and catch any synchronous exceptions as a 'Left' value.
--
-- This is parameterized on the exception type. To catch all synchronous exceptions,
-- use 'tryAny'.
--
-- @since 0.1.0.0
try :: (MonadUnliftIO m, Exception e) => m a -> m (Either e a)
try f = catch (liftM Right f) (return . Left)

-- | 'try' specialized to only catching 'IOException's.
--
-- @since 0.1.0.0
tryIO :: MonadUnliftIO m => m a -> m (Either IOException a)
tryIO = try

-- | 'try' specialized to catch all synchronous exceptions.
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

-- | 'tryDeep' specialized to catch all synchronous exceptions.
--
-- @since 0.1.0.0
tryAnyDeep :: (MonadUnliftIO m, NFData a) => m a -> m (Either SomeException a)
tryAnyDeep = tryDeep

-- | A variant of 'try' that takes an exception predicate to select
-- which exceptions are caught.
--
-- @since 0.1.0.0
tryJust :: (MonadUnliftIO m, Exception e) => (e -> Maybe b) -> m a -> m (Either b a)
tryJust f a = catch (Right `liftM` a) (\e -> maybe (throwIO e) (return . Left) (f e))

-- | A variant of 'try' that catches both synchronous and asynchronous exceptions.
--
-- See 'catchSyncOrAsync'.
--
-- @since 0.2.17
trySyncOrAsync :: (MonadUnliftIO m, Exception e) => m a -> m (Either e a)
trySyncOrAsync f = catchSyncOrAsync (liftM Right f) (return . Left)

-- | Evaluate the value to WHNF and catch any synchronous exceptions.
--
-- The expression may still have bottom values within it; you may
-- instead want to use 'pureTryDeep'.
--
-- @since 0.2.2.0
pureTry :: a -> Either SomeException a
pureTry a = unsafePerformIO $ (return $! Right $! a) `catchAny` (return . Left)

-- | Evaluate the value to NF and catch any synchronous exceptions.
--
-- @since 0.2.2.0
pureTryDeep :: NFData a => a -> Either SomeException a
pureTryDeep = unsafePerformIO . tryAnyDeep . return

-- | Internal.
catchesHandler :: MonadIO m => [Handler m a] -> SomeException -> m a
catchesHandler handlers e = foldr tryHandler (liftIO (EUnsafe.throwIO e)) handlers
    where tryHandler (ESafe.Handler handler) res
              = case fromException e of
                Just e' -> handler e'
                Nothing -> res

-- | Similar to 'catch', but provides multiple different handler functions.
--
-- For more information on motivation, see @base@'s 'EUnsafe.catches'. Note that,
-- unlike that function, this function will not catch asynchronous exceptions.
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

-- | Lifted version of 'EUnsafe.evaluate'.
--
-- @since 0.1.0.0
evaluate :: MonadIO m => a -> m a
evaluate = liftIO . EUnsafe.evaluate

-- | Deeply evaluate a value using 'evaluate' and 'NFData'.
--
-- @since 0.1.0.0
evaluateDeep :: (MonadIO m, NFData a) => a -> m a
evaluateDeep = (evaluate $!!)

-- | Allocate and clean up a resource safely.
--
-- For more information on motivation and usage of this function, see @base@'s
-- 'EUnsafe.bracket'. This function has two differences from the one in @base@.
-- The first, and more obvious, is that it works on any @MonadUnliftIO@
-- instance, not just @IO@.
--
-- The more subtle difference is that this function will use uninterruptible
-- masking for its cleanup handler. This is a subtle distinction, but at a
-- high level, means that resource cleanup has more guarantees to complete.
-- This comes at the cost that an incorrectly written cleanup function
-- cannot be interrupted.
--
-- For more information, please see <https://github.com/fpco/safe-exceptions/issues/3>.
--
-- @since 0.1.0.0
bracket :: MonadUnliftIO m => m a -> (a -> m b) -> (a -> m c) -> m c
bracket before after thing = withRunInIO $ \run -> EUnsafe.mask $ \restore -> do
  x <- run before
  res1 <- EUnsafe.try $ restore $ run $ thing x
  case res1 of
    Left (e1 :: SomeException) -> do
      -- explicitly ignore exceptions from after. We know that
      -- no async exceptions were thrown there, so therefore
      -- the stronger exception must come from thing
      --
      -- https://github.com/fpco/safe-exceptions/issues/2
      _ :: Either SomeException b <-
          EUnsafe.try $ EUnsafe.uninterruptibleMask_ $ run $ after x
      EUnsafe.throwIO e1
    Right y -> do
      _ <- EUnsafe.uninterruptibleMask_ $ run $ after x
      return y

-- | Same as 'bracket', but does not pass the acquired resource to cleanup and use functions.
--
-- For more information, see @base@'s 'EUnsafe.bracket_'.
--
-- @since 0.1.0.0
bracket_ :: MonadUnliftIO m => m a -> m b -> m c -> m c
bracket_ before after thing = bracket before (const after) (const thing)

-- | Same as 'bracket', but only perform the cleanup if an exception is thrown.
--
-- @since 0.1.0.0
bracketOnError :: MonadUnliftIO m => m a -> (a -> m b) -> (a -> m c) -> m c
bracketOnError before after thing = withRunInIO $ \run -> EUnsafe.mask $ \restore -> do
  x <- run before
  res1 <- EUnsafe.try $ restore $ run $ thing x
  case res1 of
    Left (e1 :: SomeException) -> do
      -- ignore the exception, see bracket for explanation
      _ :: Either SomeException b <-
        EUnsafe.try $ EUnsafe.uninterruptibleMask_ $ run $ after x
      EUnsafe.throwIO e1
    Right y -> return y

-- | A variant of 'bracketOnError' where the return value from the first
-- computation is not required.
--
-- @since 0.1.0.0
bracketOnError_ :: MonadUnliftIO m => m a -> m b -> m c -> m c
bracketOnError_ before after thing = bracketOnError before (const after) (const thing)

-- | Perform @thing@, guaranteeing that @after@ will run after, even if an exception occurs.
--
-- Same interruptible vs uninterrupible points apply as with 'bracket'. See @base@'s
-- 'EUnsafe.finally' for more information.
--
-- @since 0.1.0.0
finally
  :: MonadUnliftIO m
  => m a -- ^ thing
  -> m b -- ^ after
  -> m a
finally thing after = withRunInIO $ \run -> EUnsafe.uninterruptibleMask $ \restore -> do
  res1 <- EUnsafe.try $ restore $ run thing
  case res1 of
    Left (e1 :: SomeException) -> do
      -- see bracket for explanation
      _ :: Either SomeException b <- EUnsafe.try $ run after
      EUnsafe.throwIO e1
    Right x -> do
      _ <- run after
      return x

-- | Like 'onException', but provides the handler the thrown
-- exception.
--
-- @since 0.1.0.0
withException :: (MonadUnliftIO m, Exception e)
              => m a -> (e -> m b) -> m a
withException thing after = withRunInIO $ \run -> EUnsafe.uninterruptibleMask $ \restore -> do
    res1 <- EUnsafe.try $ restore $ run thing
    case res1 of
        Left e1 -> do
            -- see explanation in bracket
            _ :: Either SomeException b <- EUnsafe.try $ run $ after e1
            EUnsafe.throwIO e1
        Right x -> return x

-- | Like 'finally', but only call @after@ if an exception occurs.
--
-- @since 0.1.0.0
onException :: MonadUnliftIO m => m a -> m b -> m a
onException thing after = withException thing (\(_ :: SomeException) -> after)

-- | Synchronously throw the given exception.
--
-- Note that, if you provide an exception value which is of an asynchronous
-- type, it will be wrapped up in 'SyncExceptionWrapper'. See 'toSyncException'.
--
-- @since 0.1.0.0
throwIO :: (MonadIO m, Exception e) => e -> m a
throwIO = liftIO . EUnsafe.throwIO . toSyncException

-- | Convert an exception into a synchronous exception.
--
-- For synchronous exceptions, this is the same as 'toException'.
-- For asynchronous exceptions, this will wrap up the exception with
-- 'SyncExceptionWrapper'.
--
-- @since 0.1.0.0
toSyncException :: Exception e => e -> SomeException
toSyncException =
    ESafe.toSyncException

-- | Convert an exception into an asynchronous exception.
--
-- For asynchronous exceptions, this is the same as 'toException'.
-- For synchronous exceptions, this will wrap up the exception with
-- 'AsyncExceptionWrapper'.
--
-- @since 0.1.0.0
toAsyncException :: Exception e => e -> SomeException
toAsyncException =
    ESafe.toAsyncException

-- | Convert from a possibly wrapped exception.
--
-- The inverse of 'toAsyncException' and 'toSyncException'. When using those
-- functions (or functions that use them, like 'throwTo' or 'throwIO'),
-- 'fromException' might not be sufficient because the exception might be
-- wrapped within 'SyncExceptionWrapper' or 'AsyncExceptionWrapper'.
--
-- @since 0.2.17
fromExceptionUnwrap :: Exception e => SomeException -> Maybe e
fromExceptionUnwrap se
  | Just (ESafe.AsyncExceptionWrapper e) <- fromException se = cast e
  | Just (ESafe.SyncExceptionWrapper e) <- fromException se = cast e
  | otherwise = fromException se

-- | Check if the given exception is synchronous.
--
-- @since 0.1.0.0
isSyncException :: Exception e => e -> Bool
isSyncException =
    ESafe.isSyncException

-- | Check if the given exception is asynchronous.
--
-- @since 0.1.0.0
isAsyncException :: Exception e => e -> Bool
isAsyncException = not . isSyncException
{-# INLINE isAsyncException #-}

#if !MIN_VERSION_base(4,8,0)
-- | A synonym for 'show', specialized to 'Exception' instances.
--
-- Starting with base 4.8, the 'Exception' typeclass has a method
-- @displayException@, used for user-friendly display of exceptions.
-- This function provides backwards compatibility for users on base 4.7 and earlier,
-- so that anyone importing this module can simply use @displayException@.
--
-- @since 0.1.0.0
displayException :: Exception e => e -> String
displayException = show
#endif

-- | Unlifted version of 'EUnsafe.mask'.
--
-- @since 0.1.0.0
mask :: MonadUnliftIO m => ((forall a. m a -> m a) -> m b) -> m b
mask f = withRunInIO $ \run -> EUnsafe.mask $ \unmask ->
  run $ f $ liftIO . unmask . run

-- | Unlifted version of 'EUnsafe.uninterruptibleMask'.
--
-- @since 0.1.0.0
uninterruptibleMask :: MonadUnliftIO m => ((forall a. m a -> m a) -> m b) -> m b
uninterruptibleMask f = withRunInIO $ \run -> EUnsafe.uninterruptibleMask $ \unmask ->
  run $ f $ liftIO . unmask . run

-- | Unlifted version of 'EUnsafe.mask_'.
--
-- @since 0.1.0.0
mask_ :: MonadUnliftIO m => m a -> m a
mask_ f = withRunInIO $ \run -> EUnsafe.mask_ (run f)

-- | Unlifted version of 'EUnsafe.uninterruptibleMask_'.
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
throwString :: (MonadIO m, HasCallStack) => String -> m a
throwString s = throwIO (StringException s ?callStack)
#else
throwString :: MonadIO m => String -> m a
throwString s = throwIO (StringException s ())
#endif

-- | Smart constructor for a 'StringException' that deals with the
-- call stack.
--
-- @since 0.1.0.0
#if MIN_VERSION_base(4,9,0)
stringException :: HasCallStack => String -> StringException
stringException s = StringException s ?callStack
#else
stringException :: String -> StringException
stringException s = StringException s ()
#endif

-- | Exception type thrown by 'throwString'.
--
-- Note that the second field of the data constructor depends on
-- GHC/base version. For base 4.9 and GHC 8.0 and later, the second
-- field is a call stack. Previous versions of GHC and base do not
-- support call stacks, and the field is simply unit (provided to make
-- pattern matching across GHC versions easier).
--
-- @since 0.1.0.0
#if MIN_VERSION_base(4,9,0)
data StringException = StringException String CallStack
  deriving Typeable

-- | @since 0.1.0.0
instance Show StringException where
    show (StringException s cs) = concat
        $ "UnliftIO.Exception.throwString called with:\n\n"
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

-- | @since 0.1.0.0
instance Show StringException where
    show (StringException s _) = "UnliftIO.Exception.throwString called with:\n\n" ++ s
#endif

-- | @since 0.2.19
instance Eq StringException where
  StringException msg1 _ == StringException msg2 _ = msg1 == msg2

-- | @since 0.1.0.0
instance Exception StringException

-- | Throw an asynchronous exception to another thread.
--
-- Synchronously typed exceptions will be wrapped into an
-- `AsyncExceptionWrapper`, see
-- <https://github.com/fpco/safe-exceptions#determining-sync-vs-async>.
--
-- It's usually a better idea to use the "UnliftIO.Async" module, see
-- <https://github.com/fpco/safe-exceptions#quickstart>.
--
-- @since 0.1.0.0
throwTo :: (Exception e, MonadIO m) => ThreadId -> e -> m ()
throwTo tid = liftIO . EUnsafe.throwTo tid . toAsyncException

-- | Generate a pure value which, when forced, will synchronously
-- throw the given exception.
--
-- Generally it's better to avoid using this function and instead use 'throwIO',
-- see <https://github.com/fpco/safe-exceptions#quickstart>.
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

-- | Same as 'Control.Exception.mapException', except works in
-- a monadic context.
--
-- @since 0.2.15
mapExceptionM :: (Exception e1, Exception e2, MonadUnliftIO m) => (e1 -> e2) -> m a -> m a
mapExceptionM f = handle (throwIO . f)
