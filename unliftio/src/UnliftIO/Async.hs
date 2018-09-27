{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Unlifted "Control.Concurrent.Async".
--
-- @since 0.1.0.0
module UnliftIO.Async
  (
    -- * Asynchronous actions
    Async,
    -- ** Spawning
    async, asyncBound, asyncOn, asyncWithUnmask, asyncOnWithUnmask,

    -- ** Spawning with automatic 'cancel'ation
    withAsync, withAsyncBound, withAsyncOn, withAsyncWithUnmask,
    withAsyncOnWithUnmask,

    -- ** Querying 'Async's
    wait, poll, waitCatch, cancel, uninterruptibleCancel, cancelWith,
    A.asyncThreadId,

    -- ** STM operations
    A.waitSTM, A.pollSTM, A.waitCatchSTM,

    -- ** Waiting for multiple 'Async's
    waitAny, waitAnyCatch, waitAnyCancel, waitAnyCatchCancel,
    waitEither, waitEitherCatch, waitEitherCancel, waitEitherCatchCancel,
    waitEither_,
    waitBoth,

    -- ** Waiting for multiple 'Async's in STM
    A.waitAnySTM, A.waitAnyCatchSTM,
    A.waitEitherSTM, A.waitEitherCatchSTM,
    A.waitEitherSTM_,
    A.waitBothSTM,

    -- ** Linking
    link, link2,

    -- * Convenient utilities
    race, race_,
    concurrently, concurrently_,
    mapConcurrently, forConcurrently,
    mapConcurrently_, forConcurrently_,
    replicateConcurrently, replicateConcurrently_,
    Concurrently (..),
    Conc, conc, runConc,
    ConcException (..),
  ) where

import Control.Applicative
import qualified Control.Concurrent as C
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM
import Control.Exception (SomeException, Exception)
import Data.IORef
import Data.Typeable (Typeable)
import Control.Concurrent.MVar
import qualified UnliftIO.Exception as UE
import qualified Control.Concurrent.Async as A
import Control.Concurrent (threadDelay)
import Control.Monad (forever, liftM, (>=>), void, join, unless)
import Control.Monad.IO.Unlift
import Data.Foldable (traverse_, for_)
import Data.Traversable (for)

-- For the implementation of Conc below, we do not want any of the
-- smart async exception handling logic from UnliftIO.Exception, since
-- (eg) we're low-level enough to need to explicit be throwing async
-- exceptions synchronously.
import qualified Control.Exception as E

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup
#else
import Data.Monoid
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif

-- | Unlifted 'A.async'.
--
-- @since 0.1.0.0
async :: MonadUnliftIO m => m a -> m (Async a)
async m = withRunInIO $ \run -> A.async $ run m

-- | Unlifted 'A.asyncBound'.
--
-- @since 0.1.0.0
asyncBound :: MonadUnliftIO m => m a -> m (Async a)
asyncBound m = withRunInIO $ \run -> A.asyncBound $ run m

-- | Unlifted 'A.asyncOn'.
--
-- @since 0.1.0.0
asyncOn :: MonadUnliftIO m => Int -> m a -> m (Async a)
asyncOn i m = withRunInIO $ \run -> A.asyncOn i $ run m

-- | Unlifted 'A.asyncWithUnmask'.
--
-- @since 0.1.0.0
asyncWithUnmask :: MonadUnliftIO m => ((forall b. m b -> m b) -> m a) -> m (Async a)
asyncWithUnmask m =
  withRunInIO $ \run -> A.asyncWithUnmask $ \unmask -> run $ m $ liftIO . unmask . run

-- | Unlifted 'A.asyncOnWithUnmask'.
--
-- @since 0.1.0.0
asyncOnWithUnmask :: MonadUnliftIO m => Int -> ((forall b. m b -> m b) -> m a) -> m (Async a)
asyncOnWithUnmask i m =
  withRunInIO $ \run -> A.asyncOnWithUnmask i $ \unmask -> run $ m $ liftIO . unmask . run

-- | Unlifted 'A.withAsync'.
--
-- @since 0.1.0.0
withAsync :: MonadUnliftIO m => m a -> (Async a -> m b) -> m b
withAsync a b = withRunInIO $ \run -> A.withAsync (run a) (run . b)

-- | Unlifted 'A.withAsyncBound'.
--
-- @since 0.1.0.0
withAsyncBound :: MonadUnliftIO m => m a -> (Async a -> m b) -> m b
withAsyncBound a b = withRunInIO $ \run -> A.withAsyncBound (run a) (run . b)

-- | Unlifted 'A.withAsyncOn'.
--
-- @since 0.1.0.0
withAsyncOn :: MonadUnliftIO m => Int -> m a -> (Async a -> m b) -> m b
withAsyncOn i a b = withRunInIO $ \run -> A.withAsyncOn i (run a) (run . b)

-- | Unlifted 'A.withAsyncWithUnmask'.
--
-- @since 0.1.0.0
withAsyncWithUnmask
  :: MonadUnliftIO m
  => ((forall c. m c -> m c) -> m a)
  -> (Async a -> m b)
  -> m b
withAsyncWithUnmask a b =
  withRunInIO $ \run -> A.withAsyncWithUnmask
    (\unmask -> run $ a $ liftIO . unmask . run)
    (run . b)

-- | Unlifted 'A.withAsyncOnWithMask'.
--
-- @since 0.1.0.0
withAsyncOnWithUnmask
  :: MonadUnliftIO m
  => Int
  -> ((forall c. m c -> m c) -> m a)
  -> (Async a -> m b)
  -> m b
withAsyncOnWithUnmask i a b =
  withRunInIO $ \run -> A.withAsyncOnWithUnmask i
    (\unmask -> run $ a $ liftIO . unmask . run)
    (run . b)

-- | Lifted 'A.wait'.
--
-- @since 0.1.0.0
wait :: MonadIO m => Async a -> m a
wait = liftIO . A.wait

-- | Lifted 'A.poll'.
--
-- @since 0.1.0.0
poll :: MonadIO m => Async a -> m (Maybe (Either SomeException a))
poll = liftIO . A.poll

-- | Lifted 'A.waitCatch'.
--
-- @since 0.1.0.0
waitCatch :: MonadIO m => Async a -> m (Either SomeException a)
waitCatch = liftIO . A.waitCatch

-- | Lifted 'A.cancel'.
--
-- @since 0.1.0.0
cancel :: MonadIO m => Async a -> m ()
cancel = liftIO . A.cancel

-- | Lifted 'A.uninterruptibleCancel'.
--
-- @since 0.1.0.0
uninterruptibleCancel :: MonadIO m => Async a -> m ()
uninterruptibleCancel = liftIO . A.uninterruptibleCancel

-- | Lifted 'A.cancelWith'. Additionally uses 'UE.toAsyncException' to
-- ensure async exception safety.
--
-- @since 0.1.0.0
cancelWith :: (Exception e, MonadIO m) => Async a -> e -> m ()
cancelWith a e = liftIO (A.cancelWith a (UE.toAsyncException e))

-- | Lifted 'A.waitAny'.
--
-- @since 0.1.0.0
waitAny :: MonadIO m => [Async a] -> m (Async a, a)
waitAny = liftIO . A.waitAny

-- | Lifted 'A.waitAnyCatch'.
--
-- @since 0.1.0.0
waitAnyCatch :: MonadIO m => [Async a] -> m (Async a, Either SomeException a)
waitAnyCatch = liftIO . A.waitAnyCatch

-- | Lifted 'A.waitAnyCancel'.
--
-- @since 0.1.0.0
waitAnyCancel :: MonadIO m => [Async a] -> m (Async a, a)
waitAnyCancel = liftIO . A.waitAnyCancel

-- | Lifted 'A.waitAnyCatchCancel'.
--
-- @since 0.1.0.0
waitAnyCatchCancel :: MonadIO m => [Async a] -> m (Async a, Either SomeException a)
waitAnyCatchCancel = liftIO . A.waitAnyCatchCancel

-- | Lifted 'A.waitEither'.
--
-- @since 0.1.0.0
waitEither :: MonadIO m => Async a -> Async b -> m (Either a b)
waitEither a b = liftIO (A.waitEither a b)

-- | Lifted 'A.waitEitherCatch'.
--
-- @since 0.1.0.0
waitEitherCatch :: MonadIO m => Async a -> Async b -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatch a b = liftIO (A.waitEitherCatch a b)

-- | Lifted 'A.waitEitherCancel'.
--
-- @since 0.1.0.0
waitEitherCancel :: MonadIO m => Async a -> Async b -> m (Either a b)
waitEitherCancel a b = liftIO (A.waitEitherCancel a b)

-- | Lifted 'A.waitEitherCatchCancel'.
--
-- @since 0.1.0.0
waitEitherCatchCancel :: MonadIO m => Async a -> Async b -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchCancel a b = liftIO (A.waitEitherCatchCancel a b)

-- | Lifted 'A.waitEither_'.
--
-- @since 0.1.0.0
waitEither_ :: MonadIO m => Async a -> Async b -> m ()
waitEither_ a b = liftIO (A.waitEither_ a b)

-- | Lifted 'A.waitBoth'.
--
-- @since 0.1.0.0
waitBoth :: MonadIO m => Async a -> Async b -> m (a, b)
waitBoth a b = liftIO (A.waitBoth a b)

-- | Lifted 'A.link'.
--
-- @since 0.1.0.0
link :: MonadIO m => Async a -> m ()
link = liftIO . A.link

-- | Lifted 'A.link2'.
--
-- @since 0.1.0.0
link2 :: MonadIO m => Async a -> Async b -> m ()
link2 a b = liftIO (A.link2 a b)

-- | Unlifted 'A.race'.
--
-- @since 0.1.0.0
race :: MonadUnliftIO m => m a -> m b -> m (Either a b)
race a b = withRunInIO $ \run -> A.race (run a) (run b)

-- | Unlifted 'A.race_'.
--
-- @since 0.1.0.0
race_ :: MonadUnliftIO m => m a -> m b -> m ()
race_ a b = withRunInIO $ \run -> A.race_ (run a) (run b)

-- | Unlifted 'A.concurrently'.
--
-- @since 0.1.0.0
concurrently :: MonadUnliftIO m => m a -> m b -> m (a, b)
concurrently a b = withRunInIO $ \run -> A.concurrently (run a) (run b)

-- | Unlifted 'A.concurrently_'.
--
-- @since 0.1.0.0
concurrently_ :: MonadUnliftIO m => m a -> m b -> m ()
concurrently_ a b = withRunInIO $ \run -> A.concurrently_ (run a) (run b)

-- | Unlifted 'A.mapConcurrently'.
--
-- @since 0.1.0.0
mapConcurrently :: MonadUnliftIO m => Traversable t => (a -> m b) -> t a -> m (t b)
mapConcurrently f t = withRunInIO $ \run -> runFlat $ traverse
  (FlatApp . FlatAction . run . f)
  t
{-# INLINE mapConcurrently #-}

-- | Unlifted 'A.forConcurrently'.
--
-- @since 0.1.0.0
forConcurrently :: MonadUnliftIO m => Traversable t => t a -> (a -> m b) -> m (t b)
forConcurrently = flip mapConcurrently
{-# INLINE forConcurrently #-}

-- | Unlifted 'A.mapConcurrently_'.
--
-- @since 0.1.0.0
mapConcurrently_ :: MonadUnliftIO m => Foldable f => (a -> m b) -> f a -> m ()
mapConcurrently_ f t = withRunInIO $ \run -> runFlat $ traverse_
  (FlatApp . FlatAction . run . f)
  t
{-# INLINE mapConcurrently_ #-}

-- | Unlifted 'A.forConcurrently_'.
--
-- @since 0.1.0.0
forConcurrently_ :: MonadUnliftIO m => Foldable f => f a -> (a -> m b) -> m ()
forConcurrently_ = flip mapConcurrently_
{-# INLINE forConcurrently_ #-}

-- | Unlifted 'A.replicateConcurrently'.
--
-- @since 0.1.0.0
replicateConcurrently :: MonadUnliftIO m => Int -> m a -> m [a]
replicateConcurrently cnt m =
  case compare cnt 1 of
    LT -> pure []
    EQ -> (:[]) <$> m
    GT -> mapConcurrently id (replicate cnt m)
{-# INLINE replicateConcurrently #-}

-- | Unlifted 'A.replicateConcurrently_'.
--
-- @since 0.1.0.0
replicateConcurrently_ :: MonadUnliftIO m => Int -> m a -> m ()
replicateConcurrently_ cnt m =
  case compare cnt 1 of
    LT -> pure ()
    EQ -> void m
    GT -> mapConcurrently_ id (replicate cnt m)
{-# INLINE replicateConcurrently_ #-}

-- | Unlifted 'A.Concurrently'.
--
-- @since 0.1.0.0
newtype Concurrently m a = Concurrently
  { runConcurrently :: m a
  }

-- | @since 0.1.0.0
instance Monad m => Functor (Concurrently m) where
  fmap f (Concurrently a) = Concurrently $ liftM f a

-- | @since 0.1.0.0
instance MonadUnliftIO m => Applicative (Concurrently m) where
  pure = Concurrently . return
  Concurrently fs <*> Concurrently as =
    Concurrently $ liftM (\(f, a) -> f a) (concurrently fs as)

-- | @since 0.1.0.0
instance MonadUnliftIO m => Alternative (Concurrently m) where
  empty = Concurrently $ liftIO (forever (threadDelay maxBound))
  Concurrently as <|> Concurrently bs =
    Concurrently $ liftM (either id id) (race as bs)

#if MIN_VERSION_base(4,9,0)
-- | Only defined by @async@ for @base >= 4.9@.
--
-- @since 0.1.0.0
instance (MonadUnliftIO m, Semigroup a) => Semigroup (Concurrently m a) where
  (<>) = liftA2 (<>)

-- | @since 0.1.0.0
instance (Semigroup a, Monoid a, MonadUnliftIO m) => Monoid (Concurrently m a) where
  mempty = pure mempty
  mappend = (<>)
#else
-- | @since 0.1.0.0
instance (Monoid a, MonadUnliftIO m) => Monoid (Concurrently m a) where
  mempty = pure mempty
  mappend = liftA2 mappend
#endif

-- More efficient Conc implementation

-- | A more efficient alternative to 'Concurrently', which reduces the
-- number of threads that need to be forked. For more information, see
-- @FIXME link to blog post@. This is provided as a separate type to
-- @Concurrently@ as it has a slightly different API.
--
-- Use the 'conc' function to construct values of type 'Conc', and
-- 'runConc' to execute the composed actions. You can use the
-- @Applicative@ instance to run different actions and wait for all of
-- them to complete, or the @Alternative@ instance to wait for the
-- first thread to complete.
--
-- In the event of a runtime exception thrown by any of the children
-- threads, or an asynchronous exception received in the parent
-- thread, all threads will be killed with an 'A.AsyncCancelled'
-- exception and the original exception rethrown. If multiple
-- exceptions are generated by different threads, there are no
-- guarantees on which exception will end up getting rethrown.
--
-- For many common use cases, you may prefer using helper functions in
-- this module like 'mapConcurrently'.
--
-- There are some intentional differences in behavior to
-- @Concurrently@:
--
-- * Children threads are always launched in an unmasked state, not
--   the inherited state of the parent thread.
--
-- Note that it is a programmer error to use the @Alternative@
-- instance in such a way that there are no alternatives to an empty,
-- e.g. @runConc (empty <|> empty)@. In such a case, a 'ConcException'
-- will be thrown. If there was an @Alternative@ in the standard
-- libraries without @empty@, this library would use it instead.
--
-- @since 0.2.9.0
data Conc m a where
  Action :: m a -> Conc m a
  LiftA2 :: (a -> b -> c) -> Conc m a -> Conc m b -> Conc m c

  -- Just an optimization to avoid spawning extra threads
  Pure :: a -> Conc m a

  -- I thought there would be an optimization available from having a
  -- data constructor that explicit doesn't care about the first
  -- result. Turns out it doesn't help much: we still need to keep a
  -- TMVar below to know when the thread completes.
  --
  -- Then :: Conc m a -> Conc m b -> Conc m b

  Alt :: Conc m a -> Conc m a -> Conc m a
  Empty :: Conc m a

deriving instance Functor m => Functor (Conc m)

-- | Construct a value of type 'Conc' from an action. Compose these
-- values using the typeclass instances (most commonly 'Applicative'
-- and 'Alternative') and then run with 'runConc'.
--
-- @since 0.2.9.0
conc :: m a -> Conc m a
conc = Action


-- | Run a 'Conc' value on multiple threads.
--
-- @since 0.2.9.0
runConc :: MonadUnliftIO m => Conc m a -> m a
runConc = flatten >=> (liftIO . runFlat)

-- | @since 0.1.0.0
instance MonadUnliftIO m => Applicative (Conc m) where
  pure = Pure
  f <*> a = LiftA2 id f a
  -- See comment above on Then
  -- (*>) = Then
  liftA2 = LiftA2

-- | @since 0.1.0.0
instance MonadUnliftIO m => Alternative (Conc m) where
  empty = Empty -- this is so ugly, we don't actually want to provide it!
  (<|>) = Alt

#if MIN_VERSION_base(4,9,0)
-- | Only defined by @async@ for @base >= 4.9@.
--
-- @since 0.1.0.0
instance (MonadUnliftIO m, Semigroup a) => Semigroup (Conc m a) where
  (<>) = liftA2 (<>)

-- | @since 0.1.0.0
instance (Semigroup a, Monoid a, MonadUnliftIO m) => Monoid (Conc m a) where
  mempty = pure mempty
  mappend = (<>)
#else
-- | @since 0.1.0.0
instance (Monoid a, MonadUnliftIO m) => Monoid (Conc m a) where
  mempty = pure mempty
  mappend = liftA2 mappend
#endif

-------------------------
-- Conc implementation --
-------------------------

-- Data types for flattening out the original @Conc@ into a simplified
-- view. Goals:
--
-- * We want to get rid of the Empty data constructor. We don't want
--   it anyway, it's only there because of the Alternative typeclass.
--
-- * We want to ensure that there is no nesting of Alt data
--   constructors. There is a bookkeeping overhead to each time we
--   need to track raced threads, and we want to minimize that
--   bookkeeping.
--
-- * We want to ensure that, when racing, we're always racing at least
--   two threads.
--
-- * We want to simplify down to IO.

-- | Flattened structure, either Applicative or Alternative
data Flat a
  = FlatApp (FlatApp a)
  -- | Flattened Alternative. Has at least 2 entries, which must be
  -- FlatApp (no nesting of FlatAlts).
  | FlatAlt (FlatApp a) (FlatApp a) [FlatApp a]

deriving instance Functor Flat
instance Applicative Flat where
  pure = FlatApp . pure
  liftA2 f a b = FlatApp (FlatLiftA2 f a b)

-- | Flattened Applicative. No Alternative stuff directly in here, but
-- may be in the children.
data FlatApp a where
  FlatAction :: IO a -> FlatApp a
  FlatLiftA2 :: (a -> b -> c) -> Flat a -> Flat b -> FlatApp c
  FlatPure :: a -> FlatApp a

deriving instance Functor FlatApp
instance Applicative FlatApp where
  pure = FlatPure
  liftA2 f a b = FlatLiftA2 f (FlatApp a) (FlatApp b)

-- | Things that can go wrong in the structure of a 'Conc'. These are
-- /programmer errors/.
--
-- @since 0.2.9.0
data ConcException
  = EmptyWithNoAlternative
  deriving (Show, Typeable, Eq)
instance E.Exception ConcException

-- | Turn a 'Conc' into a 'Flat'. Note that thanks to the ugliness of
-- 'empty', this may fail, e.g. @flatten Empty@.
flatten :: forall m a. MonadUnliftIO m => Conc m a -> m (Flat a)
flatten c0 = withRunInIO $ \run -> do

  let both :: forall a. Conc m a -> IO (Flat a)
      both Empty = E.throwIO EmptyWithNoAlternative
      both (Action m) = pure $ FlatApp $ FlatAction $ run m
      both (LiftA2 f a b) = do
        a' <- both a
        b' <- both b
        pure $ FlatApp $ FlatLiftA2 f a' b'
      both (Alt a b) = do
        a' <- alt a
        b' <- alt b
        case a' $ b' [] of
          [] -> E.throwIO EmptyWithNoAlternative
          [x] -> pure $ FlatApp x
          x:y:z -> pure $ FlatAlt x y z
      both (Pure a) = pure $ FlatApp $ FlatPure a

      -- Returns a difference list for cheaper concatenation
      alt :: forall a. Conc m a -> IO ([FlatApp a] -> [FlatApp a])
      alt Empty = pure id
      alt (Alt a b) = do
        a' <- alt a
        b' <- alt b
        pure $ a' . b'
      alt (Action m) = pure (FlatAction (run m):)
      alt (LiftA2 f a b) = do
        a' <- both a
        b' <- both b
        pure (FlatLiftA2 f a' b':)
      alt (Pure a) = pure (FlatPure a:)

  both c0

-- | Run a @Flat a@ on multiple threads.
runFlat :: Flat a -> IO a

-- Silly, simple optimizations
runFlat (FlatApp (FlatAction io)) = io
runFlat (FlatApp (FlatPure x)) = pure x

-- Start off with all exceptions masked so we can install proper cleanup.
runFlat f0 = E.uninterruptibleMask $ \restore -> do
  -- How many threads have terminated? We need to ensure we kill all
  -- child threads and wait for them to die.
  countVar <- newTVarIO 0

  -- Forks off as many threads as necessary to run the given Flat a,
  -- and returns:
  --
  -- * An STM action that will block until completion and return the
  --   result.
  --
  -- * The IDs of all forked threads. These need to be tracked so they
  --   can be killed (either when an exception is thrown, or when one
  --   of the alternatives completes first).
  --
  -- It would be nice to have the STM action returned return an Either
  -- and keep the SomeException values somewhat explicit, but in all
  -- my testing this absolutely kills performance. Instead, we're
  -- going to use a hack of providing a TMVar to fill up with a
  -- SomeException when things fail.
  let go :: forall a.
            TMVar E.SomeException
         -> Flat a
         -> IO (STM a, [C.ThreadId])
      go _excVar (FlatApp (FlatPure x)) = pure (pure x, [])
      go excVar (FlatApp (FlatAction io)) = do
        var <- newEmptyTMVarIO
        tid <- C.forkIOWithUnmask $ \restore -> do
          res <- E.try $ restore io
          atomically $ do
            modifyTVar' countVar (+ 1)
            case res of
              Left e -> void $ tryPutTMVar excVar e
              Right x -> putTMVar var x
        pure (readTMVar var, [tid])
      go excVar (FlatApp (FlatLiftA2 f a b)) = do
        (a', tidsa) <- go excVar a
        (b', tidsb) <- go excVar b
        pure (liftA2 f a' b', tidsa ++ tidsb)

      go excVar0 (FlatAlt x y z) = do
        -- We're going to create our own excVar here to pass to the
        -- children, so we can avoid letting the AsyncCancelled
        -- exceptions we throw to the children here from propagating
        -- and taking down the whole system.
        excVar <- newEmptyTMVarIO
        resVar <- newEmptyTMVarIO
        pairs <- traverse (go excVar . FlatApp) (x:y:z)
        let (blockers, tids) = unzip pairs

        -- Fork a helper thread to wait for the first child to
        -- complete, or for one of them to die with an exception so we
        -- can propagate it to excVar0.
        tid <- C.forkIOWithUnmask $ \unmask -> do
          eres <- E.try $ atomically $ foldr
            (\blocker rest -> (Right <$> blocker) <|> rest)
            (Left <$> readTMVar excVar)
            blockers
          atomically $ do
            modifyTVar' countVar (+ 1)
            case eres of
              -- We were killed by an async exception, do nothing.
              Left (_ :: E.SomeException) -> pure ()
              -- Child thread died, propagate it
              Right (Left e) -> void $ tryPutTMVar excVar0 e
              -- Successful result from one of the children
              Right (Right x) -> putTMVar resVar x

          -- And kill all of the threads
          for_ (concat tids) $ \tid -> E.throwTo tid A.AsyncCancelled

        pure (readTMVar resVar, tid : concat tids)

  excVar <- newEmptyTMVarIO
  (getRes, tids) <- go excVar f0
  let tidCount = length tids
      allDone count = E.assert (count <= tidCount) (count == tidCount)

  -- Automatically retry if we get killed by a
  -- BlockedIndefinitelyOnSTM. For more information, see:
  --
  -- * https://github.com/simonmar/async/issues/14
  --
  -- * https://github.com/simonmar/async/pull/15
  let autoRetry action =
        action `E.catch`
        \E.BlockedIndefinitelyOnSTM -> autoRetry action

  -- Restore the original masking state while blocking and catch
  -- exceptions to allow the parent thread to be killed early.
  res <- E.try $ restore $ autoRetry $ atomically $
         (Left <$> readTMVar excVar) <|>
         (Right <$> getRes)

  count0 <- atomically $ readTVar countVar
  unless (allDone count0) $ do
    -- Kill all of the threads
    for_ tids $ \tid -> E.throwTo tid A.AsyncCancelled

    -- Wait for all of the threads to die. We're going to restore the
    -- original masking state here, just in case there's a bug in the
    -- cleanup code of a child thread, so that we can be killed by an
    -- async exception.
    restore $ atomically $ do
      count <- readTVar countVar
      check $ allDone count

  -- Return the result or throw an exception. Yes, we could use
  -- either or join, but explicit pattern matching is nicer here.
  case res of
    -- Parent thread was killed with an async exception
    Left e -> E.throwIO (e :: E.SomeException)
    -- Some child thread died
    Right (Left e) -> E.throwIO e
    -- Everything worked!
    Right (Right x) -> pure x
