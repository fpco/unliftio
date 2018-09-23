{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
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
  ) where

import Control.Applicative
import qualified Control.Concurrent as C
import qualified Control.Exception
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM
import Control.Exception (SomeException, Exception)
import Data.IORef
import Control.Concurrent.MVar
import qualified UnliftIO.Exception as E
import qualified Control.Concurrent.Async as A
import Control.Concurrent (threadDelay)
import Control.Monad (forever, liftM, (>=>), void)
import Control.Monad.IO.Unlift
import Data.Foldable (traverse_, for_)
import Data.Traversable (for)

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

-- | Lifted 'A.cancelWith'. Additionally uses 'E.toAsyncException' to
-- ensure async exception safety.
--
-- @since 0.1.0.0
cancelWith :: (Exception e, MonadIO m) => Async a -> e -> m ()
cancelWith a e = liftIO (A.cancelWith a (E.toAsyncException e))

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
mapConcurrently f = runConc . traverse (conc . f)

-- | Unlifted 'A.forConcurrently'.
--
-- @since 0.1.0.0
forConcurrently :: MonadUnliftIO m => Traversable t => t a -> (a -> m b) -> m (t b)
forConcurrently = flip mapConcurrently

-- | Unlifted 'A.mapConcurrently_'.
--
-- @since 0.1.0.0
mapConcurrently_ :: MonadUnliftIO m => Foldable f => (a -> m b) -> f a -> m ()
mapConcurrently_ f = runConc . traverse_ (conc . f)

-- | Unlifted 'A.forConcurrently_'.
--
-- @since 0.1.0.0
forConcurrently_ :: MonadUnliftIO m => Foldable f => f a -> (a -> m b) -> m ()
forConcurrently_ = flip mapConcurrently_

-- | Unlifted 'A.replicateConcurrently'.
--
-- @since 0.1.0.0
replicateConcurrently :: MonadUnliftIO m => Int -> m a -> m [a]
replicateConcurrently i = runConc . replicateA i . conc

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA i0 f =
  loop i0
  where
    loop i
      | i <= 0 = pure []
      | otherwise = liftA2 (:) f (loop $! i - 1)

-- | Unlifted 'A.replicateConcurrently_'.
--
-- @since 0.1.0.0
replicateConcurrently_ :: MonadUnliftIO m => Int -> m a -> m ()
replicateConcurrently_ i = runConc . replicateA_ i . conc

replicateA_ :: Applicative f => Int -> f a -> f ()
replicateA_ i0 f =
  loop i0
  where
    loop i
      | i <= 0 = pure ()
      | otherwise = f *> (loop $! i - 1)

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
-- 'runConc' to execute the composed actions.
--
-- @since 0.2.9.0
data Conc m a where
  Action :: m a -> Conc m a
  LiftA2 :: (a -> b -> c) -> Conc m a -> Conc m b -> Conc m c
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

data CAll a where
  CAction :: IO a -> CAll a
  CLiftA2 :: (a -> b -> c) -> CBoth a -> CBoth b -> CAll c
data CFirst a = CFirst (CAll a) (CAll a) [CAll a] -- 2-or-more list
data CBoth a = CApp (CAll a) | CAlt (CFirst a)

runCBoth :: forall a. CBoth a -> IO a
runCBoth (CApp (CAction x)) = x -- silly optimization
runCBoth c0 = Control.Exception.uninterruptibleMask $ \restore -> do
  countVar <- newTVarIO (0 :: Int)
  excVar <- newEmptyTMVarIO :: IO (TMVar SomeException)
  let run
        :: forall b. IO b
        -> TMVar b
        -> IO C.ThreadId
      -- really catching all exceptions including async exceptions
      run action resVar = do
        tid <- C.forkIO $ do
          res <- Control.Exception.try (restore action)
          atomically $ do
            modifyTVar' countVar (+ 1)
            case res of
              Left e ->
                case E.fromException e of
                  -- If racing below kills off the other threads,
                  -- don't treat that as our whole computation going
                  -- down. May be better to have some other method for
                  -- handling this. Or maybe the whole TMVar for
                  -- exceptions is a mistake.
                  Just A.AsyncCancelled -> pure ()
                  Nothing -> void $ tryPutTMVar excVar e
              Right b -> putTMVar resVar b
        pure tid

      go
        :: forall b. CBoth b
        -> IO (STM b, [C.ThreadId] -> [C.ThreadId])
      go (CApp app) = goApp app
      go (CAlt (CFirst a b c)) = do
        pairs <- for (a:b:c) goApp
        let (blockers, tids) = unzip pairs
        var <- newEmptyTMVarIO
        tid <- C.forkIO $ restore $ do
          mres <-
            atomically $
              (Nothing <$ readTMVar excVar) <|>
              (Just <$> foldr (<|>) retry blockers)
          for_ mres $ \res -> do
            atomically $ putTMVar var res
            for_ tids $ \tids' ->
              for_ (tids' []) $
                flip Control.Exception.throwTo A.AsyncCancelled
        let mkTids rest = tid : foldr ($) rest tids
        pure (takeTMVar var, mkTids)

      goApp
        :: forall b. CAll b
        -> IO (STM b, [C.ThreadId] -> [C.ThreadId])
      goApp (CAction action) = do
        var <- newEmptyTMVarIO
        tid <- run action var
        pure (takeTMVar var, (tid:))
      goApp (CLiftA2 f a b) = do
        (a', tida) <- go a
        (b', tidb) <- go b
        pure (liftA2 f a' b', tida . tidb)
  (getRes, mkTids) <- go c0
  let getResOrExc = atomically $
        (Left <$> readTMVar excVar) <|>
        (Right <$> getRes)
  eres <- getResOrExc `Control.Exception.catch` \e ->
    case e of
      Control.Exception.BlockedIndefinitelyOnSTM -> getResOrExc
      -- Set up this way in case a new data constructor is added
      -- _ -> Control.Exception.throwIO e
  let tids = mkTids []
  count <- atomically $ readTVar countVar
  if Control.Exception.assert (count <= length tids) (count == length tids)
    then pure ()
    else traverse_ (flip Control.Exception.throwTo A.AsyncCancelled) tids
  either Control.Exception.throwIO pure (eres :: Either SomeException a)

flatten :: forall m a. MonadUnliftIO m => Conc m a -> m (CBoth a)
flatten c0 = withRunInIO $ \run ->
  let cboth :: forall b. Conc m b -> IO (CBoth b)
      cboth Empty = error "Cannot have an Empty without a non-Empty"
      cboth (Alt x y) = do
        x' <- cfirst x
        y' <- cfirst y
        case x' $ y' [] of
          [] -> error "Cannot have an Empty without a non-Empty"
          [a] -> pure $ CApp a
          a:b:c -> pure $ CAlt $ CFirst a b c
      cboth (Action action) = pure $ CApp $ CAction $ run action
      cboth (LiftA2 f a b) = do
        a' <- cboth a
        b' <- cboth b
        pure $ CApp $ CLiftA2 f a' b'

      cfirst :: forall b. Conc m b -> IO ([CAll b] -> [CAll b])
      cfirst Empty = pure id
      cfirst (Alt x y) = do
        x' <- cfirst x
        y' <- cfirst y
        pure $ x' . y'
      cfirst x = do
        x' <- cboth x
        pure $
          case x' of
            CApp a -> (a:)
            CAlt (CFirst a b c) -> \rest -> a : b : c ++ rest

   in cboth c0

-- | Run a 'Conc' value on multiple threads.
--
-- @since 0.2.9.0
runConc :: MonadUnliftIO m => Conc m a -> m a
runConc = flatten >=> (liftIO . runCBoth)

-- | @since 0.1.0.0
instance MonadUnliftIO m => Applicative (Conc m) where
  pure = Action . return
  f <*> a = LiftA2 id f a
  liftA2 = LiftA2

-- | @since 0.1.0.0
instance MonadUnliftIO m => Alternative (Conc m) where
  empty = Empty
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
