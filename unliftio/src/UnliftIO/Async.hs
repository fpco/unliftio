{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE TupleSections#-}
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

    -- ** Pooled concurrency
    pooledMapConcurrentlyN,
    pooledMapConcurrently,
    pooledMapConcurrentlyN_,
    pooledMapConcurrently_,
    pooledForConcurrentlyN,
    pooledForConcurrently,
    pooledReplicateConcurrentlyN,
    pooledReplicateConcurrently,
    pooledReplicateConcurrentlyN_,
    pooledReplicateConcurrently_,

    -- * Convenient utilities
    race, race_,
    concurrently, concurrently_,
    mapConcurrently, forConcurrently,
    mapConcurrently_, forConcurrently_,
    replicateConcurrently, replicateConcurrently_,
    Concurrently(..)
                
  ) where

import Control.Applicative
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Concurrent.Async (Async)
import Control.Exception (SomeException, Exception)
import qualified UnliftIO.Exception as E
import qualified Control.Concurrent.Async as A
import Control.Concurrent (threadDelay, getNumCapabilities)
import Control.Monad (forever, liftM)
import Control.Monad.IO.Unlift

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup
#else
import Data.Monoid
#endif
import Data.Foldable (Foldable, toList)
import Data.Traversable (Traversable, for)

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
mapConcurrently f t = withRunInIO $ \run -> A.mapConcurrently (run . f) t

-- | Unlifted 'A.forConcurrently'.
--
-- @since 0.1.0.0
forConcurrently :: MonadUnliftIO m => Traversable t => t a -> (a -> m b) -> m (t b)
forConcurrently t f = withRunInIO $ \run -> A.forConcurrently t (run . f)

-- | Unlifted 'A.mapConcurrently_'.
--
-- @since 0.1.0.0
mapConcurrently_ :: MonadUnliftIO m => Foldable f => (a -> m b) -> f a -> m ()
mapConcurrently_ f t = withRunInIO $ \run -> A.mapConcurrently_ (run . f) t

-- | Unlifted 'A.forConcurrently_'.
--
-- @since 0.1.0.0
forConcurrently_ :: MonadUnliftIO m => Foldable f => f a -> (a -> m b) -> m ()
forConcurrently_ t f = withRunInIO $ \run -> A.forConcurrently_ t (run . f)

-- | Unlifted 'A.replicateConcurrently'.
--
-- @since 0.1.0.0
replicateConcurrently :: MonadUnliftIO m => Int -> m a -> m [a]
replicateConcurrently i m = withRunInIO $ \run -> A.replicateConcurrently i (run m)

-- | Unlifted 'A.replicateConcurrently_'.
--
-- @since 0.1.0.0
replicateConcurrently_ :: MonadUnliftIO m => Int -> m a -> m ()
replicateConcurrently_ i m = withRunInIO $ \run -> A.replicateConcurrently_ i (run m)

-- | Like 'mapConcurrently' from async, but instead of one thread per
-- element, it does pooling from a set of threads. This is useful in
-- scenarios where resource consumption is bounded and for use cases
-- where too many concurrent tasks aren't allowed.
--
-- === __Example usage__
--
-- @
-- import Say
-- 
-- action :: Int -> IO Int
-- action n = do
--   tid <- myThreadId
--   sayString $ show tid
--   threadDelay (2 * 10^6) -- 2 seconds
--   return n
-- 
-- main :: IO ()
-- main = do
--   yx \<- pooledMapConcurrently 5 (\\x -\> action x) [1..5]
--   print yx
-- @
--
-- On executing you can see that five threads have been spawned:
-- 
-- @
-- \$ ./pool
-- ThreadId 36
-- ThreadId 38
-- ThreadId 40
-- ThreadId 42
-- ThreadId 44
-- [1,2,3,4,5]
-- @
--
--
-- Let's modify the above program such that the threads is less than
-- the number of items in the list:
--
-- @
-- import Say
-- 
-- action :: Int -> IO Int
-- action n = do
--   tid <- myThreadId
--   sayString $ show tid
--   threadDelay (2 * 10^6) -- 2 seconds
--   return n
-- 
-- main :: IO ()
-- main = do
--   yx \<- pooledMapConcurrently 3 (\\x -\> action x) [1..5]
--   print yx
-- @
-- On executing you can see that only three threads are active totally:
-- 
-- @
-- \$ ./pool
-- ThreadId 35
-- ThreadId 37
-- ThreadId 39
-- ThreadId 35
-- ThreadId 39
-- [1,2,3,4,5]
-- @
--
-- @since 0.2.9
pooledMapConcurrentlyN :: (MonadUnliftIO m, Traversable t) 
                      => Int -- ^ Max. number of threads. Should not be less than 1.
                      -> (a -> m b) -> t a -> m (t b)
pooledMapConcurrentlyN numProcs f xs = 
    withRunInIO $ \run -> pooledMapConcurrentlyIO numProcs (run . f) xs

-- | Similar to 'pooledMapConcurrentlyN' but with number of threads
-- set from 'getNumCapabilities'. Usually this is useful for CPU bound
-- tasks.
--
-- @since 0.2.9
pooledMapConcurrently :: (MonadUnliftIO m, Traversable t) => (a -> m b) -> t a -> m (t b)
pooledMapConcurrently f xs = do
  withRunInIO $ \run -> do
    numProcs <- getNumCapabilities
    pooledMapConcurrentlyIO numProcs (run . f) xs

-- | Similar to 'pooledMapConcurrentlyN' but with flipped arguments.
--
-- @since 0.2.9
pooledForConcurrentlyN :: (MonadUnliftIO m, Traversable t) 
                      => Int -- ^ Max. number of threads. Should not be less than 1.
                      -> t a -> (a -> m b) -> m (t b)
pooledForConcurrentlyN numProcs xs f = flip (pooledMapConcurrentlyN numProcs) xs f

-- | Similar to 'pooledForConcurrentlyN' but with number of threads
-- set from 'getNumCapabilities'. Usually this is useful for CPU bound
-- tasks.
--
-- @since 0.2.9
pooledForConcurrently :: (MonadUnliftIO m, Traversable t) => t a -> (a -> m b) -> m (t b)
pooledForConcurrently xs f = flip pooledMapConcurrently xs f

pooledMapConcurrentlyIO :: Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
pooledMapConcurrentlyIO numProcs f xs = 
    if (numProcs < 1)
    then error "pooledMapconcurrentlyIO: number of threads < 1"
    else pooledMapConcurrentlyIO' numProcs f xs

pooledMapConcurrentlyIO' ::
  Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
pooledMapConcurrentlyIO' numProcs f xs = do
  -- prepare one IORef per result...
  jobs :: t (a, IORef b) <-
    for xs (\x -> (x, ) <$> newIORef (error "pooledMapConcurrentlyIO': empty IORef"))
  -- ...put all the inputs in a queue..
  jobsVar :: MVar [(a, IORef b)] <- newMVar (toList jobs)
  -- ...run `numProcs` threads in parallel, each
  -- of them consuming the queue and filling in
  -- the respective IORefs.
  forConcurrently_ [1..numProcs] $ \_ -> do
    let loop  = do
          mbJob :: Maybe (a, IORef b) <- modifyMVar jobsVar $ \x -> case x of
            [] -> return ([], Nothing)
            var : vars -> return (vars, Just var)
          case mbJob of
            Nothing -> return ()
            Just (x, outRef) -> do
              y <- f x
              writeIORef outRef y
              loop
    loop
  -- Read all the IORefs
  for jobs (\(_, outputRef) -> readIORef outputRef)

pooledMapConcurrentlyIO_' ::
  Traversable t => Int -> (a -> IO b) -> t a -> IO ()
pooledMapConcurrentlyIO_' numProcs f jobs = do
  jobsVar :: MVar [a] <- newMVar (toList jobs)
  forConcurrently_ [1..numProcs] $ \_ -> do
    let loop  = do
          mbJob :: Maybe a <- modifyMVar jobsVar $ \x -> case x of
            [] -> return ([], Nothing)
            var : vars -> return (vars, Just var)
          case mbJob of
            Nothing -> return ()
            Just x -> do
              y <- f x
              loop
    loop
  return ()

pooledMapConcurrentlyIO_ :: Traversable t => Int -> (a -> IO b) -> t a -> IO ()
pooledMapConcurrentlyIO_ numProcs f xs = 
    if (numProcs < 1)
    then error "pooledMapconcurrentlyIO_: number of threads < 1"
    else pooledMapConcurrentlyIO_' numProcs f xs

-- | Like 'pooledMapConcurrentlyN' but with the return value
-- discarded.
--
-- @since 0.2.9
pooledMapConcurrentlyN_ :: (MonadUnliftIO m, Traversable f) 
                        => Int -- ^ Max. number of threads. Should not be less than 1.
                        -> (a -> m b) -> f a -> m ()
pooledMapConcurrentlyN_ numProcs f t = 
  withRunInIO $ \run -> pooledMapConcurrentlyIO_ numProcs (run . f) t

-- | Like 'pooledMapConcurrently' but with the return value discarded.
--
-- @since 0.2.9
pooledMapConcurrently_ :: (MonadUnliftIO m, Traversable f) => (a -> m b) -> f a -> m ()
pooledMapConcurrently_ f t = 
  withRunInIO $ \run -> do
    numProcs <- getNumCapabilities
    pooledMapConcurrentlyIO_ numProcs (run . f) t

-- | Pooled version of 'replicateConcurrently'. Performs the action in
-- the pooled threads.
--
-- @since 0.2.9
pooledReplicateConcurrentlyN :: (MonadUnliftIO m) 
                             => Int -- ^ Max. number of threads. Should not be less than 1.
                             -> Int -- ^ Number of times to perform the action.
                             -> m a -> m [a]
pooledReplicateConcurrentlyN numProcs cnt task = 
  pooledMapConcurrentlyN numProcs (\_ -> task) [1..cnt]

-- | Similar to 'pooledReplicateConcurrentlyN' but with number of
-- threads set from 'getNumCapabilities'. Usually this is useful for
-- CPU bound tasks.
--
-- @since 0.2.9
pooledReplicateConcurrently :: (MonadUnliftIO m) 
                            => Int -- ^ Number of times to perform the action.
                            -> m a -> m [a]
pooledReplicateConcurrently cnt task = 
  pooledMapConcurrently (\_ -> task) [1..cnt]

-- | Pooled version of 'replicateConcurrently_'. Performs the action in
-- the pooled threads.
--
-- @since 0.2.9
pooledReplicateConcurrentlyN_ :: (MonadUnliftIO m) 
                              => Int -- ^ Max. number of threads. Should not be less than 1.
                              -> Int -- ^ Number of times to perform the action.
                              -> m a -> m ()
pooledReplicateConcurrentlyN_ numProcs cnt task = 
  pooledMapConcurrentlyN_ numProcs (\_ -> task) [1..cnt]

-- | Similar to 'pooledReplicateConcurrently_' but with number of
-- threads set from 'getNumCapabilities'. Usually this is useful for
-- CPU bound tasks.
--
-- @since 0.2.9
pooledReplicateConcurrently_ :: (MonadUnliftIO m) 
                             => Int -- ^ Number of times to perform the action.
                             -> m a -> m ()
pooledReplicateConcurrently_ cnt task = 
  pooledMapConcurrently_ (\_ -> task) [1..cnt]

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
