{-# LANGUAGE CPP                 #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
module UnliftIO.Internals.Async where

import           Control.Applicative
import           Control.Concurrent       (threadDelay, getNumCapabilities)
import qualified Control.Concurrent       as C
import           Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as A
import           Control.Concurrent.STM
import           Control.Exception        (Exception, SomeException)
import           Control.Monad            (forever, liftM, unless, void, (>=>))
import           Control.Monad.IO.Unlift
import           Data.Foldable            (for_, traverse_)
import           Data.Typeable            (Typeable)
import           Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import           Data.IORef (IORef, readIORef, writeIORef, newIORef)
import qualified UnliftIO.Exception       as UE

-- For the implementation of Conc below, we do not want any of the
-- smart async exception handling logic from UnliftIO.Exception, since
-- (eg) we're low-level enough to need to explicit be throwing async
-- exceptions synchronously.
import qualified Control.Exception        as E
import           GHC.Generics             (Generic)

#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup
#else
import           Data.Monoid              hiding (Alt)
#endif
import           Data.Foldable            (Foldable, toList)
import           Data.Traversable         (Traversable, for, traverse)

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

-- | Composing two unlifted 'Concurrently' values using 'Alternative' is the
-- equivalent to using a 'race' combinator, the asynchrounous sub-routine that
-- returns a value first is the one that gets it's value returned, the slowest
-- sub-routine gets cancelled and it's thread is killed.
--
-- @since 0.1.0.0
instance MonadUnliftIO m => Alternative (Concurrently m) where
  -- | Care should be taken when using the 'empty' value of the 'Alternative'
  -- interface, as it will create a thread that delays for a long period of
  -- time. The reason behind this implementation is that any other computation
  -- will finish first than the 'empty' value. This implementation is less than
  -- ideal, and in a perfect world, we would have a typeclass family that allows
  -- '(<|>)' but not 'empty'.
  --
  -- @since 0.1.0.0
  empty = Concurrently $ liftIO (forever (threadDelay maxBound))
  Concurrently as <|> Concurrently bs =
    Concurrently $ liftM (either id id) (race as bs)

--------------------------------------------------------------------------------
#if MIN_VERSION_base(4,9,0)
--------------------------------------------------------------------------------
-- | Only defined by @async@ for @base >= 4.9@.
--
-- @since 0.1.0.0
instance (MonadUnliftIO m, Semigroup a) => Semigroup (Concurrently m a) where
  (<>) = liftA2 (<>)

-- | @since 0.1.0.0
instance (Semigroup a, Monoid a, MonadUnliftIO m) => Monoid (Concurrently m a) where
  mempty = pure mempty
  mappend = (<>)
--------------------------------------------------------------------------------
#else
--------------------------------------------------------------------------------
-- | @since 0.1.0.0
instance (Monoid a, MonadUnliftIO m) => Monoid (Concurrently m a) where
  mempty = pure mempty
  mappend = liftA2 mappend
--------------------------------------------------------------------------------
#endif
--------------------------------------------------------------------------------

-- | Similar to 'mapConcurrently' but with arguments flipped
--
-- @since 0.1.0.0
forConcurrently :: MonadUnliftIO m => Traversable t => t a -> (a -> m b) -> m (t b)
forConcurrently = flip mapConcurrently
{-# INLINE forConcurrently #-}

-- | Similar to 'mapConcurrently_' but with arguments flipped
--
-- @since 0.1.0.0
forConcurrently_ :: MonadUnliftIO m => Foldable f => f a -> (a -> m b) -> m ()
forConcurrently_ = flip mapConcurrently_
{-# INLINE forConcurrently_ #-}

-- | Unlifted 'A.replicateConcurrently'.
--
-- @since 0.1.0.0
#if MIN_VERSION_base(4,7,0)
#else
replicateConcurrently :: (Functor m, MonadUnliftIO m) => Int -> m a -> m [a]
#endif
replicateConcurrently cnt m =
  case compare cnt 1 of
    LT -> pure []
    EQ -> (:[]) <$> m
    GT -> mapConcurrently id (replicate cnt m)
{-# INLINE replicateConcurrently #-}

-- | Unlifted 'A.replicateConcurrently_'.
--
-- @since 0.1.0.0
#if MIN_VERSION_base(4,7,0)
replicateConcurrently_ :: (Applicative m, MonadUnliftIO m) => Int -> m a -> m ()
#else
replicateConcurrently_ :: (MonadUnliftIO m) => Int -> m a -> m ()
#endif
replicateConcurrently_ cnt m =
  case compare cnt 1 of
    LT -> pure ()
    EQ -> void m
    GT -> mapConcurrently_ id (replicate cnt m)
{-# INLINE replicateConcurrently_ #-}

-- Conc uses GHC features that are not supported in versions <= to ghc-7.10
-- so we are going to export/use it when we have a higher version only.
--------------------------------------------------------------------------------
#if MIN_VERSION_base(4,8,0)
--------------------------------------------------------------------------------

-- | Executes a 'Traversable' container of items concurrently, it uses the 'Flat'
-- type internally.
--
-- @since 0.1.0.0
mapConcurrently :: MonadUnliftIO m => Traversable t => (a -> m b) -> t a -> m (t b)
mapConcurrently f t = withRunInIO $ \run -> runFlat $ traverse
  (FlatApp . FlatAction . run . f)
  t
{-# INLINE mapConcurrently #-}

-- | Executes a 'Traversable' container of items concurrently, it uses the 'Flat'
-- type internally. This function ignores the results.
--
-- @since 0.1.0.0
mapConcurrently_ :: MonadUnliftIO m => Foldable f => (a -> m b) -> f a -> m ()
mapConcurrently_ f t = withRunInIO $ \run -> runFlat $ traverse_
  (FlatApp . FlatAction . run . f)
  t
{-# INLINE mapConcurrently_ #-}


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
  Apply   :: Conc m (v -> a) -> Conc m v -> Conc m a
  LiftA2 :: (x -> y -> a) -> Conc m x -> Conc m y -> Conc m a

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
-- fmap f (Action routine) = Action (fmap f routine)
-- fmap f (LiftA2 g x y)   = LiftA2 (fmap f g) x y
-- fmap f (Pure val)       = Pure (f val)
-- fmap f (Alt a b)        = Alt (fmap f a) (fmap f b)
-- fmap f Empty            = Empty

-- | Construct a value of type 'Conc' from an action. Compose these
-- values using the typeclass instances (most commonly 'Applicative'
-- and 'Alternative') and then run with 'runConc'.
--
-- @since 0.2.9.0
conc :: m a -> Conc m a
conc = Action
{-# INLINE conc #-}

-- | Run a 'Conc' value on multiple threads.
--
-- @since 0.2.9.0
runConc :: MonadUnliftIO m => Conc m a -> m a
runConc = flatten >=> (liftIO . runFlat)
{-# INLINE runConc #-}

-- | @since 0.2.9.0
instance MonadUnliftIO m => Applicative (Conc m) where
  pure = Pure
  {-# INLINE pure #-}
  -- | Following is an example of how an 'Applicative' expands to a Tree
  --
  -- @@@
  -- downloadA :: IO String
  -- downloadB :: IO String
  --
  -- (f <$> conc downloadA <*> conc downloadB <*> pure 123)
  --
  --   (((f <$> a) <*> b) <*> c))
  --        (1)    (2)    (3)
  --
  -- (1)
  --   Action (fmap f downloadA)
  -- (2)
  --   Apply (Action (fmap f downloadA)) (Action downloadB)
  -- (3)
  --   Apply (Apply (Action (fmap f downloadA)) (Action downloadB))
  --        (Pure 123)
  -- @@@
  --
  (<*>) = Apply
  {-# INLINE (<*>) #-}
  -- See comment above on Then
  -- (*>) = Then
#if MIN_VERSION_base(4,11,0)
  liftA2 = LiftA2
  {-# INLINE liftA2 #-}
#endif

  a *> b = LiftA2 (\_ x -> x) a b
  {-# INLINE (*>) #-}

-- | @since 0.2.9.0
instance MonadUnliftIO m => Alternative (Conc m) where
  empty = Empty -- this is so ugly, we don't actually want to provide it!
  {-# INLINE empty #-}
  (<|>) = Alt
  {-# INLINE (<|>) #-}

#if MIN_VERSION_base(4, 11, 0)
-- | @since 0.2.9.0
instance (MonadUnliftIO m, Semigroup a) => Semigroup (Conc m a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}
#endif

-- | @since 0.2.9.0
instance (Monoid a, MonadUnliftIO m) => Monoid (Conc m a) where
  mempty = pure mempty
  {-# INLINE mempty #-}
  mappend = liftA2 mappend
  {-# INLINE mappend #-}

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
  = FlatApp !(FlatApp a)
  -- | Flattened Alternative. Has at least 2 entries, which must be
  -- FlatApp (no nesting of FlatAlts).
  | FlatAlt !(FlatApp a) !(FlatApp a) ![FlatApp a]

deriving instance Functor Flat
-- fmap f (FlatApp a) =
--  FlatApp (fmap f a)
-- fmap f (FlatAlt (FlatApp a) (FlatApp b) xs) =
--   FlatAlt (FlatApp (fmap f a)) (FlatApp (fmap f b)) (map (fmap f) xs)
instance Applicative Flat where
  pure = FlatApp . pure
  (<*>) f a = FlatApp (FlatLiftA2 id f a)
#if MIN_VERSION_base(4,11,0)
  liftA2 f a b = FlatApp (FlatLiftA2 f a b)
#endif

-- | Flattened Applicative. No Alternative stuff directly in here, but may be in
-- the children. Notice this type doesn't have a type parameter for monadic
-- contexts, it hardwires the base monad to IO given concurrency relies
-- eventually on that.
--
-- @since 0.2.9.0
data FlatApp a where
  FlatPure   :: a -> FlatApp a
  FlatAction :: IO a -> FlatApp a
  FlatApply   :: Flat (v -> a) -> Flat v -> FlatApp a
  FlatLiftA2 :: (x -> y -> a) -> Flat x -> Flat y -> FlatApp a

deriving instance Functor FlatApp
instance Applicative FlatApp where
  pure = FlatPure
  (<*>) mf ma = FlatApply (FlatApp mf) (FlatApp ma)
#if MIN_VERSION_base(4,11,0)
  liftA2 f a b = FlatLiftA2 f (FlatApp a) (FlatApp b)
#endif

-- | Things that can go wrong in the structure of a 'Conc'. These are
-- /programmer errors/.
--
-- @since 0.2.9.0
data ConcException
  = EmptyWithNoAlternative
  deriving (Generic, Show, Typeable, Eq, Ord)
instance E.Exception ConcException

-- | Simple difference list, for nicer types below
type DList a = [a] -> [a]

dlistConcat :: DList a -> DList a -> DList a
dlistConcat = (.)
{-# INLINE dlistConcat #-}

dlistCons :: a -> DList a -> DList a
dlistCons a as = dlistSingleton a `dlistConcat` as
{-# INLINE dlistCons #-}

dlistConcatAll :: [DList a] -> DList a
dlistConcatAll = foldr (.) id
{-# INLINE dlistConcatAll #-}

dlistToList :: DList a -> [a]
dlistToList = ($ [])
{-# INLINE dlistToList #-}

dlistSingleton :: a -> DList a
dlistSingleton a = (a:)
{-# INLINE dlistSingleton #-}

dlistEmpty :: DList a
dlistEmpty = id
{-# INLINE dlistEmpty #-}

-- | Turn a 'Conc' into a 'Flat'. Note that thanks to the ugliness of
-- 'empty', this may fail, e.g. @flatten Empty@.
--
-- @since 0.2.9.0
flatten :: forall m a. MonadUnliftIO m => Conc m a -> m (Flat a)
flatten c0 = withRunInIO $ \run -> do

  -- why not app?
  let both :: forall k. Conc m k -> IO (Flat k)
      both Empty = E.throwIO EmptyWithNoAlternative
      both (Action m) = pure $ FlatApp $ FlatAction $ run m
      both (Apply cf ca) = do
        f <- both cf
        a <- both ca
        pure $ FlatApp $ FlatApply f a
      both (LiftA2 f ca cb) = do
        a <- both ca
        b <- both cb
        pure $ FlatApp $ FlatLiftA2 f a b
      both (Alt ca cb) = do
        a <- alt ca
        b <- alt cb
        case dlistToList (a `dlistConcat` b) of
          []    -> E.throwIO EmptyWithNoAlternative
          [x]   -> pure $ FlatApp x
          x:y:z -> pure $ FlatAlt x y z
      both (Pure a) = pure $ FlatApp $ FlatPure a

      -- Returns a difference list for cheaper concatenation
      alt :: forall k. Conc m k -> IO (DList (FlatApp k))
      alt Empty = pure dlistEmpty
      alt (Apply cf ca) = do
        f <- both cf
        a <- both ca
        pure (dlistSingleton $ FlatApply f a)
      alt (Alt ca cb) = do
        a <- alt ca
        b <- alt cb
        pure $ a `dlistConcat` b
      alt (Action m) = pure (dlistSingleton $ FlatAction (run m))
      alt (LiftA2 f ca cb) = do
        a <- both ca
        b <- both cb
        pure (dlistSingleton $ FlatLiftA2 f a b)
      alt (Pure a) = pure (dlistSingleton $ FlatPure a)

  both c0

-- | Run a @Flat a@ on multiple threads.
runFlat :: Flat a -> IO a

-- Silly, simple optimizations
runFlat (FlatApp (FlatAction io)) = io
runFlat (FlatApp (FlatPure x)) = pure x

-- Start off with all exceptions masked so we can install proper cleanup.
runFlat f0 = E.uninterruptibleMask $ \restore -> do
  -- How many threads have been spawned and finished their task? We need to
  -- ensure we kill all child threads and wait for them to die.
  resultCountVar <- newTVarIO 0

  -- Forks off as many threads as necessary to run the given Flat a,
  -- and returns:
  --
  -- + An STM action that will block until completion and return the
  --   result.
  --
  -- + The IDs of all forked threads. These need to be tracked so they
  --   can be killed (either when an exception is thrown, or when one
  --   of the alternatives completes first).
  --
  -- It would be nice to have the returned STM action return an Either
  -- and keep the SomeException values somewhat explicit, but in all
  -- my testing this absolutely kills performance. Instead, we're
  -- going to use a hack of providing a TMVar to fill up with a
  -- SomeException when things fail.
  --
  -- TODO: Investigate why performance degradation on Either
  let go :: forall a.
            TMVar E.SomeException
         -> Flat a
         -> IO (STM a, DList C.ThreadId)
      go _excVar (FlatApp (FlatPure x)) = pure (pure x, dlistEmpty)
      go excVar (FlatApp (FlatAction io)) = do
        resVar <- newEmptyTMVarIO
        tid <- C.forkIOWithUnmask $ \restore1 -> do
          res <- E.try $ restore1 io
          atomically $ do
            modifyTVar' resultCountVar (+ 1)
            case res of
              Left e  -> void $ tryPutTMVar excVar e
              Right x -> putTMVar resVar x
        pure (readTMVar resVar, dlistSingleton tid)
      go excVar (FlatApp (FlatApply cf ca)) = do
        (f, tidsf) <- go excVar cf
        (a, tidsa) <- go excVar ca
        pure (f <*> a, tidsf `dlistConcat` tidsa)
      go excVar (FlatApp (FlatLiftA2 f a b)) = do
        (a', tidsa) <- go excVar a
        (b', tidsb) <- go excVar b
        pure (liftA2 f a' b', tidsa `dlistConcat` tidsb)

      go excVar0 (FlatAlt x y z) = do
        -- As soon as one of the children finishes, we need to kill the siblings,
        -- we're going to create our own excVar here to pass to the children, so
        -- we can prevent the ThreadKilled exceptions we throw to the children
        -- here from propagating and taking down the whole system.
        excVar <- newEmptyTMVarIO
        resVar <- newEmptyTMVarIO
        pairs <- traverse (go excVar . FlatApp) (x:y:z)
        let (blockers, workerTids) = unzip pairs

        -- Fork a helper thread to wait for the first child to
        -- complete, or for one of them to die with an exception so we
        -- can propagate it to excVar0.
        helperTid <- C.forkIOWithUnmask $ \restore1 -> do
          eres <- E.try $ restore1 $ atomically $ foldr
            (\blocker rest -> (Right <$> blocker) <|> rest)
            (Left <$> readTMVar excVar)
            blockers
          atomically $ do
            modifyTVar' resultCountVar (+ 1)
            case eres of
              -- NOTE: The child threads are spawned from @traverse go@ call above, they
              -- are _not_ children of this helper thread, and helper thread doesn't throw
              -- synchronous exceptions, so, any exception that the try above would catch
              -- must be an async exception.
              -- We were killed by an async exception, do nothing.
              Left (_ :: E.SomeException) -> pure ()
              -- Child thread died, propagate it
              Right (Left e)              -> void $ tryPutTMVar excVar0 e
              -- Successful result from one of the children
              Right (Right res)           -> putTMVar resVar res

          -- And kill all of the threads
          for_ workerTids $ \tids' ->
            -- NOTE: Replacing A.AsyncCancelled with KillThread as the
            -- 'A.AsyncCancelled' constructor is not exported in older versions
            -- of the async package
            -- for_ (tids' []) $ \workerTid -> E.throwTo workerTid A.AsyncCancelled
            for_ (dlistToList tids') $ \workerTid -> C.killThread workerTid

        pure ( readTMVar resVar
             , helperTid `dlistCons` dlistConcatAll workerTids
             )

  excVar <- newEmptyTMVarIO
  (getRes, tids0) <- go excVar f0
  let tids = dlistToList tids0
      tidCount = length tids
      allDone count =
        if count > tidCount
          then error ("allDone: count ("
                      <> show count
                      <> ") should never be greater than tidCount ("
                      <> show tidCount
                      <> ")")
          else count == tidCount

  -- Automatically retry if we get killed by a
  -- BlockedIndefinitelyOnSTM. For more information, see:
  --
  -- + https:\/\/github.com\/simonmar\/async\/issues\/14
  -- + https:\/\/github.com\/simonmar\/async\/pull\/15
  --
  let autoRetry action =
        action `E.catch`
        \E.BlockedIndefinitelyOnSTM -> autoRetry action

  -- Restore the original masking state while blocking and catch
  -- exceptions to allow the parent thread to be killed early.
  res <- E.try $ restore $ autoRetry $ atomically $
         (Left <$> readTMVar excVar) <|>
         (Right <$> getRes)

  count0 <- atomically $ readTVar resultCountVar
  unless (allDone count0) $ do
    -- Kill all of the threads
    -- NOTE: Replacing A.AsyncCancelled with KillThread as the
    -- 'A.AsyncCancelled' constructor is not exported in older versions
    -- of the async package
    -- for_ tids $ \tid -> E.throwTo tid A.AsyncCancelled
    for_ tids $ \tid -> C.killThread tid

    -- Wait for all of the threads to die. We're going to restore the original
    -- masking state here, just in case there's a bug in the cleanup code of a
    -- child thread, so that we can be killed by an async exception. We decided
    -- this is a better behavior than hanging indefinitely and wait for a SIGKILL.
    restore $ atomically $ do
      count <- readTVar resultCountVar
      -- retries until resultCountVar has increased to the threadId count returned by go
      check $ allDone count

  -- Return the result or throw an exception. Yes, we could use
  -- either or join, but explicit pattern matching is nicer here.
  case res of
    -- Parent thread was killed with an async exception
    Left e          -> E.throwIO (e :: E.SomeException)
    -- Some child thread died
    Right (Left e)  -> E.throwIO e
    -- Everything worked!
    Right (Right x) -> pure x
{-# INLINEABLE runFlat #-}

--------------------------------------------------------------------------------
#else
--------------------------------------------------------------------------------

-- | Unlifted 'A.mapConcurrently'.
--
-- @since 0.1.0.0
mapConcurrently :: MonadUnliftIO m => Traversable t => (a -> m b) -> t a -> m (t b)
mapConcurrently f t = withRunInIO $ \run -> A.mapConcurrently (run . f) t
{-# INLINE mapConcurrently #-}

-- | Unlifted 'A.mapConcurrently_'.
--
-- @since 0.1.0.0
mapConcurrently_ :: MonadUnliftIO m => Foldable f => (a -> m b) -> f a -> m ()
mapConcurrently_ f t = withRunInIO $ \run -> A.mapConcurrently_ (run . f) t
{-# INLINE mapConcurrently_ #-}

--------------------------------------------------------------------------------
#endif
--------------------------------------------------------------------------------

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
--   yx \<- pooledMapConcurrentlyN 5 (\\x -\> action x) [1..5]
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
-- Let's modify the above program such that there are less threads
-- than the number of items in the list:
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
--   yx \<- pooledMapConcurrentlyN 3 (\\x -\> action x) [1..5]
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
-- @since 0.2.10
pooledMapConcurrentlyN :: (MonadUnliftIO m, Traversable t)
                      => Int -- ^ Max. number of threads. Should not be less than 1.
                      -> (a -> m b) -> t a -> m (t b)
pooledMapConcurrentlyN numProcs f xs =
    withRunInIO $ \run -> pooledMapConcurrentlyIO numProcs (run . f) xs

-- | Similar to 'pooledMapConcurrentlyN' but with number of threads
-- set from 'getNumCapabilities'. Usually this is useful for CPU bound
-- tasks.
--
-- @since 0.2.10
pooledMapConcurrently :: (MonadUnliftIO m, Traversable t) => (a -> m b) -> t a -> m (t b)
pooledMapConcurrently f xs = do
  withRunInIO $ \run -> do
    numProcs <- getNumCapabilities
    pooledMapConcurrentlyIO numProcs (run . f) xs

-- | Similar to 'pooledMapConcurrentlyN' but with flipped arguments.
--
-- @since 0.2.10
pooledForConcurrentlyN :: (MonadUnliftIO m, Traversable t)
                      => Int -- ^ Max. number of threads. Should not be less than 1.
                      -> t a -> (a -> m b) -> m (t b)
pooledForConcurrentlyN numProcs xs f = flip (pooledMapConcurrentlyN numProcs) xs f

-- | Similar to 'pooledForConcurrentlyN' but with number of threads
-- set from 'getNumCapabilities'. Usually this is useful for CPU bound
-- tasks.
--
-- @since 0.2.10
pooledForConcurrently :: (MonadUnliftIO m, Traversable t) => t a -> (a -> m b) -> m (t b)
pooledForConcurrently xs f = flip pooledMapConcurrently xs f

pooledMapConcurrentlyIO :: Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
pooledMapConcurrentlyIO numProcs f xs =
    if (numProcs < 1)
    then error "pooledMapconcurrentlyIO: number of threads < 1"
    else pooledMapConcurrentlyIO' numProcs f xs

pooledConcurrently
  :: Int -> MVar [a] -> (a -> IO b) -> IO ()
pooledConcurrently numProcs jobsVar f = do
  forConcurrently_ [1..numProcs] $ \_ -> do
    let loop  = do
          mbJob :: Maybe a <- modifyMVar jobsVar $ \x -> case x of
            [] -> return ([], Nothing)
            var : vars -> return (vars, Just var)
          case mbJob of
            Nothing -> return ()
            Just x -> do
              _ <- f x
              loop
    loop

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
  pooledConcurrently numProcs jobsVar $ \ (x, outRef) -> f x >>= writeIORef outRef      -- Read all the IORefs
  for jobs (\(_, outputRef) -> readIORef outputRef)

pooledMapConcurrentlyIO_' ::
  Traversable t => Int -> (a -> IO b) -> t a -> IO ()
pooledMapConcurrentlyIO_' numProcs f jobs = do
  jobsVar :: MVar [a] <- newMVar (toList jobs)
  pooledConcurrently numProcs jobsVar f

pooledMapConcurrentlyIO_ :: Traversable t => Int -> (a -> IO b) -> t a -> IO ()
pooledMapConcurrentlyIO_ numProcs f xs =
    if (numProcs < 1)
    then error "pooledMapconcurrentlyIO_: number of threads < 1"
    else pooledMapConcurrentlyIO_' numProcs f xs

-- | Like 'pooledMapConcurrentlyN' but with the return value
-- discarded.
--
-- @since 0.2.10
pooledMapConcurrentlyN_ :: (MonadUnliftIO m, Traversable f)
                        => Int -- ^ Max. number of threads. Should not be less than 1.
                        -> (a -> m b) -> f a -> m ()
pooledMapConcurrentlyN_ numProcs f t =
  withRunInIO $ \run -> pooledMapConcurrentlyIO_ numProcs (run . f) t

-- | Like 'pooledMapConcurrently' but with the return value discarded.
--
-- @since 0.2.10
pooledMapConcurrently_ :: (MonadUnliftIO m, Traversable f) => (a -> m b) -> f a -> m ()
pooledMapConcurrently_ f t =
  withRunInIO $ \run -> do
    numProcs <- getNumCapabilities
    pooledMapConcurrentlyIO_ numProcs (run . f) t

-- | Like 'pooledMapConcurrently_' but with flipped arguments.
--
-- @since 0.2.10
pooledForConcurrently_ :: (MonadUnliftIO m, Traversable f) => f a -> (a -> m b) -> m ()
pooledForConcurrently_ t f = flip pooledMapConcurrently_ t f

-- | Like 'pooledMapConcurrentlyN_' but with flipped arguments.
--
-- @since 0.2.10
pooledForConcurrentlyN_ :: (MonadUnliftIO m, Traversable t)
                        => Int -- ^ Max. number of threads. Should not be less than 1.
                        -> t a -> (a -> m b) -> m ()
pooledForConcurrentlyN_ numProcs xs f = flip (pooledMapConcurrentlyN_ numProcs) xs f


-- | Pooled version of 'replicateConcurrently'. Performs the action in
-- the pooled threads.
--
-- @since 0.2.10
pooledReplicateConcurrentlyN :: (MonadUnliftIO m)
                             => Int -- ^ Max. number of threads. Should not be less than 1.
                             -> Int -- ^ Number of times to perform the action.
                             -> m a -> m [a]
pooledReplicateConcurrentlyN numProcs cnt task =
    if cnt < 1
    then return []
    else pooledMapConcurrentlyN numProcs (\_ -> task) [1..cnt]

-- | Similar to 'pooledReplicateConcurrentlyN' but with number of
-- threads set from 'getNumCapabilities'. Usually this is useful for
-- CPU bound tasks.
--
-- @since 0.2.10
pooledReplicateConcurrently :: (MonadUnliftIO m)
                            => Int -- ^ Number of times to perform the action.
                            -> m a -> m [a]
pooledReplicateConcurrently cnt task =
    if cnt < 1
    then return []
    else pooledMapConcurrently (\_ -> task) [1..cnt]

-- | Pooled version of 'replicateConcurrently_'. Performs the action in
-- the pooled threads.
--
-- @since 0.2.10
pooledReplicateConcurrentlyN_ :: (MonadUnliftIO m)
                              => Int -- ^ Max. number of threads. Should not be less than 1.
                              -> Int -- ^ Number of times to perform the action.
                              -> m a -> m ()
pooledReplicateConcurrentlyN_ numProcs cnt task =
  if cnt < 1
  then return ()
  else pooledMapConcurrentlyN_ numProcs (\_ -> task) [1..cnt]

-- | Similar to 'pooledReplicateConcurrently_' but with number of
-- threads set from 'getNumCapabilities'. Usually this is useful for
-- CPU bound tasks.
--
-- @since 0.2.10
pooledReplicateConcurrently_ :: (MonadUnliftIO m)
                             => Int -- ^ Number of times to perform the action.
                             -> m a -> m ()
pooledReplicateConcurrently_ cnt task =
  if cnt < 1
  then return ()
  else pooledMapConcurrently_ (\_ -> task) [1..cnt]
