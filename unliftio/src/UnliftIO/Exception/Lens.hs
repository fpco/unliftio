{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions from "Control.Exception.Lens", but using 'MonadUnliftIO', not
-- 'MonadCatch'
module UnliftIO.Exception.Lens
  ( catching
  , catching_
  , handling
  , handling_
  , trying
  , trying_
  ) where

import Prelude

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad (liftM)
import Data.Monoid (First)
import UnliftIO.Exception (SomeException, catchJust, tryJust)
import Control.Applicative (Const(..))
import Data.Monoid (First(..))

#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce
#else
import Unsafe.Coerce
#endif

-- | 'Control.Exception.Lens.catching' using 'MonadUnliftIO'
--
-- @since 0.2.25.0
catching :: MonadUnliftIO m => Getting (First a) SomeException a -> m r -> (a -> m r) -> m r
catching l = catchJust (preview l)
{-# INLINE catching #-}

-- | 'Control.Exception.Lens.catching_' using 'MonadUnliftIO'
--
-- @since 0.2.25.0
catching_ :: MonadUnliftIO m => Getting (First a) SomeException a -> m r -> m r -> m r
catching_ l a b = catchJust (preview l) a (const b)
{-# INLINE catching_ #-}

-- | 'Control.Exception.Lens.handling' using 'MonadUnliftIO'
--
-- @since 0.2.25.0
handling :: MonadUnliftIO m => Getting (First a) SomeException a -> (a -> m r) -> m r -> m r
handling l = flip (catching l)
{-# INLINE handling #-}

-- | 'Control.Exception.Lens.handling_' using 'MonadUnliftIO'
--
-- @since 0.2.25.0
handling_ :: MonadUnliftIO m => Getting (First a) SomeException a -> m r -> m r -> m r
handling_ l = flip (catching_ l)
{-# INLINE handling_ #-}

-- | 'Control.Exception.Lens.trying' using 'MonadUnliftIO'
--
-- @since 0.2.25.0
trying :: MonadUnliftIO m => Getting (First a) SomeException a -> m r -> m (Either a r)
trying l = tryJust (preview l)
{-# INLINE trying #-}

-- | 'Control.Exception.Lens.trying_' using 'MonadUnliftIO'
--
-- @since 0.2.25.0
trying_ :: MonadUnliftIO m => Getting (First a) SomeException a -> m r -> m (Maybe r)
trying_ l m = preview _Right `liftM` trying l m
{-# INLINE trying_ #-}

--------------------------------------------------------------------------------
-- Enough of (micro)lens to accomplish this mondule without any dependencies
--
-- TODO: code review note: should we just bring in microlens?
--------------------------------------------------------------------------------
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

_Right :: Traversal (Either a b) (Either a b') b b'
_Right f (Right b) = Right <$> f b
_Right _ (Left a) = pure (Left a)
{-# INLINE _Right #-}

type Getting r s a = (a -> Const r a) -> s -> Const r s

preview :: Getting (First a) s a -> s -> Maybe a
preview l = getFirst #. foldMapOf l (First #. Just)
{-# INLINE preview #-}

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l f = getConst #. l (Const #. f)
{-# INLINE foldMapOf #-}

#if __GLASGOW_HASKELL__ >= 708
( #. ) :: Coercible c b => (b -> c) -> (a -> b) -> (a -> c)
( #. ) _ = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b
#else
( #. ) :: (b -> c) -> (a -> b) -> (a -> c)
( #. ) _ = unsafeCoerce
#endif

{-# INLINE ( #. ) #-}

infixr 9 #.
