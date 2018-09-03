{-# LANGUAGE DeriveDataTypeable #-}
module UnliftIO.MemoizeSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import Test.Hspec
import Test.Hspec.QuickCheck
import UnliftIO
import Data.Typeable

data Dummy = Dummy
  deriving (Show, Typeable)
instance Exception Dummy

spec :: Spec
spec = do
  let basics maker = do
        prop "sanity" $ \i -> do
          x <- maker $ return (i :: Int)
          getMemoized x `shouldReturn` i
        prop "runs once" $ \i -> do
          count <- newIORef (0 :: Int)
          x <- maker $ do
            modifyIORef' count (+ 1)
            return (i :: Int)
          replicateM_ 10 $ getMemoized x `shouldReturn` i
          readIORef count `shouldReturn` 1
        it "runs once with exception" $ do
          count <- newIORef (0 :: Int)
          x <- maker $ do
            modifyIORef' count (+ 1)
            throwIO Dummy
          replicateM_ 10 $ getMemoized x `shouldThrow` (\Dummy -> True)
          readIORef count `shouldReturn` 1
  describe "memoizeRef" $ basics memoizeRef
  describe "memoizeMVar" $ do
    basics memoizeMVar
    prop "runs once in multiple threads" $ \i -> do
      count <- newIORef (0 :: Int)
      x <- memoizeMVar $ do
        threadDelay 10000
        atomicModifyIORef' count $ \cnt -> (cnt + 1, ())
        return (i :: Int)
      replicateConcurrently_ 10 $ getMemoized x `shouldReturn` i
      readIORef count `shouldReturn` 1
