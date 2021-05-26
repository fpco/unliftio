module UnliftIO.ExceptionSpec (spec) where

import Control.Monad (void)
import Data.Bifunctor (first)
import Test.Hspec
import UnliftIO
import UnliftIO.Concurrent (threadDelay)

spec :: Spec
spec = do
  let -- The callback will run in a thread that gets cancelled immediately,
      -- then get Exception2 thrown synchronously after 1 second.
      withAsyncExceptionThrown :: (IO a -> IO b) -> IO b
      withAsyncExceptionThrown f = do
        var <- newEmptyMVar
        a <- async $ f $ do
          putMVar var ()
          threadDelay 1000000
          throwIO Exception2
        -- wait until thread is running, then cancel
        takeMVar var
        cancel a
        -- check result
        wait a
      -- The callback will run in a thread that gets Exception1 thrown as
      -- an async exception immediately, then get Exception2 thrown
      -- synchronously after 1 second.
      withWrappedAsyncExceptionThrown :: (IO a -> IO b) -> IO b
      withWrappedAsyncExceptionThrown f = do
        var <- newEmptyMVar
        a <- async $ f $ do
          putMVar var ()
          threadDelay 1000000
          throwIO Exception2
        -- wait until thread is running, then cancel
        takeMVar var
        throwTo (asyncThreadId a) Exception1
        -- check result
        wait a
  describe "catchSyncOrAsync" $ do
    it "should catch sync exceptions" $ do
      result <- (`catchSyncOrAsync` return) $ throwIO Exception1
      result `shouldBe` Exception1
    it "should catch async exceptions" $ do
      result <- withAsyncExceptionThrown $ \m -> m `catchSyncOrAsync` return
      result `shouldBe` AsyncCancelled
    it "should catch unliftio-wrapped async exceptions" $ do
      result <- withWrappedAsyncExceptionThrown $ \m -> m `catchSyncOrAsync` return
      fromAsyncException result `shouldBe` Just Exception1
  describe "handleSyncOrAsync" $ do
    it "should catch sync exceptions" $ do
      result <- handleSyncOrAsync return $ throwIO Exception1
      result `shouldBe` Exception1
    it "should catch async exceptions" $ do
      result <- withAsyncExceptionThrown $ \m -> handleSyncOrAsync return m
      result `shouldBe` AsyncCancelled
    it "should catch unliftio-wrapped async exceptions" $ do
      result <- withWrappedAsyncExceptionThrown $ \m -> handleSyncOrAsync return m
      fromAsyncException result `shouldBe` Just Exception1
  describe "trySyncOrAsync" $ do
    it "should catch sync exceptions" $ do
      result <- trySyncOrAsync $ void $ throwIO Exception1
      result `shouldBe` Left Exception1
    it "should catch async exceptions" $ do
      result <- withAsyncExceptionThrown $ \m -> trySyncOrAsync (void m)
      result `shouldBe` Left AsyncCancelled
    it "should catch unliftio-wrapped async exceptions" $ do
      result <- withWrappedAsyncExceptionThrown $ \m -> trySyncOrAsync (void m)
      first fromAsyncException result `shouldBe` Left (Just Exception1)

  describe "fromAsyncException" $ do
    it "should be the inverse of toAsyncException" $ do
      fromAsyncException (toAsyncException Exception1) `shouldBe` Just Exception1

  let shouldLeft x = either (const Nothing) Just x `shouldBe` Nothing
      shouldRight x = either (Just . show) (const Nothing) x `shouldBe` Nothing
  describe "pureTry" $ do
    it "Right for defined values" $ shouldRight $ pureTry ()
    it "Left for bottom" $ shouldLeft $ pureTry (undefined :: ())
    it "Right for wrapped bottom" $ shouldRight $ pureTry $ Just (undefined :: ())
  describe "pureTryDeep" $ do
    it "Right for defined values" $ shouldRight $ pureTryDeep ()
    it "Left for bottom" $ shouldLeft $ pureTryDeep (undefined :: ())
    it "Left for wrapped bottom" $ shouldLeft $ pureTryDeep $ Just (undefined :: ())

  describe "mapExceptionM" $ do
    it "should convert an exception" $ do
      result <- try $ mapExceptionM (\Exception1 -> Exception2) (throwIO Exception1)
      result `shouldBe` (Left Exception2 :: Either Exception2 ())
    it "should not convert unrelated exceptions" $ do
      result <- try $ mapExceptionM (\Exception1 -> Exception2) (throwIO Exception2)
      result `shouldBe` (Left Exception2 :: Either Exception2 ())

data Exception1 = Exception1 deriving (Show, Eq)
instance Exception Exception1

data Exception2 = Exception2 deriving (Show, Eq)
instance Exception Exception2
