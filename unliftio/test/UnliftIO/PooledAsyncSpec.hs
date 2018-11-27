module UnliftIO.PooledAsyncSpec (spec) where

import Test.Hspec
import Control.Concurrent
import Data.List (nub, sort)
import Test.QuickCheck
import Data.Functor ((<$>))
import UnliftIO

data MyPooledException = PoolHellException
                         deriving Show

instance Exception MyPooledException

spec :: Spec
spec = do
  let exAction :: Int -> IO Int
      exAction x = do
        if (x == 2) then throwIO PoolHellException else return ()
        return x

      action :: Int -> IO ThreadId
      action x = do
         threadDelay (2 * 10^5)
         myThreadId

      myVar :: IO (TVar Int)
      myVar = atomically $ newTVar 0

      maxTVar :: Int -> TVar Int -> IO ()
      maxTVar cval tvar = do
         atomically $ do
           v <- readTVar tvar
           if cval >= v
           then writeTVar tvar cval
           else return ()

      poolException :: Selector MyPooledException
      poolException = const True

  describe "pooled mapConcurrencyN" $ do
    it "Throws exception properly" $ do
       (pooledMapConcurrentlyN 5 exAction [1..5]) `shouldThrow` poolException

    it "total thread should be >= 1" $ do
       (pooledMapConcurrentlyN 0 action [1..5]) `shouldThrow` anyErrorCall

    it "should not spawn more than five threads for five concurrent tasks" $ do
       xs <- (pooledMapConcurrentlyN 5 action [1..5])
       (length $ nub xs) `shouldSatisfy` (<= (5 :: Int))

    it "should not spawn more than three threads for five concurrent tasks" $ do
       xs <- (pooledMapConcurrentlyN 3 action [1..5])
       (length $ nub xs) `shouldSatisfy` (<= (3 :: Int))

    it "never uses more than the given number of pools and doesn't miss any return values" $
        forAllShrink ((+ 1) . abs <$> arbitrary) (filter (>= 1) . shrink) $ \threads ->
            property $ \list -> do
                threadIdsVar <- newTVarIO []
                let go :: Int -> IO Int
                    go i = do
                        tid <- myThreadId
                        atomically $ modifyTVar threadIdsVar (tid :)
                        return i
                list' <- pooledMapConcurrentlyN threads go list
                sort list' `shouldBe` sort list
                tids <- readTVarIO threadIdsVar
                length (nub tids) `shouldSatisfy` (<= threads)

  describe "pooled mapConcurrencyN_" $ do
    it "Throws exception properly" $ do
       (pooledMapConcurrentlyN_ 5 exAction [1..5]) `shouldThrow` poolException

    it "total thread should be >= 1" $ do
       (pooledMapConcurrentlyN_ 0 action [1..5]) `shouldThrow` anyErrorCall

    it "find proper maximum value" $ do
       var <- myVar
       xs <- (pooledMapConcurrentlyN_ 5 (\x -> maxTVar x var) [1..5])
       newVar <- atomically $ readTVar var
       atomically $ writeTVar var 0
       newVar `shouldBe` 5

    it "find proper maximum value with 2 threads" $ do
       var <- myVar
       xs <- (pooledMapConcurrentlyN_ 2 (\x -> maxTVar x var) [1..5])
       newVar <- atomically $ readTVar var
       atomically $ writeTVar var 0
       newVar `shouldBe` 5

    it "find proper maximum value with 1 threads" $ do
       var <- myVar
       xs <- (pooledMapConcurrentlyN_ 1 (\x -> maxTVar x var) [1..5])
       newVar <- atomically $ readTVar var
       atomically $ writeTVar var 0
       newVar `shouldBe` 5

    it "make sure activity is happening in different threads" $ do
       let myThreads :: IO (TVar [ThreadId])
           myThreads = atomically $ newTVar []

           collectThreads :: TVar [ThreadId] -> IO ()
           collectThreads threadVar = do
             tid <- myThreadId
             atomically $ do
               tvar <- readTVar threadVar
               writeTVar threadVar (tid:tvar)
             threadDelay $ 2 * 10^5

       tid <- myThreads
       xs <- pooledMapConcurrentlyN_ 5 (\_ -> collectThreads tid) [1..5]
       tids <- atomically $ readTVar tid
       (length $ nub tids) `shouldSatisfy` (<= 5)

    it "Not more than 5 threads will be spawned even if pooling is set to 8 " $ do
       let myThreads :: IO (TVar [ThreadId])
           myThreads = atomically $ newTVar []

           collectThreads :: TVar [ThreadId] -> IO ()
           collectThreads threadVar = do
             tid <- myThreadId
             atomically $ do
               tvar <- readTVar threadVar
               writeTVar threadVar (tid:tvar)
             threadDelay $ 2 * 10^5

       tid <- myThreads
       xs <- pooledMapConcurrentlyN_ 8 (\_ -> collectThreads tid) [1..5]
       tids <- atomically $ readTVar tid
       (length $ nub tids) `shouldSatisfy` (<= 5)

  describe "replicate concurrencyN" $ do
    it "Throws exception properly" $ do
       (pooledReplicateConcurrentlyN 5 1 (exAction 2)) `shouldThrow` poolException

    it "total thread should be >= 1" $ do
       (pooledReplicateConcurrentlyN 0 1 (action 1)) `shouldThrow` anyErrorCall

    it "Read tvar value should be 100" $ do
       var <- myVar
       xs <- (pooledReplicateConcurrentlyN 5 5 (maxTVar 100 var))
       newVar <- atomically $ readTVar var
       atomically $ writeTVar var 0
       newVar `shouldBe` 100

    it "should not spawn more than five threads for five concurrent tasks" $ do
       xs <- (pooledReplicateConcurrentlyN 5 5 (action 1))
       (length $ nub xs) `shouldSatisfy` (<= (5 :: Int))

    it "should not spawn more than three threads for five concurrent tasks" $ do
       xs <- (pooledReplicateConcurrentlyN 3 5 (action 1))
       (length $ nub xs) `shouldSatisfy` (<= (3 :: Int))

    it "should give empty list" $ do
       xs <- (pooledReplicateConcurrentlyN 3 0 (action 1))
       xs `shouldBe` []

    it "should give empty list for -ve count" $ do
       xs <- (pooledReplicateConcurrentlyN 3 (-3) (action 1))
       xs `shouldBe` []

  describe "pooled replicateConcurrencyN_" $ do
    it "Throws exception properly" $ do
       (pooledReplicateConcurrentlyN_ 5 1 (exAction 2)) `shouldThrow` poolException

    it "total thread should be >= 1" $ do
       (pooledReplicateConcurrentlyN_ 0 2 (action 1)) `shouldThrow` anyErrorCall

    it "find proper maximum value" $ do
       var <- myVar
       pooledReplicateConcurrentlyN_ 5 3 (maxTVar 200 var)
       newVar <- atomically $ readTVar var
       atomically $ writeTVar var 0
       newVar `shouldBe` 200

    it "Should be initial value" $ do
       var <- myVar
       pooledReplicateConcurrentlyN_ 5 (-2) (maxTVar 200 var)
       newVar <- atomically $ readTVar var
       atomically $ writeTVar var 0
       newVar `shouldBe` 0
