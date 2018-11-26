module UnliftIO.PooledAsyncSpec (spec) where

import Test.Hspec
import Control.Concurrent
import Data.List (nub, sort)
import Test.QuickCheck
import Data.Functor ((<$>))
import UnliftIO

spec :: Spec
spec = do
  let exAction :: Int -> IO Int
      exAction x = do
        if (x == 2) then error "hell" else return ()
        return x
               
      action :: Int -> IO ThreadId
      action x = do
         threadDelay (2 * 10^6)
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
                 
  describe "pooled mapConcurrencyN" $ do
    it "Throws exception properly" $ do
       (pooledMapConcurrentlyN 5 exAction [1..5]) `shouldThrow` anyErrorCall

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
       (pooledMapConcurrentlyN_ 5 exAction [1..5]) `shouldThrow` anyErrorCall

    it "total thread should be >= 1" $ do
       (pooledMapConcurrentlyN_ 0 action [1..5]) `shouldThrow` anyErrorCall
    
    it "find proper maximum value" $ do
       var <- myVar                                  
       xs <- (pooledMapConcurrentlyN_ 5 (\x -> maxTVar x var) [1..5])
       newVar <- atomically $ readTVar var
       atomically $ writeTVar var 0
       newVar `shouldBe` 5

  describe "replicate concurrencyN" $ do
    it "Throws exception properly" $ do
       (pooledReplicateConcurrentlyN 5 1 (exAction 2)) `shouldThrow` anyErrorCall

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
       (pooledReplicateConcurrentlyN_ 5 1 (exAction 2)) `shouldThrow` anyErrorCall

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

