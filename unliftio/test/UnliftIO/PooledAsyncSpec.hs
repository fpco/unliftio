module UnliftIO.PooledAsyncSpec (spec) where

import Test.Hspec
import Control.Concurrent
import Data.List (nub)
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
