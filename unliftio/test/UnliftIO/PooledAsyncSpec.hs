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
                 
  describe "pooled mapConcurrency" $ do
    it "Throws exception properly" $ do
       (pooledMapConcurrently 5 exAction [1..5]) `shouldThrow` anyErrorCall
    
    it "spawns five threads for five concurrent tasks" $ do
       xs <- (pooledMapConcurrently 5 action [1..5])
       (length $ nub xs) `shouldBe` 5

    it "spawns three threads for five concurrent tasks" $ do
       xs <- (pooledMapConcurrently 3 action [1..5])
       (length $ nub xs) `shouldBe` 3
