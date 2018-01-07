module UnliftIO.IOSpec (spec) where

import Test.Hspec
import UnliftIO.IO
import Control.Concurrent (threadDelay)

spec :: Spec
spec = do
  describe "getMonotonicTime" $ do
    it "increases" $ do
      x <- getMonotonicTime
      threadDelay 5000
      y <- getMonotonicTime
      y - x `shouldSatisfy` (>= 5e-3)
