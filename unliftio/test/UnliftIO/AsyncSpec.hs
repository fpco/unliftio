module UnliftIO.AsyncSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Modifiers
import UnliftIO
import Control.Applicative
import Control.Concurrent (threadDelay)

data MyExc = MyExc
  deriving (Show, Eq, Typeable)
instance Exception MyExc

spec :: Spec
spec = do
  prop "replicateConcurrently_ works" $ \(NonNegative cnt) -> do
    ref <- newIORef (0 :: Int)
    replicateConcurrently_ cnt $ atomicModifyIORef' ref $ \i -> (i + 1, ())
    readIORef ref `shouldReturn` cnt
  it "handles exceptions" $ do
    runConc (conc (pure ()) *> conc (throwIO MyExc))
      `shouldThrow` (== MyExc)
  it "Applicative instance" $ do
    runConc (conc (pure ()) *> conc (throwIO MyExc))
      `shouldThrow` (== MyExc)
  it "Alternative instance" $ do
    var <- newEmptyMVar
    runConc $
      conc (takeMVar var) <|>
      conc (threadDelay maxBound) <|>
      conc (pure ())
    putMVar var () -- ensure the takeMVar doesn't get an exception
