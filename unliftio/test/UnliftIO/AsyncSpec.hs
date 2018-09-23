module UnliftIO.AsyncSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Modifiers
import UnliftIO

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
    runConcurrently (mkConcurrently (pure ()) *> mkConcurrently (throwIO MyExc))
      `shouldThrow` (== MyExc)
  it "Applicative instance" $ do
    runConcurrently (mkConcurrently (pure ()) *> mkConcurrently (throwIO MyExc))
      `shouldThrow` (== MyExc)
