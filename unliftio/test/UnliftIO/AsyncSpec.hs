module UnliftIO.AsyncSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Modifiers
import UnliftIO
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (throwSTM)
import Control.Exception (getMaskingState, MaskingState (Unmasked))

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
  it "nesting" $ do
    var <- newEmptyMVar
    let sillyAlts :: Conc IO a -> Conc IO a
        sillyAlts c = c <|> conc (takeMVar var >> error "shouldn't happen")
    res <- runConc $ sillyAlts $ (+)
      <$> sillyAlts (conc (pure 1))
      <*> sillyAlts (conc (pure 2))
    res `shouldBe` 3
    putMVar var ()
  it "cleanup happens with Applicative" $ do
    var <- newTVarIO (0 :: Int)
    let worker = conc $ bracket_
          (atomically $ modifyTVar' var (+ 1))
          (atomically $ modifyTVar' var (subtract 1))
          (threadDelay 10000000 >> error "this should never happen")
        count = 10
        killer = conc $ atomically $ do
          count' <- readTVar var
          checkSTM $ count == count'
          throwSTM MyExc
        composed = foldr (*>) killer (replicate count worker)
    runConc composed `shouldThrow` (== MyExc)
    atomically (readTVar var) `shouldReturn` 0
  it "first exception wins" $ do
    let composed = conc (throwIO MyExc) *> conc (threadDelay 1000000 >> error "foo")
    runConc composed `shouldThrow` (== MyExc)
  it "child masking state" $
    uninterruptibleMask_ $
      runConc $ conc (threadDelay maxBound) <|>
        conc (getMaskingState `shouldReturn` Unmasked)
  it "parent is killable" $ do
    ref <- newIORef (0 :: Int)
    mres <- timeout 50000 $ runConc $
      conc (pure ()) *>
      conc ((writeIORef ref 1 >> threadDelay maxBound >> writeIORef ref 2) `finally` writeIORef ref 3)
    mres `shouldBe` Nothing
    res <- readIORef ref
    case res of
      0 -> putStrLn "make timeout longer"
      1 -> error "it's 1"
      2 -> error "it's 2"
      3 -> pure ()
      _ -> error $ "what? " ++ show res
