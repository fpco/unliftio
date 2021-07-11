import Criterion
import Criterion.Main
import Criterion.Types (Config(..))
import Control.Concurrent (threadDelay)
-- import UnliftIO
import qualified UnliftIO.Internals.Async as SUT
import qualified Control.Concurrent.Async as A
import Data.List (foldl')
import Data.IORef
import Control.Applicative (liftA2, (<|>), empty)

sizes :: (Int -> [Benchmark]) -> [Benchmark]
sizes f = map
  (\size -> bgroup (show size) (f size))
  [1, 100, 1000, 10000, 20000, 40000, 80000, 160000, 320000, 640000]

sum' :: [Int] -> Int
sum' = foldl' (+) 0
{-# INLINE sum' #-}

replicateA_ :: Applicative f => Int -> f () -> f ()
replicateA_ cnt0 f =
    let go 1 = f
        go i = f *> go (i - 1)
     in go cnt0
{-# INLINE replicateA_ #-}

main :: IO ()
main =
  defaultMainWith
    (defaultConfig {reportFile = Just "./out/conc-bench_report.html"})
    [
      bgroup "concurrently, no results" $ sizes $ \size ->
      [ bench "A.replicateConcurrently_" $ whnfIO $ A.replicateConcurrently_ size (pure ())
      , bench "replicateConcurrently_" $ whnfIO $ SUT.replicateConcurrently_ size (pure ())
      , bench "Conc" $ whnfIO $ SUT.runConc $ replicateA_ size $ SUT.conc $ pure ()
      , bench "Conc, cheating" $ whnfIO $ SUT.runConc $ replicateA_ size $ pure ()
      ]

    , bgroup "concurrently, with results" $ sizes $ \size ->
        [ bench "A.mapConcurrently" $ whnfIO $ fmap sum' $ A.mapConcurrently pure [1..size]
        , bench "mapConcurrently"   $ whnfIO $ fmap sum' $ SUT.mapConcurrently pure [1..size]
        , bench "Conc" $ whnfIO $ SUT.runConc $
            let go i
                  | i == size = SUT.conc (pure i)
                  | otherwise = liftA2 (+) (SUT.conc (pure i)) (go (i + 1))
             in go 1
        -- This is cheating, since it's using our Pure data constructor
        , bench "Conc, cheating" $ whnfIO $ SUT.runConc $
            let go i
                  | i == size = pure i
                  | otherwise = liftA2 (+) (pure i) (go (i + 1))
             in go 1
        ]

    , bgroup "race" $ sizes $ \size ->
      [ bench "A.Concurrently" $ whnfIO $
          A.runConcurrently $
          foldr (<|>) empty (replicate size (pure ()))
      , bench "Concurrently" $ whnfIO $
          SUT.runConcurrently $
          foldr (<|>) empty (replicate size (pure ()))
      , bench "Conc" $ whnfIO $
          SUT.runConc $
          foldr (<|>) empty (replicate size (SUT.conc (pure ())))
      -- This is cheating, since it's using our Pure data constructor
      , bench "Conc, cheating" $ whnfIO $
          SUT.runConc $
          foldr (<|>) empty (replicate size (pure ()))
      ]

    , bgroup "alternative (with result)" $
        sizes $ \size ->
          [ bench "Concurrently" $
            whnfIO $
            SUT.runConcurrently $
            let go i
                  | i == size = SUT.Concurrently (pure i)
                  | otherwise = liftA2 (+) (SUT.Concurrently (pure i)) (go (i + 1))
             in (SUT.Concurrently $ threadDelay maxBound >> return 0) <|> (go 1) <|>
                (SUT.Concurrently $ threadDelay maxBound >> return 0)
          , bench "Conc" $
            whnfIO $
            SUT.runConc $
            let go i
                  | i == size = SUT.conc (pure i)
                  | otherwise = liftA2 (+) (SUT.conc (pure i)) (go (i + 1))
             in (SUT.conc $ threadDelay maxBound >> return 0) <|> (go 1) <|>
                (SUT.conc $ threadDelay maxBound >> return 0)
          , bench "Conc, cheating" $
            whnfIO $
            SUT.runConc $
            let go i
                  | i == size = SUT.conc (pure i)
                  | otherwise = liftA2 (+) (pure i) (go (i + 1))
             in (SUT.conc $ threadDelay maxBound >> return 0) <|> (go 1) <|>
                (SUT.conc $ threadDelay maxBound >> return 0)
          ]
      , let size = 10
         in bgroup
              "alternative (nested)"
              [ bench "Concurrently" $
                whnfIO $
                SUT.runConcurrently $
                let go i
                      | i == size = SUT.Concurrently (pure i)
                      | i `mod` 2 == 0 =
                        (liftA2 (+) (SUT.Concurrently (pure i)) (go (i + 1))) <|>
                        (liftA2 (+) (SUT.Concurrently (pure i)) (go (i + 2)))
                      | otherwise =
                        liftA2 (+) (SUT.Concurrently (pure i)) (go (i + 1))
                 in go 1
              , bench "Conc" $
                whnfIO $
                SUT.runConc $
                let go i
                      | i == size = SUT.conc (pure i)
                      | i `mod` 2 == 0 =
                        (liftA2 (+) (SUT.conc (pure i)) (go (i + 1))) <|>
                        (liftA2 (+) (SUT.conc (pure i)) (go (i + 2)))
                      | otherwise = liftA2 (+) (SUT.conc (pure i)) (go (i + 1))
                 in go 1
              , bench "Conc, cheating" $
                whnfIO $
                SUT.runConc $
                let go i
                      | i == size = SUT.conc (pure i)
                      | i `mod` 2 == 0 =
                        (liftA2 (+) (pure i) (go (i + 1))) <|>
                        (liftA2 (+) (pure i) (go (i + 2)))
                      | otherwise = liftA2 (+) (pure i) (go (i + 1))
                 in go 1
              ]
    ]
