import Gauge
import Gauge.Main
import Control.Concurrent (threadDelay)
import UnliftIO
import qualified Control.Concurrent.Async as A
import Data.List (foldl')
import Control.Applicative (liftA2, (<|>), empty)

sizes :: (Int -> [Benchmark]) -> [Benchmark]
sizes f = map
  (\size -> bgroup (show size) (f size))
  [1, 2, 10, 100, 1000, 10000, 100000]

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
main = defaultMain
  [ bgroup "concurrently, minimal work" $ sizes $ \size ->
    [ bench "A.replicateConcurrently_" $ whnfIO $ do
        ref <- newIORef (0 :: Int)
        A.replicateConcurrently_ size $ atomicModifyIORef' ref $ \i -> (i + 1, ())
    , bench "replicateConcurrently_" $ whnfIO $ do
        ref <- newIORef (0 :: Int)
        replicateConcurrently_ size $ atomicModifyIORef' ref $ \i -> (i + 1, ())
    , bench "Conc" $ whnfIO $ do
        ref <- newIORef (0 :: Int)
        runConc $ replicateA_ size $ conc $ atomicModifyIORef' ref $ \i -> (i + 1, ())
    ]
  , bgroup "concurrently, no results" $ sizes $ \size ->
    [ bench "A.replicateConcurrently_" $ whnfIO $ A.replicateConcurrently_ size (pure ())
    , bench "replicateConcurrently_" $ whnfIO $ replicateConcurrently_ size (pure ())
    , bench "Conc" $ whnfIO $ runConc $ replicateA_ size $ conc $ pure ()
    , bench "Conc, cheating" $ whnfIO $ runConc $ replicateA_ size $ pure ()
    ]
  , bgroup "concurrently, with results" $ sizes $ \size ->
      [ bench "A.mapConcurrently" $ whnfIO $ fmap sum' $ A.mapConcurrently pure [1..size]
      , bench "mapConcurrently" $ whnfIO $ fmap sum' $ mapConcurrently pure [1..size]
      , bench "Conc" $ whnfIO $ runConc $
          let go i
                | i == size = conc (pure i)
                | otherwise = liftA2 (+) (conc (pure i)) (go (i + 1))
           in go 1
      -- This is cheating, since it's using our Pure data constructor
      , bench "Conc, cheating" $ whnfIO $ runConc $
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
        runConcurrently $
        foldr (<|>) empty (replicate size (pure ()))
    , bench "Conc" $ whnfIO $
        runConc $
        foldr (<|>) empty (replicate size (conc (pure ())))
    -- This is cheating, since it's using our Pure data constructor
    , bench "Conc, cheating" $ whnfIO $
        runConc $
        foldr (<|>) empty (replicate size (pure ()))
    ]
  , bgroup "race (with result)" $
      sizes $ \size ->
        [ bench "Concurrently" $
          whnfIO $
          runConcurrently $
          let go i
                | i == size = Concurrently (pure i)
                | otherwise = liftA2 (+) (Concurrently (pure i)) (go (i + 1))
           in (Concurrently $ threadDelay maxBound >> return 0) <|> (go 1) <|>
              (Concurrently $ threadDelay maxBound >> return 0)
        , bench "Conc" $
          whnfIO $
          runConc $
          let go i
                | i == size = conc (pure i)
                | otherwise = liftA2 (+) (conc (pure i)) (go (i + 1))
           in (conc $ threadDelay maxBound >> return 0) <|> (go 1) <|>
              (conc $ threadDelay maxBound >> return 0)
        , bench "Conc, cheating" $
          whnfIO $
          runConc $
          let go i
                | i == size = conc (pure i)
                | otherwise = liftA2 (+) (pure i) (go (i + 1))
           in (conc $ threadDelay maxBound >> return 0) <|> (go 1) <|>
              (conc $ threadDelay maxBound >> return 0)
        ]
    , let size = 10
       in bgroup
            "race (nested)"
            [ bench "Concurrently" $
              whnfIO $
              runConcurrently $
              let go i
                    | i == size = Concurrently (pure i)
                    | i `mod` 2 == 0 =
                      (liftA2 (+) (Concurrently (pure i)) (go (i + 1))) <|>
                      (liftA2 (+) (Concurrently (pure i)) (go (i + 2)))
                    | otherwise =
                      liftA2 (+) (Concurrently (pure i)) (go (i + 1))
               in go 1
            , bench "Conc" $
              whnfIO $
              runConc $
              let go i
                    | i == size = conc (pure i)
                    | i `mod` 2 == 0 =
                      (liftA2 (+) (conc (pure i)) (go (i + 1))) <|>
                      (liftA2 (+) (conc (pure i)) (go (i + 2)))
                    | otherwise = liftA2 (+) (conc (pure i)) (go (i + 1))
               in go 1
            , bench "Conc, cheating" $
              whnfIO $
              runConc $
              let go i
                    | i == size = conc (pure i)
                    | i `mod` 2 == 0 =
                      (liftA2 (+) (pure i) (go (i + 1))) <|>
                      (liftA2 (+) (pure i) (go (i + 2)))
                    | otherwise = liftA2 (+) (pure i) (go (i + 1))
               in go 1
            ]
  ]
