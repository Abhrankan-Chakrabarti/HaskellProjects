{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.IORef
import System.CPUTime (getCPUTime)
import Control.Exception (evaluate)
import System.Environment
import System.Random
import Control.Concurrent (threadDelay)

-- Pure bubble sort (for benchmarking)
pureBubble :: [Int] -> [Int]
pureBubble xs = case bubblePass xs of
                  (True, xs')  -> pureBubble xs'
                  (False, xs') -> xs'
  where
    bubblePass :: [Int] -> (Bool, [Int])
    bubblePass (x:y:zs)
      | x > y     = let (swapped, rest) = bubblePass (x:zs) in (True, y:rest)
      | otherwise = let (swapped, rest) = bubblePass (y:zs) in (swapped, x:rest)
    bubblePass xs = (False, xs)

-- Tachyonic (animated) bubble sort
tachyonicBubble :: Bool -> [Int] -> IO [Int]
tachyonicBubble wMode xs = do
    arr <- newIORef xs
    let n = length xs
    sorted <- newIORef False
    swaps <- newIORef 0

    whileM_ (fmap not (readIORef sorted)) $ do
        writeIORef sorted True
        bubblePass arr sorted swaps (n-1) n wMode

    readIORef arr
  where
    bubblePass arr sorted swaps 0 _ _ = return ()
    bubblePass arr sorted swaps i n wMode = do
        list <- readIORef arr
        when (i > 0) $ do
          let (before, rest) = splitAt (i-1) list
          case rest of
            (x:y:after) -> do
              if x > y
                then do
                  let newList = before ++ y:x:after
                  writeIORef arr newList
                  writeIORef sorted False
                  modifyIORef swaps (+1)
                  currSwaps <- readIORef swaps
                  displayList currSwaps wMode newList i n
                else do
                  currSwaps <- readIORef swaps
                  displayList currSwaps wMode list i n
              bubblePass arr sorted swaps (i-1) n wMode
            _ -> return ()

-- Display function with warp sounds and progress bar
displayList :: Int -> Bool -> [Int] -> Int -> Int -> IO ()
displayList currentSwaps wMode lst swapIndex n = do
  putStr "\ESC[2J"    -- Clear screen
  putStr "\ESC[H"     -- Move cursor to top left
  mapM_ (putStrLn . formatElement swapIndex) (zip [0..] lst)

  -- Progress bar
  let progress = fromIntegral (n - currentSwaps) / fromIntegral n
      barWidth = 30
      filled = round (progress * fromIntegral barWidth)
      empty = barWidth - filled
      bar = replicate filled '#' ++ replicate empty '-'
      percent = round (progress * 100) :: Int

  putStrLn $ "\nProgress: [" ++ bar ++ "] " ++ show percent ++ "%"

  -- Warp sounds every 7 swaps if in warp mode
  when (wMode && currentSwaps `mod` 7 == 0 && currentSwaps /= 0) warpSounds

  if wMode
    then threadDelay 8000
    else threadDelay 40000

-- Format elements for display
formatElement :: Int -> (Int, Int) -> String
formatElement idx (i, val)
  | i == idx || i == idx - 1 = "\ESC[32m" ++ show val ++ "\ESC[0m"
  | otherwise = show val

-- Warp sounds
warpSounds :: IO ()
warpSounds = do
  let sounds = ["ZOOOM!", "SWAPPP!", "BLAM!", "KAPOW!", "WHOOSH!", "FWOOOSH!", "THWACK!", "FIZZ!", "KRRSSSH!"]
  idx <- randomRIO (0, length sounds - 1)
  putStrLn $ " " ++ sounds !! idx

-- whileM_ helper (like Control.Monad.Loops)
whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ cond action = do
  c <- cond
  when c $ do
    _ <- action
    whileM_ cond action

-- Benchmark runner
tachyonicBenchmark :: String -> ([Int] -> [Int]) -> [Int] -> IO (Double, [Int])
tachyonicBenchmark name sorter xs = do
    putStrLn "Tachyonic benchmark initiated!"
    putStrLn $ "Starting benchmark: " ++ name
    start <- getCPUTime
    let result = sorter xs
    _ <- evaluate (last result)
    end <- getCPUTime
    let elapsed = fromIntegral (end - start) / (10 ^ 12) :: Double
    putStrLn $ "Finished in " ++ show elapsed ++ " seconds."
    return (elapsed, result)

-- Pretty print color messages
tachyonicPutColorLn :: String -> String -> IO ()
tachyonicPutColorLn color msg = do
  putStr color
  putStrLn msg
  putStr "\ESC[0m"

-- Helper to run benchmark
runBenchmark :: Int -> IO ()
runBenchmark n = do
    when (n <= 0) $ error "Limit must be positive."
    shuffled <- replicateM n (randomRIO (1, n))
    (bTime, bResult) <- tachyonicBenchmark "Nebula BubbleSort" pureBubble shuffled
    putStrLn $ "First 10 sorted elements: " ++ show (take 10 bResult)

-- Helper to run demo
runDemo :: Bool -> Int -> IO ()
runDemo warpMode n = do
    when (n <= 0) $ error "Limit must be positive."
    shuffled <- replicateM n (randomRIO (1, n))
    _ <- tachyonicBubble warpMode shuffled
    return ()

-- Upgraded Banner with ASCII Logo
tachyonicBanner :: IO ()
tachyonicBanner = do
  tachyonicPutColorLn "\ESC[36m"
    " _____          _                       _   \n\
    \|_   _|_ _  ___| |__  _   _  ___  _ __ (_) ___ \n\
    \  | |/ _` |/ __| '_ \\| | | |/ _ \\| '_ \\| |/ __|\n\
    \  | | (_| | (__| | | | |_| | (_) | | | | | (__ \n\
    \  |_|\\__,_|\\___|_| |_|\\__, |\\___/|_| |_|_|\\___|\n\
    \                      |___/                    "
  tachyonicPutColorLn "\ESC[36m" $ replicate 47 '='
  putStrLn ""  -- Extra space after the banner

-- Main
main :: IO ()
main = do
    args <- getArgs
    tachyonicBanner
    case args of
      ["--help"] -> printHelp
      ("--benchmark" : x : _) -> parseAndRun (runBenchmark) x
      ("--demo" : x : _) -> parseAndRun (runDemo False) x
      ("--warp" : x : _) -> parseAndRun (runDemo True) x
      _ -> do
          tachyonicPutColorLn "\ESC[31m" "Error: Invalid arguments!\n"
          printHelp
          error "Exiting due to invalid input."

-- Helper to parse number safely
parseAndRun :: (Int -> IO ()) -> String -> IO ()
parseAndRun action str =
    case reads str :: [(Int, String)] of
      [(n, "")] | n > 0 -> action n
      _ -> do
          tachyonicPutColorLn "\ESC[31m" "Error: Size must be a positive integer!\n"
          printHelp
          error "Exiting due to invalid size input."

-- Help message with logo
printHelp :: IO ()
printHelp = do
    putStrLn "Usage:"
    putStrLn "  ./tachyonic_benchmark --benchmark <size>"
    putStrLn "  ./tachyonic_benchmark --demo <size>"
    putStrLn "  ./tachyonic_benchmark --warp <size>"
    putStrLn "  ./tachyonic_benchmark --help"
    putStrLn "\nExample:"
    putStrLn "  ./tachyonic_benchmark --benchmark 500"
