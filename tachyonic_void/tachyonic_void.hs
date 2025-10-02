import System.CPUTime
import Control.Monad
import Text.Printf
import Control.Exception (evaluate)
import System.IO
import Control.Concurrent (threadDelay)
import Data.List (sort)
import System.Environment (getArgs)

-- Colored Output Helpers
tachyonicPutColor :: String -> String -> IO ()
tachyonicPutColor color text = putStr $ color ++ text ++ "\ESC[0m"

tachyonicPutColorLn :: String -> String -> IO ()
tachyonicPutColorLn color text = tachyonicPutColor color (text ++ "\n")

-- Tachyonic Quicksort
tachyonicQsort :: Ord a => [a] -> [a]
tachyonicQsort []     = []
tachyonicQsort (x:xs) = tachyonicQsort smaller ++ [x] ++ tachyonicQsort larger
  where
    smaller = filter (<=x) xs
    larger  = filter (>x) xs

-- Tachyonic Insertion Sort
tachyonicIsort :: Ord a => [a] -> [a]
tachyonicIsort []     = []
tachyonicIsort (x:xs) = insertTachyonic x (tachyonicIsort xs)

insertTachyonic :: Ord a => a -> [a] -> [a]
insertTachyonic x []     = [x]
insertTachyonic x (y:ys) = if x <= y then x:y:ys else y : insertTachyonic x ys

-- Tachyonic Randomizer
tachyonicRandomise :: [a] -> [a]
tachyonicRandomise xs = shuffleTachyonic 7 xs

shuffleTachyonic :: Int -> [a] -> [a]
shuffleTachyonic 0 xs = xs
shuffleTachyonic n xs = shuffleTachyonic (n-1) (riffleTachyonic xs)

riffleTachyonic :: [a] -> [a]
riffleTachyonic [] = []
riffleTachyonic xs = interleaveTachyonic ys zs
  where
    (ys, zs) = splitAt (length xs `div` 2) xs

interleaveTachyonic :: [a] -> [a] -> [a]
interleaveTachyonic [] ys = ys
interleaveTachyonic xs [] = xs
interleaveTachyonic (x:xs) ys = x : interleaveTachyonic ys xs

-- Display Bar with Animation
tachyonicDisplayBar :: String -> Double -> Double -> Int -> String -> IO ()
tachyonicDisplayBar label time maxTime barMax color = do
  printf "%-8s | " label
  hFlush stdout
  tachyonicAnimateEvent (round (time / maxTime * fromIntegral barMax)) barMax color
  printf " (%.6f sec)\n" time

tachyonicAnimateEvent :: Int -> Int -> String -> IO ()
tachyonicAnimateEvent total barMax color = animateFrame 0
  where
    frames = ["⚡", "⚡", "🌀", "✨", "⚡"]
    animateFrame current
      | current > total = return ()
      | otherwise = do
          let pct = (fromIntegral current :: Double) / (fromIntegral barMax :: Double)
              frame = frames !! (current `mod` length frames)
          putStr "\r\t"
          tachyonicPutColor color (replicate current '▓')
          tachyonicPutColor color frame
          tachyonicPutColor color (replicate (barMax - current) '·')
          printf " %5.1f%%" (pct * 100)
          hFlush stdout
          threadDelay (5000 + round (pct * 3000))
          animateFrame (current + 1)

-- Victory Showdown
tachyonicBattle :: Double -> Double -> IO ()
tachyonicBattle t1 t2
  | t1 < t2 = tachyonicVictory "Tachyonic QSort" t1 "\ESC[32m"
  | t2 < t1 = tachyonicVictory "Tachyonic ISort" t2 "\ESC[34m"
  | otherwise = tachyonicStandoff

tachyonicVictory :: String -> Double -> String -> IO ()
tachyonicVictory who time color = do
  putStrLn ""
  tachyonicPutColorLn color $ ">>> " ++ who ++ " annihilates spacetime! (" ++ show time ++ "s) <<<"
  putStr "\BEL"  -- Terminal bell!

tachyonicStandoff :: IO ()
tachyonicStandoff = do
  putStrLn ""
  tachyonicPutColorLn "\ESC[35m" ">>> Tachyonic Singularity Standoff! The universe collapses! <<<"

-- Banner
tachyonicBanner :: IO ()
tachyonicBanner = do
  tachyonicPutColorLn "\ESC[36m" $ replicate 70 '='
  tachyonicPutColorLn "\ESC[33m" $ centerTextTachyonic "T A C H Y O N I C    V O I D    T U R B O C H A R G E D"
  tachyonicPutColorLn "\ESC[36m" $ replicate 70 '='

centerTextTachyonic :: String -> String
centerTextTachyonic s = let w = 70 in replicate ((w - length s) `div` 2) ' ' ++ s

-- Main Function
tachyonicMain :: IO ()
tachyonicMain = do
  args <- getArgs
  tachyonicBanner
  putStrLn "Tachyonic benchmark initiated!"
  
  n <- case args of
    (x:_) -> return (read x :: Int)
    _ -> do
      putStr "Enter upper limit (e.g., 5000): "
      hFlush stdout
      input <- getLine
      return (read input :: Int)
  
  let list = [1..n]
      shuffled = tachyonicRandomise list
  _ <- evaluate (shuffled !! (n - 1))

  putStrLn "\nTachyonic Quicksort engaging warp drive..."
  startQsort <- getCPUTime
  let qsortResult = tachyonicQsort shuffled
  _ <- evaluate (qsortResult !! (n - 1))
  endQsort <- getCPUTime
  let qsortTime = fromIntegral (endQsort - startQsort) / (10 ^ 12) :: Double

  putStrLn "Tachyonic InsertionSort destabilizing cosmic fabric..."
  startIsort <- getCPUTime
  let isortResult = tachyonicIsort shuffled
  _ <- evaluate (isortResult !! (n - 1))
  endIsort <- getCPUTime
  let isortTime = fromIntegral (endIsort - startIsort) / (10 ^ 12) :: Double

  let maxTime = max qsortTime isortTime

  putStrLn "\n=== Tachyonic Void Timing Results ===\n"
  tachyonicDisplayBar "qsort" qsortTime maxTime 50 "\ESC[32m"
  tachyonicDisplayBar "isort" isortTime maxTime 50 "\ESC[34m"

  tachyonicBattle qsortTime isortTime

main :: IO ()
main = tachyonicMain
