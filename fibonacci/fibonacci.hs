import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Numeric (readHex)
import Control.Exception (evaluate)

parseOption :: [String] -> String -> (Bool, [String])
parseOption args option = (option `elem` args, filter (/= option) args)

-- Prompt function for interactive input
prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

-- Parse decimal, scientific, or hex input
parseNumber :: String -> Maybe Integer
parseNumber s
    | take 2 s == "0x" || take 2 s == "0X" = case readHex (drop 2 s) of
        [(n, "")] -> Just n
        _         -> Nothing
    | 'e' `elem` s || 'E' `elem` s = case reads s :: [(Double, String)] of
        [(d, "")] -> Just (round d)
        _         -> Nothing
    | otherwise = case reads s :: [(Integer, String)] of
        [(n, "")] -> Just n
        _         -> Nothing

-- Perform Fibonacci
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n
    | n < 0     = let f = fib (-n)
                      k = if odd (-n) then 1 else -1
		  in f * k
    | even n    = let k = div n 2
                      a = fib k
                      b = fib (k - 1)
	          in a * (2 * b + a)
    | otherwise = let k = div (n - 1) 2
                      a = fib k
                      b = fib (k - 1)
	          in (2 * a + b) * (2 * a - b) + 2 * (-1) ^ k

-- Main program with CLI + interactive fallback
main :: IO ()
main = do
  args <- getArgs
  let (shouldPrint, filteredArgs) = parseOption args "--print"
  n <- case filteredArgs of
         [sn] -> return sn
         _ -> prompt "Enter n:\t"
  case parseNumber n of
    Just val -> do
      start <- getCPUTime
      let fibval = fib val
      _ <- evaluate fibval
      end <- getCPUTime
      let elapsed = fromIntegral (end - start) / (10 ^ 12) :: Double
      if shouldPrint then putStrLn $ "F(" ++ show val ++ ") = " ++ show fibval else return ()
      printf "Elapsed time:\t%.6f sec\n" elapsed
    Nothing -> putStrLn "Invalid number format. Please enter a valid integer, scientific, or hex value."
