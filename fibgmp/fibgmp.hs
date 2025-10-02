{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where

import Foreign.C.Types (CULong)
import Foreign.Ptr (Ptr(..))
import Numeric (readHex)
import Numeric.GMP.Types (MPZ)
import Numeric.GMP.Utils (withOutInteger_)
import Numeric.GMP.Raw.Safe (mpz_fib_ui)
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hFlush, stdout)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Control.Exception (evaluate)

-- Convert Integer to CULong for FFI
convertIntegerToCULong :: Integer -> CULong
convertIntegerToCULong = fromIntegral

-- Perform Fibonacci using GMP in IO
fibIO :: CULong -> IO Integer
fibIO n = withOutInteger_ $ \rop ->
  mpz_fib_ui rop n

-- Pure wrapper
fib :: CULong -> Integer
fib n = unsafePerformIO $ fibIO n

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

-- Main program with CLI + interactive fallback
main :: IO ()
main = do
  args <- getArgs
  let (shouldPrint, filtered) = parseOption args "--print"
  let (verbose, filteredArgs) = parseOption filtered "--verbose"
  n <- case filteredArgs of
         [sn] -> return sn
         _ -> prompt "Enter n:\t"
  case parseNumber n of
    Just val -> do
      let longval = convertIntegerToCULong val
      if verbose then putStrLn $ "Computing F(" ++ show longval ++ ")..." else return ()
      start <- getCPUTime
      let fibval = fib longval
      _ <- evaluate fibval
      end <- getCPUTime
      let elapsed = fromIntegral (end - start) / (10 ^ 12) :: Double
      if shouldPrint then putStrLn $ "F(" ++ show longval ++ ") = " ++ show fibval else return ()
      endWrite <- getCPUTime
      let elapsedWrite = fromIntegral (endWrite - end) / (10 ^ 12) :: Double
      if shouldPrint && verbose then printf "Writing time:\t%.6f sec\n" elapsedWrite else return ()
      printf "Elapsed time:\t%.6f sec\n" elapsed
    Nothing -> putStrLn "Invalid number format. Please enter a valid integer, scientific, or hex value."
