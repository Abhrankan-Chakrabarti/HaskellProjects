{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where

import Foreign.Ptr (Ptr(..))
import Numeric.GMP.Types (MPZ)
import Numeric.GMP.Utils (withInInteger, withOutInteger_)
import Numeric.GMP.Raw.Safe (mpz_sqrt)
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)
import System.IO

sqrtIO :: Integer -> IO Integer
sqrtIO n = do
  withOutInteger_ $ \rop ->
    withInInteger n $ \op ->
      mpz_sqrt rop op

isqrt :: Integer -> Integer
isqrt n = unsafePerformIO $ sqrtIO n

golden_ratio :: Integer -> (Integer, Integer)
golden_ratio digits = divMod (div (isqrt (5 * one ^ 2) + one) 2) one
  where
    one = 10 ^ digits

put_str0 n width one = do
    putStr "0"
    strzfill n (width * 10) one

strzfill n width one = if n * width < one then put_str0 n width one else putStrLn $ show n

print_strzfill n one = if n > 0 then strzfill n 10 one else putStrLn $ show n

main :: IO ()
main = do
  [sdigits] <- getArgs
  digits <- readIO sdigits
  let (quotient, remainder) = golden_ratio digits
  hSetEncoding stdout utf8
  putStr $ "ϕ to " ++ show digits ++ " digits is: " ++
             show quotient ++ "."
  print_strzfill remainder (10 ^ digits)
