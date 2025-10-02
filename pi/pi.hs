{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where

import Foreign.Ptr (Ptr(..))
import Numeric.GMP.Types (MPZ)
import Numeric.GMP.Utils (withInInteger, withOutInteger_)
import Numeric.GMP.Raw.Safe (mpz_sqrt)
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

sqrtIO :: Integer -> IO Integer
sqrtIO n = do
  withOutInteger_ $ \rop ->
    withInInteger n $ \op ->
      mpz_sqrt rop op

isqrt :: Integer -> Integer
isqrt n = unsafePerformIO $ sqrtIO n

bs :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
bs a b c3_over_24 = if b - a == 1
                    then bs1 a b c3_over_24
                    else bs2 a b c3_over_24

bs1 :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
bs1 a b c3_over_24 = if a == 0
                     then bs11 a b c3_over_24
                     else bs12 a b c3_over_24

bs11 :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
bs11 a b c3_over_24 = (pab, qab, tab)
  where
    pab = 1
    qab = 1
    tab = pab * t a

t :: Integer -> Integer
t a = (13591409 + 545140134*a) * (-2 * (mod a 2) + 1)

bs12 :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
bs12 a b c3_over_24 = (pab, qab, tab)
  where
    pab = (6*a-5)*(2*a-1)*(6*a-1)
    qab = a ^ 3 * c3_over_24
    tab = pab * t a

bs2 :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
bs2 a b c3_over_24 = (pab, qab, tab)
  where
    m = div (a + b) 2
    (pam, qam, tam) = bs a m c3_over_24
    (pmb, qmb, tmb) = bs m b c3_over_24
    pab = pam * pmb
    qab = qam * qmb
    tab = qmb * tam + pam * tmb

pi_chud :: Integer -> (Integer, Integer)
pi_chud digits = divMod (div (q * 426880 * isqrt (10005 * one ^ 2)) t) one
  where
    one = 10 ^ digits
    c = 640320
    c3_over_24 = div (c ^ 3) 24
    dpt = 14.1816474627254776555
    dd :: Double
    dd = fromIntegral digits
    n = ceiling (dd / dpt) + 1
    (p, q, t) = bs 0 n c3_over_24

put_str0 n width one = do
    putStr "0"
    strzfill n (width * 10) one

strzfill n width one = if n * width < one then put_str0 n width one else putStrLn $ show n

print_strzfill n one = if n > 0 then strzfill n 10 one else putStrLn $ show n

main :: IO ()
main = do
  [sdigits] <- getArgs
  digits <- readIO sdigits
  let (quotient, remainder) = pi_chud digits
  putStr $ "π to " ++ show digits ++ " digits is: " ++
             show quotient ++ "."
  print_strzfill remainder (10 ^ digits)
