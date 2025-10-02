import Text.Read (readMaybe)

-- Binary Splitting Function (as provided in your code)
bs :: Integer -> Integer -> Double -> (Integer, Integer)
bs a b x = if b - a == 1
           then (round (x ^ b), b)
           else bs2 a b x

bs2 :: Integer -> Integer -> Double -> (Integer, Integer)
bs2 a b x = (pam * qmb + pmb, qam * qmb)
  where
    m = div (a + b) 2
    (pam, qam) = bs a m x
    (pmb, qmb) = bs m b x

-- Calculate the smallest x such that the sum of logBase 10 exceeds y
invgammaof10tothepower :: Double -> Integer
invgammaof10tothepower y = invgammaof10pow y 1.0 0.0

invgammaof10pow :: Double -> Double -> Double -> Integer
invgammaof10pow y x s = if y > s
                        then invgammaof10pow y (x + 1.0) (s + logBase 10 (x + 1.0))
                        else ceiling x

-- Compute exp(x) with the specified number of digits of precision
exp_bs :: Double -> Integer -> (Integer, Integer)
exp_bs x digits = divMod (div (p * one) q + one) one
  where
    n = invgammaof10tothepower (fromIntegral digits) + 1
    one = 10 ^ digits
    (p, q) = bs 0 n x

put_str0 n width one = do
    putStr "0"
    strzfill n (width * 10) one

strzfill n width one = if n * width < one then put_str0 n width one else putStrLn $ show n

print_strzfill n one = if n > 0 then strzfill n 10 one else putStrLn $ show n

-- Main Function: Interactive Input
main :: IO ()
main = do
    putStrLn "Enter the value of x (exponent):"
    inputX <- getLine
    putStrLn "Enter the number of digits of precision:"
    inputDigits <- getLine
    case (readMaybe inputX :: Maybe Double, readMaybe inputDigits :: Maybe Integer) of
      (Just x, Just digits) -> do
          let (quotient, remainder) = exp_bs x digits
          putStr $ "e^" ++ show x ++ " to " ++ show digits ++ " digits is: " ++
                     show quotient ++ "."
          print_strzfill remainder (10 ^ digits)
      _ -> putStrLn "Invalid input. Please enter a valid number for x and an integer for digits."
