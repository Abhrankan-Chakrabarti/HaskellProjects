import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Numeric (readHex)
import Control.Exception (evaluate)

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

fib_luc :: Integer -> (Integer, Integer)
fib_luc 0 = (0, 2)
fib_luc n
    | n < 0     = let (f, l) = fib_luc (-n)
                      k = if odd (-n) then 1 else -1
                  in (f * k, l * k)
    | odd n     = let (f, l) = fib_luc (n - 1)
                  in ((f + l) `div` 2, (5 * f + l) `div` 2)
    | otherwise = let (f, l) = fib_luc (n `div` 2)
                      m = n `mod` 4
                      t = m - m `mod` 2
                  in (f * l, l ^ 2 + t * 2 - 2)

main :: IO ()
main = do
    args <- getArgs
    let printFib = "--fib" `elem` args
        printLuc = "--lucas" `elem` args
        printAll = "--print" `elem` args
        shouldPrintFib = printAll || printFib
        shouldPrintLuc = printAll || printLuc

    putStrLn "This program computes the Fibonacci and Lucas numbers of a given integer."
    putStr "Enter a number: "
    hFlush stdout
    input <- getLine

    case parseNumber input of
        Just n -> do
            start <- getCPUTime
            let (fib, luc) = fib_luc n
            _ <- evaluate fib
            _ <- evaluate luc
            end <- getCPUTime
            let elapsed = fromIntegral (end - start) / (10 ^ 12) :: Double

            if shouldPrintFib then putStrLn $ "F(" ++ show n ++ ")\t= " ++ show fib else return ()
            if shouldPrintLuc then putStrLn $ "L(" ++ show n ++ ")\t= " ++ show luc else return ()
            printf "Elapsed time:\t%.6f sec\n" elapsed

        Nothing -> putStrLn "Invalid number format. Please enter a valid integer, scientific, or hex value."
