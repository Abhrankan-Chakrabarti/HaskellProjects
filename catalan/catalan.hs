import System.Environment (getArgs)

cat 0 = 1
cat n = cat (n - 1) * (4 * n - 2) `div` (n + 1)

main = do
    [sn] <- getArgs
    n <- readIO sn
    putStrLn $ "Cat(" ++ show n ++ ")\t= " ++ show (cat n)
