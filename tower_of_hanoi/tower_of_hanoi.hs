import System.Environment (getArgs)

move 1 s d t = putStrLn $ show s ++ " --> " ++ show d
move n s d t = do
    move (n - 1) s t d
    move 1 s d t
    move (n - 1) t d s

main = do
    [sn] <- getArgs
    n <- readIO sn
    move n 'A' 'C' 'B'
