import System.Environment (getArgs)
a 0 n = n+1
a m 0 = a (m-1) 1
a m n = a (m-1) (a m (n-1))
main = do
  [sm, sn] <- getArgs
  m <- readIO sm
  n <- readIO sn
  putStrLn $ "A(" ++ show m ++ "," ++ show n ++ ") = " ++ show (a m n)
