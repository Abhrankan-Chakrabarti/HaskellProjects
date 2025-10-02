import System.Environment (getArgs)

place k i x = loop 1 k i x

check j k i x = if x!!j == i || abs (x!!j - i) == abs (j - k)
                then False
                else loop (j + 1) k i x

loop j k i x = if j <= k - 1
               then check j k i x
               else True

nqueens k n x c l = mainloop 1 n k x c l

solve i n k x c l = if place k i x
                  then place1 i n k x c l
                  else mainloop (i + 1) n k x c l

mainloop i n k x c l = if i <= n
                     then solve i n k x c l
                     else decide i n k x c l

decide i n k x c [] = putStr ""
decide i n k x c l = mainloop (p + 1) n q x c b
  where
    (a, b) = splitAt 1 l
    (p, q) = a!!0

place1 i n k x c l = try i n k w c l
  where
    (a, b) = splitAt k x
    (y, z) = splitAt 1 b
    w = a ++ [i] ++ z

try i n k x c l = if k == n
                then display i n k x c l
                else nqueens (k + 1) n x c ([(i, k)] ++ l)

display i n k x c l = do
    putStrLn $ "Solution " ++ show c ++ ": " ++ show z
    mainloop (i + 1) n k x (c + 1) l
    where
      (y, z) = splitAt 1 x

f x = 0

main = do
    [sn] <- getArgs
    n <- readIO sn
    nqueens 1 n (map f [0..n]) 1 []
