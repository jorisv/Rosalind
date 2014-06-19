import System.Environment

fib :: Int -> Int -> Int
fib n k
    | n <= 2 = 1
    | otherwise = (fib (n-2) k)*k + (fib (n-1) k)

main = do
    args <- getArgs
    let [n, k] = (map read args) :: [Int]
    print $ fib n k
