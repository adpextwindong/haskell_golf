import Debug.Trace

fib :: Integer -> Integer
fib n | trace ("fib " ++ show n) False = undefined
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
