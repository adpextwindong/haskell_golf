gnuff_criterea = 0.00000001
goodenough guess delta = (delta / guess) < gnuff_criterea

absdelta x y = abs (x - y)

square x = x * x
average x y = (x + y) / 2

improve2 g x = average g (x / g)
improve3 g x = ((x / (square g)) + (2 * g)) / 3

rootniter impf guess x = if goodenough guess delta
                          then guess
                          else rootniter impf guess x
                          where
                             improved = impf guess x
                             delta = absdelta guess improved

rootn :: Double -> Int -> Double
rootn x n
    | n < 2 || n > 3 = undefined
    | otherwise =
        rootniter impf 1 x
        where
            impf = case n of
                        2 -> improve2
                        3 -> improve3
