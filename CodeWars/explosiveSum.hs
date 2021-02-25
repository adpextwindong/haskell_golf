module ExplosiveSum where

explosiveSum :: Integer -> Integer
explosiveSum 1 = 1
explosiveSum n = sum $ map (\p -> (explosiveSum (fst p) + (explosiveSum (snd p)))) pairs
    where pairs = distPEQ n
          mid = n `div` 2
          midR = mid + (n `mod` 2)


part :: Integral b => b -> Int
part 1 = 1
part n = 2 + ((sum . map (length . distPEQ)) [2..(n-1)])

distPEQ :: Integral b => b -> [(b, b)]
distPEQ n = [(x,y) | x <- [1..mid], y<-[mid..n], x+y == n]
    where mid = n `div` 2
          