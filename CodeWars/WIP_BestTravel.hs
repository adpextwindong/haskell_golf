-- https://www.codewars.com/kata/55e7280b40e1c4a06d0000aa/train/haskell

import Data.List ( subsequences )

combos :: Int -> [a] -> [[a]]
combos n xs = filter ((== n) . length ) $ subsequences xs

chooseBestSum :: Int -> Int -> [Int] -> Maybe Int
chooseBestSum cost n xs = if best == 0
                          then Nothing 
                          else Just best
    where best = foldl max 0 $ filter (<= cost) (sum <$> combos n xs)