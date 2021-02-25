module TwoSum (twoSum) where
import Data.Maybe
import Data.List

twoSum :: [Int] -> Int -> (Int,Int)
twoSum xs targ = head $ [(snd x,snd y) | x <- ts, y <- ts, (snd x) /= (snd y), (fst x) + (fst y) == targ]
    where ts = zipWith (,) xs [1..]