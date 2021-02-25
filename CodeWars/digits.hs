module DescendingOrder where
import Data.List

digits :: Integer -> [Integer]
digits n
  | n < 10 = [n]
  | otherwise = digits remainding ++ [digit]
  where
  remainding = n `div` 10
  digit = mod n 10

undigits :: [Integer] -> Integer
undigits [] = 0
undigits (x:xs) = fromInteger $ (x * toInteger place) + undigits xs
 where base = length xs
       place = 10 ^ base

descendingOrder :: Integer -> Integer
descendingOrder = undigits . reverse . sort . digits