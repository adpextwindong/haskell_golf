module Codewars.SumOddNumbers where

rowSumOddNumbers :: Integer -> [Integer]
rowSumOddNumbers i = row
  where
    ind = fromIntegral i :: Int
    odds = filter even [1..]
    dropCount = sum $ take (ind - 1) [1..]
    row = take ind $ drop dropCount odds
