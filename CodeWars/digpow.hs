module Codewars.Kata.DigPow where

getDigits :: Integer -> [Integer]
getDigits = map (\c -> read [c] :: Integer) . show

lhs :: (Integral b, Num c) => b -> b -> [c] -> c
lhs power base digits = sum . map (\p -> (fst p)^(snd p)) $ zipWith (,) digits [power+base, power+base+1..]

digpow :: Integer -> Integer -> Integer
digpow n p = case candidate `mod` n of
              0 -> candidate `div` n
              otherwise -> -1
  where digits = getDigits n
        lhsLE = takeWhile (<= n) [lhs p x digits | x <- [0..]]
        candidate = head . reverse $ lhsLE
        
        