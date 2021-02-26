module Codewars.Kata.Decompose where
import Data.Ratio
import Control.Applicative
import Data.Maybe
import Debug.Trace

decompose :: String -> String -> [String]
decompose n d = showRatio <$> decomp (read n :: Integer) (read d :: Integer) Nothing

showRatio :: Ratio Integer -> String
showRatio r = show (numerator r) ++ rest
      where rest = if denominator r == 1
                   then ""
                   else "/" ++ show (denominator r)

candidates :: Ratio Integer  -> Integer -> Integer -> [Ratio Integer]
candidates x start end = dropWhile (> x) $ liftA2 (%) [1] [start..end]

decomp :: Integer -> Integer -> Maybe Integer -> [Ratio Integer]
decomp _ 0 _ = []
decomp 0 _ _ = []
decomp 1 d _ = [1%d]
decomp n d prev
      | n > d = let mul = (n `div` d) in mul % 1 : decomp (n - (mul * d)) d prev
      | null nextChoices = []
      | head nextChoices == x = [x]
      | otherwise =  trace (show "~~TRACE" ++ show n ++ " " ++ show d ++ "\n" ++ show nextChoices ++ "\n" ++ show trails) (head trails)
    where x = n % d
          start = fromMaybe 2 prev
          nextChoices = candidates x start d
          remainders = (x -) <$> nextChoices
          trails = (\c -> let rem = x - c in c : decomp (numerator rem) (denominator rem) (fmap (+1) prev)) <$> nextChoices