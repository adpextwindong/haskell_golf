-- https://www.codewars.com/kata/5629db57620258aa9d000014/train/haskell

-- module Codewars.G964.Mixin where

import Data.List (sortBy, intercalate)
import Data.Char (isLower)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge
import Control.Applicative

data Smix = LeftSmix | RightSmix | EqSmix
    deriving (Eq, Ord)

showSmix :: (Smix, (Char,Int)) -> String
showSmix (LeftSmix, (k,v)) = "1:" ++ replicate v k
showSmix (RightSmix, (k,v)) = "2:" ++ replicate v k
showSmix (EqSmix, (k,v)) = "=:" ++ replicate v k

charCountMap :: String -> Map.Map Char Int
charCountMap s = Map.fromListWith (+) $ liftA2 (,) s [1]

--Merge handling and dropping 1s
dealWith :: Bool -> Char -> Int -> (Smix, (Char, Int))
dealWith invert k x
    | invert = (RightSmix, (k,x))
    | otherwise = (LeftSmix, (k,x))

dealWithBoth :: Char -> Int -> Int -> (Smix, (Char, Int))
dealWithBoth k x 1 = (LeftSmix, (k, x))
dealWithBoth k 1 y = (RightSmix, (k, y))
dealWithBoth k x y
    | x == y = (EqSmix, (k, x))
    | x > y = (LeftSmix, (k, x))
    | x < y = (RightSmix, (k, y))

generalOrdering :: (Smix, (Char, Int)) -> (Smix, (Char, Int)) -> Ordering
generalOrdering l@(ls, (k1, v1)) r@(rs, (k2,v2))
    | v1 == v2 = case compare ls rs of
                    EQ -> compare k1 k2
                    _ -> compare ls rs
    | otherwise = compare v2 v1

mix :: String -> String -> String
mix s1 s2 = intercalate "/" (showSmix <$> sortedBySmix)
    where countMapLeft = charCountMap $ filter isLower s1
          countMapRight = charCountMap $ filter isLower s2
          mergeMap = Merge.merge (Merge.mapMissing (dealWith False))
                                 (Merge.mapMissing (dealWith True))
                                 (Merge.zipWithMatched dealWithBoth)
                                 countMapLeft countMapRight
          result = snd <$> Map.toList mergeMap
          sortedBySmix = sortBy generalOrdering $ filter ((> 1) . snd . snd) result