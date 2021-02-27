-- https://www.codewars.com/kata/5629db57620258aa9d000014/train/haskell

-- module Codewars.G964.Mixin where

import Data.Maybe (catMaybes)
import Data.List (sortBy, intercalate)
import Data.Char (isLower)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge
import Control.Applicative

data Smix = LeftSmix | RightSmix | EqSmix

showSmix :: (Smix, (Char,Int)) -> String
showSmix (LeftSmix, (k,v)) = "1:" ++ (replicate v k)
showSmix (RightSmix, (k,v)) = "2:" ++ (replicate v k)
showSmix (EqSmix, (k,v)) = "=:" ++ (replicate v k)

charCountMap :: String -> Map.Map Char Int
charCountMap s = Map.fromListWith (+) $ liftA2 (,) s [1]

--Merge handling and dropping 1s
dealWith :: Bool -> Char -> Int -> Maybe (Smix, (Char, Int))
dealWith invert k x
    | x == 1 = Nothing
    | invert = Just (RightSmix, (k,x))
    | otherwise = Just (LeftSmix, (k,x))

dealWithBoth :: Char -> Int -> Int -> Maybe (Smix, (Char, Int))
dealWithBoth k 1 1 = Nothing
dealWithBoth k x 1 = Just (LeftSmix, (k, x))
dealWithBoth k 1 y = Just (RightSmix, (k, y))
dealWithBoth k x y
    | x == y = Just (EqSmix, (k, x))
    | x > y = Just (LeftSmix, (k, x))
    | x < y = Just (RightSmix, (k, y))

smixOrdering :: (Smix, (Char, Int)) -> (Smix, (Char, Int)) -> Ordering
--TODO we should just partition by value, partition by smix, then sort by char
smixOrdering (_,(k1, v1)) (_,(k2, v2))
    | v1 < v2 = GT
    | v1 > v2 = LT
    | k1 < k2 = LT
    | k1 > k2 = GT
    | otherwise = EQ

mix :: String -> String -> String
mix s1 s2 = intercalate "/" $ showSmix  <$> result
    where countMapLeft = charCountMap $ filter isLower s1
          countMapRight = charCountMap $ filter isLower s2
          mergeMap = Merge.merge (Merge.mapMaybeMissing (dealWith False))
                                 (Merge.mapMaybeMissing (dealWith True))
                                 (Merge.zipWithMaybeMatched dealWithBoth)
                                 countMapLeft countMapRight
          result = sortBy smixOrdering $ fmap snd $ Map.toList mergeMap
