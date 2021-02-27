-- https://www.codewars.com/kata/5629db57620258aa9d000014/train/haskell

-- module Codewars.G964.Mixin where

import Data.Char (isLower)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge
import Control.Applicative

data Smix k v = LeftSmix k v | RightSmix k v | EqSmix k v

charCountMap :: String -> Data.Map.Map Char Integer
charCountMap s = Map.fromListWith (+) $ liftA2 (,) s [1]

dealWithLeft :: k -> x -> Maybe Y
dealWithLeft k x
    | x == 1 = Nothing
    | otherwise = Just (LeftSmix k x)

dealWithRight :: k -> x -> Maybe Y
dealWithRight k x
    | x == 1 = Nothing
    | otherwise = Just (RightSmix k x)

dealWithBoth :: k -> x -> y -> Maybe Z
dealWithBoth k 1 1 = Nothing
dealWithBoth k x 1 = Just (LeftSmix k x)
dealWithBoth k 1 y = Just (RightSmix k y)
dealWithBoth k x y
    | x == y = Just (EqSmix k x)
    | x > y = Just (LeftSmix k x)
    | x < y = Just (RightSmix k y)

--TODO test this, maybe we can even filter 1's before even merging
-- merge (mapMaybeMissing dealWithLeft)
--            (mapMaybeMissing dealWithRight)
--            (zipWithMaybeMatched dealWithBoth)
--            countMapLeft countMapRight


mix :: [Char] -> [Char] -> [Char]
mix s1 s2 = undefined
    where countMapLeft = charCountMap $ filter isLower s1
          countMapRight = charCountMap $ filter isLower s2
          mergeMap = undefined --TODO handle merging then printing

--https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-Map-Merge-Strict.html#v:merge
