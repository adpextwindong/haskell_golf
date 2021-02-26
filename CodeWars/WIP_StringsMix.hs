-- https://www.codewars.com/kata/5629db57620258aa9d000014/train/haskell

module Codewars.G964.Mixin where

import Data.Char (isLower)
import qualified Data.Map
import Control.Applicative ( Applicative(liftA2) )

charCountMap :: String -> Data.Map.Map Char Integer
charCountMap s = Data.Map.fromListWith (+) $ liftA2 (,) s [1]

mix :: [Char] -> [Char] -> [Char]
mix s1 s2 = undefined
    where countMapLeft = charCountMap $ filter isLower s1
          countMapRight = charCountMap $ filter isLower s2
          mergeMap = undefined --TODO handle merging then printing

--https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-Map-Merge-Strict.html#v:merge
