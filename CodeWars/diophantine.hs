module Codewars.Kata.Dioph where

import Control.Monad
import Data.List
import Data.Set

solequa :: Integer -> [(Integer, Integer)]
solequa n = toList $ fromList $ testMinus n ++ testPlus n ++ testComposite n

form x y = (x - 2*y) * (x + 2*y)

--This is too slow but works for the initial tests
validY n = [(x,y) | x <- [0..n], y <- [0.. (div x 2)], form x y == n ]

validY2 n = [(x, y) | x <- [0..n], y <- firstY n x]
firstY n x = go n x [0..(div x 2)]
    where go n x (y:ys) = if form x y == n
                          then [y]
                          else go n x ys
          go _ _ [] = []

squareP x = sqrt (fromIntegral x) == (fromIntegral . floor . sqrt $ fromIntegral x)

test :: Integer -> [(Integer, Integer)]
test n = do
    x <- [x | x <- [0..n], 0 == mod (x*x - n) 4]
    let ysquare = div ((x*x) - n) 4
    guard $ squareP ysquare
    return (x,floor (sqrt (fromIntegral ysquare)))

whole x = fromIntegral (floor x) == x

testMinus :: Integer -> [(Integer, Integer)]
testMinus n = do
    let xs = [x | x <- [0..n], 0 == mod (x*x - n) 4]
    yminus <- [(x,div (n - x) 2) | x <- xs, whole ((fromIntegral (n - x)) / 2.0), form x (div (n - x) 2) == n]
    return yminus

testPlus :: Integer -> [(Integer, Integer)]
testPlus n = do
    let xs = [x | x <- [0..n], 0 == mod (x*x - n) 4]
    yplus <- [(x,div (n + x) 2) | x <- xs, whole ((fromIntegral (n + x)) / 2.0), form x (div (n + x) 2) == n]
    return yplus

testComposite :: Integer -> [(Integer, Integer)] 
testComposite n = do
    x <- [x | x <- [0..n], 0 == mod (x*x - n) 4]
    let ysquare = div ((x*x) - n) 4
    guard $ squareP ysquare
    return (x,floor (sqrt (fromIntegral ysquare)))