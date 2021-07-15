module Codewars.Kata.Dioph where

import Control.Monad

solequa :: Integer -> [(Integer, Integer)]
solequa = test

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
    x <- reverse $ [n, n-1..0]
    guard ((mod ((x*x) - n ) 4) == 0)
    let ysquare = div ((x*x) - n) 4
    guard $ squareP ysquare
    return (x,floor (sqrt (fromIntegral ysquare)))
