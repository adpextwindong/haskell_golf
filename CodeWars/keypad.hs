module Haskell.Codewars.KeypadEntry where
import Data.Char

presses :: String -> Int
presses = sum . map pCount . map toUpper

pCount :: Char -> Int
pCount c
  | elem c "PQRS" = 1 + (ord 'P' - (ord c))
  | elem c "WXYZ" = 1 + (ord 'W' - (ord c))
  | elem c "TUV" = 1 + (ord 'T' - (ord c))
  | c == ' ' = 1
  | otherwise = 1 + ((ord c - ord 'A') `mod` 3)
  