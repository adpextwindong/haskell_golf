module Haskell.SylarDoom.MazeRunner where
import Data.List
import Data.Maybe

type Maze = [[Int]]
type Position = (Int,Int)
type Direction = Char

data MzResult = Finish | Dead | Lost | Oob
  deriving (Show)

mzDirAdd :: Position -> Direction -> Position
mzDirAdd (px,py) d = (px+dx, py+dy)
  where (dx,dy) = case d of
                  'N' -> (1,0)
                  'S' -> (-1,0)
                  'W' -> (0,-1)
                  'E' -> (0,1)

inbounds :: Int -> Position -> Bool
inbounds n (x,y) = (x < n && x >= 0) && (y < n && y >= 0)

mzLookup :: Maze -> Position -> MzResult
mzLookup m p@(x,y) = if not (inbounds (length m) p)
                     then
                       Oob
                     else
                       case m !! x !! y of
                         0 -> Lost
                         1 -> Dead
                         2 -> Lost
                         3 -> Finish
                 
--This should be monadic or something
mzAux :: Maze -> Position -> [Direction] -> MzResult
mzAux m p [] = mzLookup m p --lookup
mzAux m p (d:ds) = do
  let newPos = mzDirAdd p d
  let res = mzLookup m p
  case res of
    Lost -> mzAux m newPos ds
    otherwise -> res
  
findStartPos :: Maze -> Position
findStartPos mz = (x,y)
  where ts = map (elemIndex 2) mz
        x = length (takeWhile (== Nothing) ts)
        y = fromJust $ head $ dropWhile (== Nothing) ts

mazeRunner :: [[Int]] -> [Char] -> [Char]
mazeRunner maze directions = show $ mzAux maze startPos directions 
  where startPos = findStartPos maze

--TODO find starting position index

maze = [[1,1,1,1,1,1,1],
        [1,0,0,0,0,0,3],
        [1,0,1,0,1,0,1],
        [0,0,1,0,0,0,1],
        [1,0,1,0,1,0,1],
        [1,0,0,0,0,0,1],
        [1,2,1,0,1,0,1]] :: Maze