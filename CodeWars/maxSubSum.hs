import Data.Array

naiveSubSeqs :: [Int] -> [[Int]]
naiveSubSeqs [] = []
--, (subSeqs drop1FromEnd xs)
naiveSubSeqs xs = (xs : naiveSubSeqs (drop1 xs)) ++ naiveSubSeqs (drop1FromEnd xs)
  where
    drop1 = drop 1
    drop1FromEnd = drop 1 . reverse

-- 
subSeq' :: [Int] -> Int
subSeq' xs = foldr max 0 $ Data.Array.elems memo 
  --seq this?
  where len = length xs -- tabulate the memoized results into dsAux
        bounds = ((0,0), (len,len))
        memo = Data.Array.listArray bounds [dsAux i j | (i,j) <- Data.Array.range bounds]
        dsAux x y --todo incorporate y
            |  x + y >= len = 0 --we have gone 
            |  x + y == (len - 1) = xs !! x --we have dropped enough that its a single
            |  otherwise = max (xs !! x + memo ! ((x+1), y)) (memo ! (x, y +1))
                  -- need some array that indexes like (x,y)
                  -- so we can check solutions at (x+1,y) or (x,y+1)
                  -- to represent the smaller problem where we dropped from left or right
                  -- maybe make this xs !! x constant access w/ a vector or something

-- considering the choices are embedded in the array indices
-- at any memo cell return the max of the current spot + drop left || 

-- Return the greatest subarray sum within the array of integers passed in.
maxSequence :: [Int] -> Int
maxSequence [] = 0
maxSequence xs = subSeq' xs
--foldr max 0 $ map sum $ naiveSubSeqs xs