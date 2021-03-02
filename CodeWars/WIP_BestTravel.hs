-- https://www.codewars.com/kata/55e7280b40e1c4a06d0000aa/train/haskell

import Data.List ( subsequences, sort )
import qualified Data.IntMap.Strict as IMap
import qualified Data.Set as Set
import qualified Data.Heap as Heap

combos :: Int -> [a] -> [[a]]
combos n xs = filter ((== n) . length ) $ subsequences xs

chooseBestSum :: Int -> Int -> [Int] -> Maybe Int
chooseBestSum cost n xs = if best == 0
                          then Nothing
                          else Just best
    where best = foldl max 0 $ filter (<= cost) (sum <$> combos n xs)

chooseBestCost :: Int -> Int -> [Int] -> Maybe Int
chooseBestCost cost n xs = undefined
    where sXS = sort xs

-- assumes sorted xs                   Sum      Indexes
costMap :: Int -> Int -> [Int] -> IMap.IntMap (Set.Set Int) -- key is sum here, might need to use set of indexes instead of lists for speed
costMap cost 1 xs = IMap.fromAscList baseSums
    where filteredByCost = filter (<= cost) xs
          baseSums = zip filteredByCost $ fmap Set.singleton [1..]

costHeap :: Int -> Int -> [Int] -> Maybe Int
costHeap cost 1 xs = fmap fst resulthead
    where ts = zip xs (Set.singleton <$> [1..])
          hs = Heap.fromList ts :: Heap.MaxPrioHeap Int (Set.Set Int)
          candidates = Heap.break (\(p,s) -> p <= cost) hhs
          resulthead = Heap.viewHead . snd $ candidates

costHeap cost n (x:xs) = undefined

tts = zip [50..55] (Set.singleton <$> [1..])
hhs = Heap.fromList tts :: Heap.MaxPrioHeap Int (Set.Set Int)
ccs = 53
