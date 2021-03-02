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

type SumMaxHeapWInd = Heap.HeapT (Heap.Prio Heap.FstMaxPolicy (Int, Set.Set Int)) (Set.Set Int)
type SumWIndItem = (Int, Set.Set Int)

costHeap :: Int -> Int -> [Int] -> SumMaxHeapWInd
costHeap _ _ [] = Heap.empty
costHeap cost 1 xs = tail_candidates 
    where ts = zip xs (Set.singleton <$> [1..])
          hs = Heap.fromList ts :: Heap.MaxPrioHeap Int (Set.Set Int)
          (prefix, tail_candidates) = Heap.break (\(p,s) -> p <= cost) hhs

costHeap cost n xs = undefined
    where ts = zip xs [1..]
          subCostHeap = costHeap cost (n-1) xs
          candidate_heaps = fmap (proccessSubHeap subCostHeap cost) ts
          result = Heap.unions candidate_heaps
          
-- Heap.break (\(p,s) p + x <= cost && x notMember s) (costHeap cost (n-1) xs)
-- union (fmap (\(x,ind) ))

-- Heap.break for viable sums, then fmap the new sum operand and index into the set
proccessSubHeap :: SumMaxHeapWInd -> Int -> (Int, Int) -> SumMaxHeapWInd
proccessSubHeap = undefined
-- TODO fix this
-- proccessSubHeap subCostHeap cost choice@(x, index) = result
--     where (_, candidate_heap) = Heap.break (pViableSum cost choice) subCostHeap :: ([SumWIndItem], SumMaxHeapWInd)
--           result = fmap (newSum choice) candidate_heap

pViableSum :: Int -> (Int, Int) -> (Int, Set.Set Int) -> Bool
pViableSum cost (x, index) (p,s) = p + x <= cost && (Set.notMember x s)

newSum :: (Int, Int) -> (Int, Set.Set Int) -> (Int, Set.Set Int)
newSum (x, index) (p,s) = (x+p, Set.insert index s)

pluckSum :: SumMaxHeapWInd -> Maybe Int 
pluckSum = fmap fst . Heap.viewHead

bts = zip [50..55] (Set.singleton <$> [1..])
ts = zip [70..75] [1..]
hhs = Heap.fromList bts :: Heap.MaxPrioHeap Int (Set.Set Int)
ccs = 127