module FactorialDecomposition.Kata (decomp) where
import Data.List ( intercalate )
import Data.Map as DM ( Map, fromList, assocs, unionWith )
import Data.Array ( Ix(range), (!), listArray )
import GHC.Integer (divInteger)

--https://stackoverflow.com/questions/3596502/lazy-list-of-prime-numbers
--plugging this in for the list of primes
primes :: [Integer]
primes = sieve (2 : [3, 5..])
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

primeFactors :: Integer -> [Integer]
primeFactors n  = [x | x <- (takeWhile (<= n) primes), n `mod` x == 0]

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

checkPrimeExp :: Integer -> Integer -> Integer
checkPrimeExp n prime = checkPrimeExpAux n prime 1
  where checkPrimeExpAux x p pow = if isInt ((fromIntegral x) / (fromIntegral (p ^ pow)))
                                   then 1 + (checkPrimeExpAux n (p) (pow+1))
                                   else 0

exps :: Integer -> [Integer] -> [Integer]
exps n [] = []
exps n (x:xs) = case next of
                  1 -> [pow]
                  otherwise -> pow : (exps next xs)
  where pow = checkPrimeExp n x
        next = divInteger n (x^pow)

factorization :: Integer -> [(Integer, Integer)]
factorization n = zipWith (,) pf powers
    where pf = primeFactors n
          powers = reverse $ exps n $ reverse pf

decomp :: Int -> String
decomp n = intercalate " * " $ Prelude.map fmt $ DM.assocs $ fdcomp (toInteger n)
  where fmt (b,p) = case p of
                    1 -> show b
                    otherwise -> show b ++ "^" ++ show p

fdcomp :: Integer -> Map Integer Integer
fdcomp n = fdAux n
  where
    bounds = (0,n)
    fdmemo = Data.Array.listArray bounds [fdAux x | x <- Data.Array.range bounds]
    fdAux x
      | x == 0 = DM.fromList []
      | otherwise = unionWith (+) (DM.fromList (factorization x)) (fdmemo Data.Array.! (x-1))