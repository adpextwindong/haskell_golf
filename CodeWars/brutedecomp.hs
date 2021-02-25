{-# LANGUAGE MagicHash #-}

module FactorialDecomposition.Kata (decomp) where
import Data.List ( intercalate )
import GHC.Integer (divInteger)

--https://stackoverflow.com/questions/3596502/lazy-list-of-prime-numbers
--plugging this in for the list of primes
primes :: [Integer]
primes = sieve (2 : [3, 5..])
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

fact :: Integer -> Integer
fact 1 = 1
fact n = n * fact (n-1)

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

factorizationOfNonPrime n = zipWith (,) pf powers
    where pf = primeFactors n
          powers = reverse $ exps n $ reverse pf
          
decomp :: Integer -> String
decomp n = intercalate " * " $ map fmt ts
  where pf = primeFactors (fact n)
        powers = reverse $ exps (fact n) (reverse pf)
        ts = zipWith (,) pf powers
        fmt (b,p) = case p of
                    1 -> show b
                    otherwise -> show b ++ "^" ++ show p


--TODO this brute force method only works till 7!
--It seems like a dp problem

--take the prime factors of n and join it against the prime factor map of n-1 .... 1

factorialDecomposition :: Integer -> Map Integer Integer