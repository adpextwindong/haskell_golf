baseVotes :: Int -> Int
baseVotes x = (2 * x) + bonusPoints x

{-
--Mutually exclusive clauses
bonus_points :: Int -> Int
bonus_points x | x < 50 = 0
               | x < 100 = 25 * (div x 50)
               | otherwise = 50 * (div x 100)
-}

--Clauses "stack"
bonusPoints x = clause50 x + clause100 x

clause50 x = 25 * (div x 50)

clause100 x = 50 * (div x 100)
