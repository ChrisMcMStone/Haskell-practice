module Exercise1 where

-- Bonus Exercise. 10 points.
--
-- Modify the algorithm below so that we compute the list of prime
-- factors instead for each number. Replace booleans by lists.
-- 
-- For example, if we evaluate 
--
--     take 30 sieve
--
-- you should get the factors, without repetition and in decreasing order:
--
-- [[],[],[2],[],[3,2],[],[2],[3],[5,2],[],[3,2],[],[7,2],[5,3],[2],[],[3,2],[],[5,2],[7,3],[11,2],[],[3,2],[5],[13,2],[3],[7,2],[],[5,3,2],[]]
--
-- We won't give a marking script for bonus questions.
--
-- You should change both the type signatures and the definitions at some points.
-- 
--                    W A R N I N G
--
-- The names of the functions should *not* be changed -- otherwise
-- automatic marking won't work and you won't get any mark. 
--
-- This will  be strictly inforced in this and all exercises.

sieve :: [[Int]] -- Change this type to [[Int]]
sieve = sievefrom 2 (repeat []) -- Change this too.

sievefrom :: Int -> [[Int]] -> [[Int]] -- Change the type appropriately.
sievefrom n ([]  : xs) = []  : sievefrom (n+1) (cross n xs) -- What else needs to be changed?
sievefrom n (x : xs) = x : sievefrom (n+1) xs

cross :: Int -> [[Int]] -> [[Int]] -- From now on you are on your own.
cross n xs = cross' (n-1) xs
  where 
    cross' 0 (x : xs) = (n : x) : cross' (n-1) xs
    cross' i (x : xs) = x : cross' (i-1) xs

primes :: [Int]
primes = primesfrom 2 sieve

primesfrom :: Int -> [[Int]] -> [Int]
primesfrom n ([]   : xs) = n : primesfrom (n+1) xs
primesfrom n (_ : xs) = primesfrom (n+1) xs

