-- Prime numbers using the Sieve method, without performing division.
-- Produces an infinite list. 
-- https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

-- We represent "crossed out" by False.
--
-- We start with no element crossed out:
--
-- 2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 ...
-- T  T  T  T  T  T  T  T  T  T  T  T  T  T  T  T  T  T  T ...
--
-- Look at the first element that is not crossed out, namely 2.
-- Keep it as it is, but cross out every second element
--
-- 2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 ...
-- T  T  F  T  F  T  F  T  F  T  F  T  F  T  F  T  F  T  F ...
--
-- Now we look at the next element which is not crossed out, namely 3.
-- Keep it as it is, but cross out every third element
--
-- 2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 ...
-- T  T  F  T  F  T  F  F  F  T  F  T  F  F  F  T  F  F  F ...
--
-- Now we look at the next element which is not crossed out, namely 5.
-- Keep it as it is, but cross out every fifth element
--
-- 2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 ...
-- T  T  F  T  F  T  F  F  F  T  F  T  F  F  F  T  F  F  F ...

--
-- And so on.
-- The elements that remain uncrossed are the prime numbers.
--
-- We don't need the numbers in the list.
-- An infinite list of booleans suffices. 
-- We start with the infinite lists of True's.
-- Then we repeatedly sieve it from 2:

sieve :: [Bool]
sieve = sievefrom 2 (repeat True)

-- To sieve an infinite list, we look at its head. 
--
-- If it is True, we output True, and we cross out every nth
-- element. We use an auxiliary function for this.
--
-- If it is False, we output False, and sieve from the tail,
-- increasing the position count n. 

sievefrom :: Int -> [Bool] -> [Bool]
sievefrom n (True  : xs) = True  : sievefrom (n+1) (cross n xs)
sievefrom n (False : xs) = False : sievefrom (n+1) xs

-- We now define cross n xs so that the result is xs with every nth
-- element crossed out. A helper function gets the count, which
-- decreases in recursive calls, and every time it reaches zero it
-- both crosses out and resets the counter to n-1 again.

cross :: Int -> [Bool] -> [Bool] 
cross n xs = cross' (n-1) xs
  where 
    cross' 0 (x : xs) = False : cross' (n-1) xs
    cross' i (x : xs) = x     : cross' (i-1) xs

-- We get 
-- take 50 sieve =
-- [True,True,False,True,False,True,False,False,False,True,False,True,False,
-- False,False,True,False,True,False,False,False,True,False,False,False,False,
-- False,True,False,True,False,False,False,False,False,True,False,False,False,
-- True,False,True,False,False,False,True,False,False,False,False]

-- Is a given number n prime? Look at the sequence at position n:

prime ::  Int -> Bool
prime n = sieve !! (n-2)

-- To get the list of primes, we convert the list of
-- booleans to a list of integers. 

primes :: [Int]
primes = primesfrom 2 sieve

primesfrom :: Int -> [Bool] -> [Int]
primesfrom n (True   : xs) = n : primesfrom (n+1) xs
primesfrom n (False  : xs) =     primesfrom (n+1) xs

-- We get 
-- take 20 primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]
--
-- The only arithmetic operations we performed were +1 and -1.
-- However, our algorithm is not very efficient.
--
-- Exercise. Modify this algorithm so that we compute the list of
-- prime factors instead for each number. Replace booleans by lists.
