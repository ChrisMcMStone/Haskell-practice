import System.Random

{-

There are monads for all sorts of things in Haskell. In this module we
will only touch some of them.

Here I motivate and define a monad for (pseudo) probabilistic
computation.

The sample application I consider is quicksort.

There is another example in your exercise: play a game randomly. We
also generate random initial board states in our test bench.

-}

{-

We begin with a version of deterministic quicksort, where we choose the
pivot as the first element of the list.

-}

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =  qsort [y | y <- xs, y < x] 
             ++ [x] 
             ++ qsort [y | y <- xs, y >= x] 

{-

This is nice, short and simple. And it has the usual time complexity:
n^2 in the worst case, and n*log(n) on average (as most of you learned
in the Foundations module).

Let's try to confirm this experimentally. 

To test the average behavariour, we will use random lists,
manufactured with System.Random. 

  http://hackage.haskell.org/package/random-1.1/docs/System-Random.html

  The class RandomGen provides a common interface to random number generators.

  StdGen is a type for "standard random number generation".

  mkStdGen :: Int -> StdGen 
  creates a generator with a given "seed".

  next :: Random g => g -> (Int , g)
  given a generator g, produces a random integer and new generator.

  random :: (RandomGen g, Random a) => g -> (a, g)
  similar

  randoms :: (RandomGen g, Random a) => g -> [a] 
  infinite random list

  randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
  produces a random number within a range
  
-}

randoms' :: (RandomGen g, Random a) => g -> [a] 
randoms' g = let (x , g') = random g 
             in x : randoms' g'

{-

Some tests:

*Main> length(qsort (take 200 (randoms (mkStdGen 17))))
200
(0.01 secs, 2071036 bytes)

*Main> length(qsort (take 2000 (randoms (mkStdGen 17))))
2000
(0.03 secs, 7280096 bytes)

*Main> length(qsort (take 20000 (randoms (mkStdGen 17))))
20000
(0.24 secs, 65687492 bytes)

*Main> length(qsort (take 200000 (randoms (mkStdGen 17))))
200000
(2.72 secs, 736396732 bytes)

*Main> length(qsort [1..200])
200
(0.02 secs, 4143824 bytes)

*Main> length(qsort [1..2000])
2000
(0.98 secs, 187177132 bytes)

*Main> length(qsort [1..4000])
4000
(3.83 secs, 739226592 bytes)

*Main> length(qsort [1..8000])
8000
(14.18 secs, 2948828780 bytes)

*Main> length(qsort [1..20000])
20000
(87.97 secs, 18408167984 bytes)
*Main> 


Randomized quicksort makes the expected time, rather than the average
time, to be n*log(n). In particular, this should reduce the time of
the slow examples above.

I will write it in two ways: directly, and with a monad. The two of
them are equivalent, but the second one resembles more closely what we
would write in an imperative language such as Java.

Without a monad. We need to pass a seed around.

-}

-- The pivot is the nth element of the list.
getPivot :: [a] -> Int -> (a, [a])
getPivot (x:xs) 0 = (x,xs)
getPivot (x:xs) n = let (p,ys) = getPivot xs (n-1) in (p, x:ys)

-- We use the standard random number generator for randomized quicksort.
rqsort1' :: Ord a => [a] -> StdGen -> ([a], StdGen)
rqsort1' [] g = ([], g)
rqsort1' xs g =
 let
   (n, g1)   = randomR (0, length xs - 1) g 
   (p, ys)   = getPivot xs n
   (as, g2)  = rqsort1' [y | y <- ys, y < p] g1
   (bs, g3)  = rqsort1' [y | y <- ys, y >= p] g2
 in  
   (as ++ [p] ++ bs, g3)

rqsort1 :: Ord a => [a] -> [a]
rqsort1 xs = ys
 where  
  seed = 3 -- say
  (ys, _) = rqsort1' xs (mkStdGen seed)

{-

Experiments:

*Main> length(rqsort1 (take 200000 (randoms (mkStdGen 17))))
200000
(6.01 secs, 1381747820 bytes)
*Main> length(rqsort1 [1..200000])
200000
(5.56 secs, 1252674148 bytes)

We gain something, we lose something. 

Rogue case got much faster. Random case doubled its time.

(But our implementation is very naive: we should avoid computing
lengths, for instance.)

-}


{-

It is not nice to have to pass the generator around. It would be much
nicer to just call the random number generator to produce the next
random thing, like one does in many imperative languages.

This can be accomplished using a monad. This monad is predefined
somewhere in some Haskell library (find out). 

But we will define it ourselves:

-}

data Rand a = Box(StdGen -> (a , StdGen))

{-

An element of the random monad is a function StdGen->(a , StdGen),
which we put in a "box".

NB. In the exercise we defined it with a different naming convention:

data Rand a = Rand(StdGen -> (a , StdGen))

Then "Rand" in the left-hand side and right-hand side are *different*.

In the lhs it is the name of a *type constructor* (given the type a, we get the type Rand a).

In the rhs it is the name of a *value constructor* 

This is the usual naming convention for Haskell, but I am departing
from it here just for emphasis. (In the exercise we keep the usual
naming convention.)

Recall the types of return and bind (>>=).

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

So in the following we have the type signatures

   h  :: g -> (a , g)
   h' :: g -> (b , g)
   f  :: a -> Rand b 

-}

instance Monad Rand where
 return x = Box(\g -> (x,g))
 Box h >>= f = Box(\g -> let (x, g') = h g 
                             (Box h') = f x
                         in h' g')
 
randInt :: Rand Int
randInt = Box random

randIntR :: (Int, Int) -> Rand Int
randIntR (lower, upper) = Box(randomR(lower, upper))

-- This is how we "can get out" of Rand a: 
runRand :: Int -> Rand a -> a
runRand seed (Box h) = fst(h(mkStdGen seed))


-- Picks a random element with uniform distribution:
uniform :: [a] -> Rand a
uniform xs = do
    n <- randIntR (0, length xs - 1)
    return(xs !! n)

-- Randomized quicksort using our random monad.

rqsort2' :: Ord a => [a] -> Rand [a]
rqsort2' [] = return []
rqsort2' xs = do n <- randIntR (0, length xs - 1)
                 let (p, ys) = getPivot xs n
                 as <- rqsort2' [y | y <- ys, y < p] 
                 bs <- rqsort2' [y | y <- ys, y >= p] 
                 return(as ++ [p] ++ bs)

rqsort2 :: Ord a => [a] -> [a]
rqsort2 xs = runRand seed (rqsort2' xs)
 where seed = 3 -- say


{-

*Main> length(rqsort2 (take 200000 (randoms (mkStdGen 17))))
200000
(6.53 secs, 1511391068 bytes)

*Main> length(rqsort2 [1..200000])
200000
(6.12 secs, 1382945268 bytes)

-}

-- This was called blah in the lecture, which we did it together:

randomList :: Int -> Rand [Int]
randomList 0 = return []
randomList n = do  
  i <- randInt
  xs <- randomList(n-1)
  return(i : xs)

{- Wrong attempts in the lecture (to be avoided in the lab):

randomList :: Int -> Rand [Int]
randomList 0 = []
randomList n = do  
  i <- randInt
  return(i : randomList(n-1))

-}

randomList1 :: Int -> Rand [Int]
randomList1 0 = return []
randomList1 n = randInt >>= (\i -> randomList1(n-1) >>= (\xs -> return(i : xs)))

-- The silly Box is making things more difficult to understand than
-- necessary. But we still need it, unfortunately, to conform with
-- Haskell's conventions. However, we can avoid it in order to explain
-- the above program more directly.

type Rand' a = StdGen -> (a , StdGen)

return' :: a -> Rand' a
return' x = \g -> (x,g)

infixl 3 >>==

(>>==) :: Rand' a -> (a -> Rand' b) -> Rand' b
h >>== f = \g -> let (x, g') = h g in f x g'
         
randInt' :: Rand' Int
randInt' = random

randomList' :: Int -> Rand' [Int]
randomList' 0 = return' []
randomList' n = randInt' >>== (\i -> randomList'(n-1) >>== (\xs -> return'(i : xs)))

{- Now let's unfold the definitions:

randomList' 0 
  = return' []
  = \g -> ([], g)

randomList' n 
  = randInt' >>== (\i -> randomList'(n-1) >>== (\xs -> return'(i : xs)))
  = random  >>== (\i -> randomList'(n-1) >>== (\xs -> return'(i : xs)))    
  = \g -> let (x,g1) = random g in (\i -> randomList'(n-1) >>== (\xs -> return'(i : xs))) x g1
  = \g -> let (x,g1) = random g in randomList'(n-1) >>== (\xs -> return'(x : xs)) g1
  = \g -> let (x,g1) = random g in let (xs,g2) = randomList'(n-1) g1 in (\xs -> return'(x : xs)) xs g2
  = \g -> let (x,g1) = random g in let (xs,g2) = randomList'(n-1) g1 in return'(x : xs) g2
  = \g -> let (x,g1) = random g in let (xs,g2) = randomList'(n-1) g1 in (x : xs, g2)
  = \g -> let (x,g1) = random g
              (xs,g2) = randomList'(n-1) g1 
          in (x : xs, g2)

This shows that the above program randomList' is equivalent to the following:

-}

randomList'' :: Int -> StdGen -> ([Int], StdGen)
randomList'' 0 g = ([], g)
randomList'' n g = let (x,g1) = random g
                       (xs,g2) = randomList''(n-1) g1 
                   in (x : xs, g2)

{-

Hence all the monad does is to keep track of the various g's, passing
them around appropriately (with >>=).

-}
