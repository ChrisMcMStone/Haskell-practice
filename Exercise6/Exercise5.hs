{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
module Exercise5 where

import System.Random
import Data.Char
import Data.List
import Control.Monad
import Control.Applicative
import Data.Maybe


{-

    This exercise will run over several weeks, with three deadlines:

    Part #1. Tuesday 11th November, 23.59.
    Part #2. Friday  21th November, 23.59.
    Part #3. Thursday 4th December, 23.59.

    Testbench releases:

    Part #1. Friday  7th November noon.
    Part #2. Friday 14th November noon.
    Part #3. Friday 21th November, or before.

    This is the last programming exercise for the module.

    For each deadline, submit this file with the solutions completed
    up to the required point (or more) on canvas.

    We will play "peg solitaire" 
    https://en.wikipedia.org/wiki/Peg_solitaire

    You might find this website useful for testing:

    http://www.pegu.it/

Before stating the exercise, we will give a few definitions.

-}

data Direction = N | E | S | W
                 deriving (Eq, Show, Read)

-- Cartesian coordinate system
-- (as used in Java 2D API)
-- (x,y) :
-- the x coordinate increases to the right, and
-- the y coordinate increases downward,
-- as shown in the following figure
--
--      a b c d
--      0 1 2 3 ----> x
--  a 0
--  b 1
--  c 2
--    |
--    V
--    y
type Coord = (Int, Int)

-- A move is a coordinate and a direction.
type Move = (Coord, Direction)

-- Exercise (40 points). For deadline #1.
--
-- Define the following function to parse a string and produce a
-- sequence of moves in the above format.
--
--
-- An example of a string of moves is "adENba2EEWS".
--
-- This should produce 
-- [((0,3),E),((2,3),N),((1,0),E),((3,0),E),((5,0),E),((7,0),W),((5,0),S)]
--
--   Two consecutive lower case letters are coordinates, as above, to
--   move the current position to.
--
--   The letters N, E, W, S are directions to move to. 
--
--   A non-negative number specifies how many times the next
--   move N, E, W, or S should be performed. 
--
-- So, in the above example,
-- which contains "2EE", the move E will be performed 2+1 times.
--
-- Ignore all spaces (newline, tab, blank) using the function isSpace
-- from the imported module Data.Char.
--
--

parseMoves :: String -> [Move]
parseMoves [] = []
parseMoves [_] = error "fucked it"
parseMoves (x:y:xs) | isLower x && isLower y = parseMoves' coord xs
                    | otherwise = error "fucked it again"
        where
                coord = toCoord x y

parseMoves' :: Coord -> String -> [Move]
parseMoves' _ [] = []
parseMoves' (x,y) ('N':xs) = ((x,y), N) : parseMoves' (x, y-2) xs
parseMoves' (x,y) ('E':xs) = ((x,y), E) : parseMoves' (x+2, y) xs
parseMoves' (x,y) ('S':xs) = ((x,y), S) : parseMoves' (x, y+2) xs
parseMoves' (x,y) ('W':xs) = ((x,y), W) : parseMoves' (x-2, y) xs
parseMoves' co ('-':xs) = parseMoves' co (dropWhile isDigit xs)
parseMoves' co xs = case digits of [] -> parseMoves xs
                                   n -> parseMoves' co (replicate (read n) (head rest) ++ tail rest)
                        where (digits, rest) = span isDigit xs 
                                  
toCoord :: Char -> Char -> Coord
toCoord x y = (ord x - 97, ord y - 97)


-- We need the following definition for the next exercise.
--
-- The state of each place in the board is one of the following:
--
--  - illegal
--  - empty
--  - occupied
--
-- So, a legal position is one which is empty or occupied.
--
-- When (i,j) is illegal then no ((i,j),_) is in the list defining the
-- state of the board.
--
-- ((i,j),True)  means (i,j) is occupied by a peg, and
-- ((i,j),False) means (i,j) is empty,
-- where 0 <= i,j <= 25, i.e. the maximum size of a board is 26x26
type BoardSpec = [(Coord, Bool)]

-- Exercise (10 points). For deadline #1.
--
-- Write a function that counts how many pegs
-- are in a given board:
countPeg :: BoardSpec -> Int
countPeg = foldr(\(_, p) acc -> if p then acc+1 else acc) 0

-- Exercise (40 points). For deadline #1.
--
-- Convert a board to a String.
--
-- An example of a output is
--
--      o o o
--      o o o
--  o o o o o o o
--  o o o - o o o
--  o o o o o o o
--      o o o
--      o o o
--
-- There is a space between each pair of adjacent cells in the same line.
-- An illegal cell is presented by a space if there are legal cells on the right. 
-- 'o' means that the cell is occupied by a peg, and
-- '-' means that the cell is empty.
-- Notice that there is NO spaces in the end of each line.
--
-- This will be useful for you to test your program.


findMax :: Coord -> BoardSpec -> Coord
findMax co [] = co
findMax (x,y) (((i,j), _):xs) | i > x && j > y = findMax (i, j) xs
                              | i > x = findMax (i, y) xs
                              | j > y = findMax (x, j) xs
                              | otherwise = findMax (x, y) xs

loop :: Coord -> Coord -> BoardSpec -> String
loop _ _ [] = ""
loop (xM, yM) co@(x, y) bs@((coB, peg):xs) | (xM == x) && (yM ==y) = [insertChar peg]
                                           | x > xM = '\n' : loop (xM, yM) (0, y+1) bs
                                           | otherwise = if co == coB then
                                                                      insertChar peg : ' ' : loop (xM, yM) (x+1, y) xs
                                                                      else
                                                                         ' ' : ' ' : loop (xM, yM) (x+1, y) bs 
trimSpaces :: String -> String
trimSpaces = intercalate "\n" . map (reverse . dropWhile (== ' ') . reverse) . lines

flipB :: BoardSpec -> BoardSpec
flipB = map (\((x,y),p) -> ((y,x),p))

insertChar :: Bool -> Char
insertChar True = 'o'
insertChar _ = '-'

showBoard :: BoardSpec -> String
showBoard board = trimSpaces $ loop maxC (0,0) (flipB $ sort $ flipB board)
        where 
         maxC = findMax (0,0) board

english :: BoardSpec
english = [ ((x, y), not $ all (==3) [x,y])
          | x <- [0..6]
          , y <- [0..6]
          , (x,y) `notElem` [ (x',y')
                            | let xs = [0,1,5,6]
                            , x' <- xs
                            , y' <- xs
                            ]
          ]


-- This too:
printBoard :: BoardSpec -> IO ()
printBoard = putStrLn . showBoard



-- Exercise (40 points). For deadline #1.
--
-- Given a board specification (of type BoardSpec defined below), and
-- given a list of moves (of type Move defined above), produce either
-- Nothing (when a move in the list is impossible), or Just the
-- resulting board after performing all the moves.
-- 
-- Using this, and the function countPeg you defined above, also
-- define a function simulateMoves which only gives Just the number of
-- resulting pegs, or Nothing if an illegal move occurs:


runMoves :: BoardSpec -> [Move] -> Maybe BoardSpec
runMoves bs [] = return bs
runMoves bs ((co, d):moves) | isValidMove bs co d =
                                       let newBoard = makeMove bs (co, d) in
                                               runMoves newBoard moves
                            | otherwise = Nothing
                        
makeMove :: BoardSpec -> Move -> BoardSpec
makeMove bs (coord, d) = map (\(co, p) -> case () of 
                                        _ | co == coord -> (co, False) 
                                        _ | co == applyDirection 1 coord d -> (co, False)
                                        _ | co == applyDirection 2 coord d -> (co, True)
                                        _  -> (co, p)) bs

isValidMove :: BoardSpec -> Coord -> Direction -> Bool
isValidMove bs co d = isPeg bs co && isPegNext bs co d && isValidPos bs co d

applyDirection :: Int -> Coord -> Direction -> Coord
applyDirection n (x, y) N = (x, y-n)
applyDirection n (x, y) S = (x, y+n)
applyDirection n (x, y) E = (x+n, y)
applyDirection n (x, y) W = (x-n, y)

isPeg :: BoardSpec -> Coord -> Bool
isPeg bs coord = (coord, True) `elem` bs

isPegNext :: BoardSpec -> Coord -> Direction -> Bool
isPegNext bs (x, y) N = isPeg bs (x, y-1)
isPegNext bs (x, y) S = isPeg bs (x, y+1)
isPegNext bs (x, y) E = isPeg bs (x+1, y)
isPegNext bs (x, y) W = isPeg bs (x-1, y)

isValidPos :: BoardSpec -> Coord -> Direction -> Bool
isValidPos bs (x, y) N = ((x, y-2), False) `elem` bs
isValidPos bs (x, y) S = ((x, y+2), False) `elem` bs
isValidPos bs (x, y) E = ((x+2, y), False) `elem` bs
isValidPos bs (x, y) W = ((x-2, y), False) `elem` bs


simulateMoves :: BoardSpec -> [Move] -> Maybe Int
simulateMoves bs ms = countPeg <$> runMoves bs ms

-- Exercise (40 points). For deadline #2
--
-- Two positions of a board are adjacent if (1) they are next to each
-- other vertically, or (2) they are next to each other
-- horizontally. A board is called connected if every legal position
-- may be reached from any other legal position by travelling via
-- Check whether (the legal positions of) a board are connected. 

isConnected :: BoardSpec -> Bool
isConnected bs = numConnectedComponents bs == 1

-- Bonus Exercise (20 points). For deadline #2
--
-- Count how many connected components the legal positions form.
--
-- There is at most one connected component if the board is connected
-- as above.
numConnectedComponents :: BoardSpec -> Int
numConnectedComponents bs = numConnectedComponents2 $ map fst bs

numConnectedComponents2 :: [Coord] -> Int
numConnectedComponents2 [] = 0
numConnectedComponents2 ys@(x:_) = 1 + numConnectedComponents2 rest
        where
                rest = ys \\ gatherComponent ys [x] [x]

gatherComponent :: [Coord] -> [Coord] -> [Coord] -> [Coord]
gatherComponent _ [] comp = comp
gatherComponent bs (x:xs) comp = gatherComponent bs newSet newComp
        where
                newComp = comp ++ adjacent
                newSet = xs `union` adjacent
                adjacent = getAdj bs x comp

getAdj :: [Coord] -> Coord -> [Coord] -> [Coord]
getAdj bs (x,y) visited = n ++ s ++ e ++ w
        where
                n = checkVisited bs visited (x, y-1)
                s = checkVisited bs visited (x, y+1)
                e = checkVisited bs visited (x+1, y)
                w = checkVisited bs visited (x-1, y)

checkVisited :: [Coord] -> [Coord] -> Coord -> [Coord]
checkVisited bs visited co
        |co `notElem` bs = []
        |otherwise = if co `elem` visited then [] else [co]


-- We define our own Rand monad for random number generation:
newtype Rand a = Rand (StdGen -> (a , StdGen))

instance Monad Rand where
 return x = Rand (\s -> (x,s))
 Rand g >>= f = Rand (\s -> let (x, s') = g s 
                                (Rand h) = f x
                           in h s')
 
randDouble :: Rand Double
randDouble = Rand random

randInt :: Rand Int
randInt = Rand random

randBool :: Rand Bool
randBool = Rand random

-- Run a Rand with the specified initial seed
runRand :: Int -> Rand a -> a
runRand seed (Rand g) = fst(g (mkStdGen seed))


-- Generates a random element
uniform :: [a] -> Rand a
uniform l = do
    i <- randInt
    let n = abs i `mod` length l
    return $ l !! n

-- Example:
--
-- *Template6 Control.Monad> runRand 1 (replicateM 30 (uniform [1..10]))
-- [5,2,1,1,10,8,5,7,2,8,7,2,6,3,5,7,1,10,3,6,6,1,9,2,7,8,3,10,6,4]
--
-- (This in principle can give different results in different versions of Haskell,
--  if the random generation procedure in the libraries are changed.)

testRand :: Rand (Int, Int, Int)
testRand = do
    x <- uniform [1..10]
    y <- uniform [1..10]
    z <- uniform [1..10]
    return (x, y, z)

-- Exercise (30 points). For deadline #2.
-- Generate a random square board of a certain height and width.
--
{- 1. mapMaybe across generated coordinates using *uniform [True, False]*,
- to determine if they exist or not.
- This will throw out the Nothings and leave Just Coord 's.A
- 
-}

genBoardSpec :: Int -> Rand BoardSpec 
genBoardSpec n = 
        let xs = [0..(n-1)]
            board = [(x,y) | x <- xs, y <- xs] in
        liftM catMaybes (mapM genSingle board)
        where
                genSingle (x,y) = do
                        exist <- uniform [True, False]
                        peg <- uniform [True, False]
                        return (if exist then Just ((x,y), peg) else Nothing)


-- Exercise (30 points). For deadline #2.
--
-- Given a board, randomly play legal moves until no further legal
-- moves are possible.

playRandomly :: BoardSpec -> Rand [Move]
playRandomly board = let moves = possibleMoves board in
                case moves of [] -> return []
                              _ -> do
                                        move <- uniform moves
                                        let newBoard = makeMove board move
                                        liftM (move:) (playRandomly newBoard)


possibleMoves :: BoardSpec -> [Move]
possibleMoves bs = foldr (\(co, _) acc -> getValidMoves bs co ++ acc) [] bs

getValidMoves :: BoardSpec -> Coord -> [Move]
getValidMoves bs co = n ++ s ++ e ++ w
        where
                n = [(co,N) | isValidMove bs co N]
                s = [(co,S) | isValidMove bs co S]
                e = [(co,E) | isValidMove bs co E]
                w = [(co,W) | isValidMove bs co W]

-- Exercise (100 points). For deadline #3.
--
-- This will be your best try for playing the game intelligently.
--
-- To get any mark at all in this exercise, your solution should (1)
-- take no more than the test bench allows, in a lab machine, in a
-- board size given by the test bench, and (2) be strictly better than
-- a random play.
--
-- The test bench for this part will be given in advance.
--
--
-- Mark allocation, subject to the above restrictions.
--
--     100 for ending up with one peg, and fast (according to the testbench).
--      70 for ending up with one peg, and reasonably fast (according to the testbench).
--   40-69 for ending with few pegs, and reasonably fast (according to the testbench).
--    1-39 see test bench results.
--
-- The test bench will only give you approximate marks, due to the
-- probabilistic nature of our testing. We will test your program
-- several times and take averages.

play :: BoardSpec -> [Move]
play = undefined

