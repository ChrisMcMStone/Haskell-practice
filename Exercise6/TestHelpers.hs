{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}
module TestHelpers where

--import Prelude hiding (catch)
--import Control.Applicative
import Control.Exception (assert)
--import Control.Monad hiding (replicateM)
import Data.List
--import Data.Char (isDigit)
--import qualified Control.Monad
--import Test.QuickCheck.Monadic (run, monadicIO, assert)
import Test.QuickCheck
import Exercise5
     (showBoard, runMoves, simulateMoves, genBoardSpec, runRand, BoardSpec, Move, Direction(..), Coord, Rand, playRandomly)


instance Ord Direction  where
    N <= _ = True
    _ <= W = True
    E <= E = True
    E <= S = True
    _ <= _ = False




showBoard_without_testing_whitespace = pad_to_wide_line . showBoard

-- Lines shouldn't get longer than this.
--
-- Assuming a max 26x26 board, lines shouldn't get longer than 53.
const_pad_length = 60

pad_to_wide_line :: String -> String
pad_to_wide_line s = unlines $ map padline $ lines s
  where
    padline line =
        assert (length line <= const_pad_length)
        $ line ++ (replicate (const_pad_length - length line) ' ')


func_runMoves :: (BoardSpec, [Move]) -> Maybe BoardSpec
func_runMoves (board, moves) = fmap sort $ runMoves board moves
func_simulateMoves :: (BoardSpec, [Move]) -> Maybe Int
func_simulateMoves (board, moves) = simulateMoves board moves

test_not_implemented_yet :: Property
test_not_implemented_yet = error "we still have to write this test"


--
-- Properties to test.
--
enough_random_boards :: Property
enough_random_boards = forAll arbSize hasEnoughRandomBoards

hasEnoughRandomBoards :: Int -> Bool
hasEnoughRandomBoards size
        | not allBoardsCorrect = error "not all boards were correct"
        | not (numWithoutDupl * 2 > numOfTests)
            = error ("not enough unique boards: only "
                    ++ show numWithoutDupl
                    ++ " unique boards out of "
                    ++ show numOfTests
                    ++ " using height/width "
                    ++ show size
                    ++ ".")
        | otherwise = True
        -- Our sample solutions work for
        --      numWithoutDupl * 4 > numOfTests * 3
        -- but students are allowed to have only
        --      numWithoutDupl * 2 > numOfTests
    where
        numOfTests = size
        seed       = size
        sizes = replicate numOfTests size

        -- numOfTests random boards, all sorted
        generatedBoards = fmap sort $ runRand seed $ mapM genBoardSpec sizes

        allBoardsCorrect = all (checkBrdCorrectness size) generatedBoards

        withoutDuplicates = fastNub generatedBoards
        numWithoutDupl    = length withoutDuplicates

checkBrdCorrectness :: Int       -- dimensions
                    -> BoardSpec -- assume that the board is sorted
                    -> Bool
checkBrdCorrectness size brd = go (fstCoords, fstCoords) brd
    where
        fstCoords = fst $ head brd

        go _ [] = True
        go mm [(coord,_)] = checkMM $ computeMM mm coord
        go mm ((coord,_):rest@((coord',_):_))
            | coord == coord' = False
            | otherwise       = go (computeMM mm coord) rest

        checkMM ((xMin, yMin), (xMax, yMax)) =
            xMax - xMin <= size && yMax - yMin <= size

        computeMM ((xMin, yMin), (xMax, yMax)) (x,y) =
            ((min xMin x, min yMin y), (max xMax x, max yMax y))

enough_rand_moves_for_board = enough_rand_moves_for_board' False

enough_rand_moves_for_board_for_testing = enough_rand_moves_for_board' True



-- Using the built-in side effects of Haskell
enough_rand_moves_for_board' :: Bool -> (BoardSpec, Int) -> ()
enough_rand_moves_for_board' stricter (board, seed)
        = ((seqList $ map assertPlayCorrectAndFinal withoutDuplicates)
           $ if not (numWithoutDupl >= numRequiredUniques)
             then error ("not enough unique plays: only "
                   ++ show numWithoutDupl
                   ++ " unique plays out of "
                   ++ show numOfTests ++ " tests; "
                   ++ show numRequiredUniques ++ " were required using board:\n"
                   ++ showBoard board)
          else ())
    where
        numRequiredUniques
            | length (filter snd board) <= 8 = 1
            | length (filter snd board) <= 10 = 5
            | length (filter snd board) <= 12 = 20
            | otherwise = 100

        numOfTests =
            if stricter
                then 3 * numRequiredUniques `div` 2
                else 2 * numRequiredUniques

        -- numOfTests random boards, all sorted
        generatedPlays = runRand seed $ sequence [playRandomly board | _ <- [1..numOfTests]]

        assertPlayCorrectAndFinal :: [Move] -> ()
        assertPlayCorrectAndFinal play = case runMoves board play of
            Nothing -> error (
                "Your generated play is incorrect: "
                ++ show play ++ "\n in board: " ++ showBoard board)
            Just board' ->
                if isFinal board'
                    then ()
                    else error (
                        "Your generated play doesn't play until the end: "
                        ++ show play ++ "\n in board: " ++ showBoard board)

        withoutDuplicates = fastNub generatedPlays
        numWithoutDupl    = length withoutDuplicates

-- You are not expected to understand this.
seqList :: [a] -> b -> b
seqList [] y = y
seqList (x:xs) y = x `seq` seqList xs y

arbSize :: Gen Int
arbSize = do
    arbitrary `suchThat` (> 0)

--
-- Helper functions.
--
uniq :: Eq a => [a] -> [a]
uniq (a1:a2:xs)
    | a1 == a2 = uniq (a2:xs)
    | otherwise = a1 : (uniq (a2:xs))
uniq [a1] = [a1]
uniq [] = []

fastNub :: (Ord a, Eq a) => [a] -> [a]
fastNub = uniq . sort

testLess :: Args
testLess = stdArgs { maxSuccess = 17 }

isFinal :: BoardSpec -> Bool
isFinal board = not $ any movePossible allTriples
  where
    movePossible (False, True, True) = True
    movePossible (True, True, False) = True
    movePossible _ = False

    allTriples = (concatMap (increasingTriples getX) $ rows board)
               ++(concatMap (increasingTriples getY) $ columns board)

type Cell = (Coord, Bool)
columns :: BoardSpec -> [[Cell]]
rows :: BoardSpec -> [[Cell]]
columns = groupBy' getX
rows = groupBy' getY

getX ((x, _), _) = x
getY ((_, y), _) = y

-- sorts and groups
groupBy' :: (Ord b, Eq b) => (a -> b) -> [a] -> [[a]]
groupBy' key l = groupBy grouping $ sortBy comparison l
  where
    comparison x y = compare (key x) (key y)
    grouping x y = (==) (key x) (key y)

increasingTriples :: (Eq c, Enum c) => ((b, a) -> c) -> [(b, a)] -> [(a, a, a)]
increasingTriples key ((c1, z1):rest@((c2, z2):(c3, z3):_))
    | succ (key (c1, z1)) == (key (c2, z2)) && succ (key (c2, z2)) == (key (c3, z3))
        = (z1, z2, z3) : increasingTriples key rest
    | otherwise = increasingTriples key rest
increasingTriples key _ = []
