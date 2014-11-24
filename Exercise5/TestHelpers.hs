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
import Exercise5 (showBoard, runMoves, simulateMoves, BoardSpec, Move)


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
