module Helpers where

import Exercise2(Expr(..), Sequence(..), Tree(..), Tree'(..), Result(Value, Error), toNormal, fromNormal, sequenceMap, sequenceAppend, sequenceFlatten, sequenceSplit, treeMap, treeMap')
import qualified Exercise2 as Solution

-- We can use this to test parametricity.
newtype Wrap a = Wrap a deriving (Show, Eq)

-- To wrap a unary function, use fmap. 
-- To wrap a binary function:
wrap2 :: (a -> b -> c) -> Wrap a -> Wrap b -> Wrap c
wrap2 f (Wrap x) (Wrap y) = Wrap (f x y)


func_treeFold :: Tree' (Wrap Integer) -> Wrap Integer
func_treeFold t = Solution.treeFold (wrap2 f) t
  where
    -- Some random nonassociative noncommutative function
    f :: Integer -> Integer -> Integer
    f x y = x + 2 * y

func_seqAppend :: (Sequence Integer, Sequence Integer) -> Sequence Integer
func_seqAppend (xs, ys) = (xs `sequenceAppend` ys)


