module Exercise2 where

data Expr = Constant Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Eq, Show)

exprShow :: Expr -> String
exprShow (Constant n) = show n
exprShow (Add e1 e2)  = "(" ++ s1 ++ " + " ++ s2 ++ ")"
  where
    s1 = exprShow e1
    s2 = exprShow e2
exprShow (Sub e1 e2)  = "(" ++ exprShow e1 ++ " - " ++ exprShow e2 ++ ")"
exprShow (Mul e1 e2)  = "(" ++ exprShow e1 ++ " * " ++ exprShow e2 ++ ")"
exprShow (Div e1 e2)  = "(" ++ exprShow e1 ++ " / " ++ exprShow e2 ++ ")"

-- 2 * 4 + 5
eExample :: Expr
eExample = Add (Mul (Constant 2) (Constant 4)) (Constant 5)

eExample2 :: Expr
eExample2 = Add (Constant 3) (Add (Constant 5) (Constant 6))

eval :: Expr -> Int
eval (Constant n) = n
eval (Add e1 e2)  = eval e1 + eval e2
eval (Sub e1 e2)  = eval e1 - eval e2
eval (Mul e1 e2)  = eval e1 * eval e2
eval (Div e1 e2)  = eval e1 `div` eval e2

-- Exercise. Complete the following definition so that when division
-- by zero occurs within a subexpression, the result of the evaluation
-- of the expression is Error. 10 points.

data Result = Value Int | Error
     deriving (Eq, Show)


eval' :: Expr -> Result
eval' (Constant n) = Value n
eval' (Add e1 e2)  = add (eval' e1) (eval' e2)
eval' (Sub e1 e2)  = sub (eval' e1) (eval' e2)
eval' (Mul e1 e2)  = mul (eval' e1) (eval' e2)
eval' (Div e1 e2)  = div' (eval' e1) (eval' e2)

add :: Result -> Result -> Result
add _ Error = Error
add Error _ = Error
add (Value m) (Value n) = Value (m+n)

sub :: Result -> Result -> Result
sub  _ Error = Error
sub Error _ = Error
sub (Value m) (Value n) = Value (m-n)

mul :: Result -> Result -> Result
mul  _ Error = Error
mul Error _ = Error
mul (Value m) (Value n) = Value (m*n)

div' :: Result -> Result -> Result
div'  _ Error = Error
div' Error _ = Error
div' (Value _) (Value 0) = Error
div' (Value m) (Value n) = Value (m `div` n)

expr1 :: Expr
expr1 = Add (Constant 1) (Div (Constant 1) (Constant 0))

-- Note. Result type is isomorphics to Maybe Int type. As we will see later,
-- Maybe will provide a convenient type for working with fault-sensitive
-- algorithms.

-- Bonus Exercise. Define the following function as "show" above so that the
-- minimal amounts of needed brackets are used. 10 bonus points.
--
-- Define the following function as "show" above so that the minimal amounts
-- of needed brackets are used. That is: you should not distinguish between a
-- + (b + c) and (a + b) + c, but you should use parentheses to distinguish
-- between a * b + c and a * (b + c).
--
cleverShow :: Expr -> String
cleverShow (Constant n) = show n

cleverShow (Add e1 e2) = cleverShow e1 ++ " + " ++ cleverShow e2

cleverShow (Sub e1 e2) = case needBracketMorDl e2 of
                              True -> cleverShow e1 ++ " - " ++ "(" ++ cleverShow e2 ++ ")"
                              False -> cleverShow e1 ++ " - " ++ cleverShow e2

cleverShow (Mul e1 e2) = case (needBracketMorDl e1, needBracketMorDl e2) of 
                               (True, True) -> "(" ++ cleverShow e1 ++ ")" ++ " * " ++ "(" ++ cleverShow e2 ++ ")"
                               (True, False) -> "(" ++ cleverShow e1 ++ ")" ++ " * " ++ cleverShow e2
                               (False, True) -> cleverShow e1 ++ " * " ++ "(" ++ cleverShow e2 ++ ")"
                               (False, False) -> cleverShow e1 ++ " * " ++ cleverShow e2

cleverShow (Div e1 e2) = case (needBracketMorDl e1, needBracketD e2) of
                                 (True, True) -> "(" ++ cleverShow e1 ++ ")" ++ " / " ++ "(" ++ cleverShow e2 ++ ")"
                                 (True, False) -> "(" ++ cleverShow e1 ++ ")" ++ " / " ++ cleverShow e2
                                 (False, True) -> cleverShow e1  ++ " / " ++ "(" ++ cleverShow e2 ++ ")"
                                 (False, False) -> cleverShow e1 ++ " / " ++ cleverShow e2

--Output: "(1 * 4 * -5 * -5 - (-6 - 5) * -4 * -4) / (3 / (-9 / -9) / ((0 + 2) / (3))) = Error"
--Expected output: "(1 * 4 * -5 * -5 - (-6 - 5) * -4 * -4) / (3 / (-9 / -9) / ((0 + 2) / 3)) = Error"
-- 3 * 4 / 6 * 3
--(-6 * -3) / -6 / 8 
--  -6 * -3 / (-6 / 8)

needBracketMorDl :: Expr -> Bool
needBracketMorDl (Add _ _) = True
needBracketMorDl (Sub _ _) = True
needBracketMorDl _ = False

needBracketD :: Expr -> Bool
needBracketD (Add _ _) = True
needBracketD (Sub _ _) = True
needBracketD (Mul _ _) = True
needBracketD (Div _ _) = True
needBracketD _ = False

needBracketSr :: Expr -> Bool
needBracketSr (Sub _ _) = True
needBracketSr (Add _ _) = True
needBracketSr _ = False

{-
 -needBracketD :: Expr -> Bool
 -needBracketD
 -needBracketD
 -
 -}
-- (2 * 8) / (4 / 8)
-- 2 * 8 - 4 * 8

-- use clevershow to define a function which for an expression outputs "e = v"
-- where E is just cleverly shown the original expression expression and V is
-- the result of evaluation. In case it is Error, V should be just "Error".

show' :: Result -> String
show' (Value x) = show x
show' Error = "Error"

showWithOutput :: Expr -> String
showWithOutput e = case eval' e of Error -> cleverShow e ++ " = " ++ show' Error 
                                   (Value x) -> cleverShow e ++ " = " ++ show x

-- We can define our own type of sequences:

data Sequence a = Empty | Cons a (Sequence a)
     deriving (Eq, Show)

-- Exercise. Show that this type is isomorphic to [a], by
-- defining the following functions. 10 points.

toNormal :: Sequence a -> [a]
toNormal Empty = []
toNormal (Cons x xs) = x : toNormal xs

fromNormal :: [a] -> Sequence a
fromNormal = foldr Cons Empty

mySeq :: Sequence Int
mySeq = Cons 3 (Cons 4 (Cons 5 Empty))

mySeq2 :: Sequence Int
mySeq2 = Cons 6 (Cons 7 (Cons 8 Empty))

-- Exercise. Define a sequenceMap function satisfying
--
-- sequenceMap f (Cons x0 (Cons x1 (Cons x2 ... Empty)))
--       = (Cons (f x0) (Cons (f x1) (Cons (f x2) ... Empty)))
--
-- E.g. sequenceMap odd (cons 3 (cons 4 (cons 5 empty))) = cons true (cons false (cons true empty))
-- 10 points.

sequenceMap :: (a -> b) -> Sequence a -> Sequence b
sequenceMap _ Empty = Empty
sequenceMap f (Cons x xs) = Cons (f x) (sequenceMap f xs)

-- Exercise. Define a function which for two sequences computes their
-- concatenation. 10 points.
sequenceAppend :: Sequence a -> Sequence a -> Sequence a
sequenceAppend Empty Empty = Empty
sequenceAppend Empty b = b
sequenceAppend a Empty = a
sequenceAppend (Cons x Empty) b = Cons x b
sequenceAppend (Cons x xs) b = Cons x $ sequenceAppend xs b

-- Exercise. Define a function which appends sequence of sequences together.
-- 10 points.
--
-- For example, for the sequence
--   (Cons (Cons 1 (Cons 2 Empty)) (Cons (Cons 3 (Cons 4 Empty)) Empty))
-- return
--   (Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty))))

sequenceFlatten :: Sequence (Sequence a) -> Sequence a
sequenceFlatten Empty = Empty
sequenceFlatten (Cons Empty xs) = sequenceFlatten xs
sequenceFlatten (Cons x xs) = sequenceAppend x (sequenceFlatten xs)

-- Exercise. Define a function which splits a sequence into two halfs of equal
-- length, or with the first half one element longer if the length of the
-- sequence is odd. 10 points.
--
sequenceSplit :: Sequence a -> (Sequence a, Sequence a)
sequenceSplit Empty = (Empty, Empty)
sequenceSplit z = let len = sequenceLength z 
                      half = len `div` 2 
                      in
                        case len `mod` 2 of 0 -> (sequenceTake z half, sequenceDrop z half)
                                            _ -> (sequenceTake z $ half + 1, sequenceDrop z $ half + 1)

sequenceTake :: Sequence a -> Int -> Sequence a
sequenceTake Empty _ = Empty
sequenceTake _ 0 = Empty
sequenceTake (Cons x xs) n = Cons x $ sequenceTake xs $ n-1

sequenceDrop :: Sequence a -> Int -> Sequence a
sequenceDrop Empty _ = Empty
sequenceDrop x 0 = x
sequenceDrop (Cons _ xs) n = sequenceDrop xs $ n-1

sequenceLength :: Sequence a -> Int
sequenceLength Empty = 0
sequenceLength (Cons _ xs) = 1 + sequenceLength xs

-- Now consider trees.

data Tree  a = Leaf    | Fork  a (Tree a)  (Tree a)  deriving (Show, Eq)
data Tree' a = Leaf' a | Fork'   (Tree' a) (Tree' a) deriving (Show, Eq)

-- Exercise. Write treeMap which applies a function from the argument to
-- every element in the Tree. 10 points:
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Leaf = Leaf
treeMap f (Fork x l r) = Fork (f x) (treeMap f l) (treeMap f r)

-- Exercise. The same as above but for Tree'.
-- 10 points:
treeMap' :: (a -> b) -> Tree' a -> Tree' b
treeMap' f (Leaf' a) = Leaf' $ f a
treeMap' f (Fork' l r) = Fork' (treeMap' f l) (treeMap' f r)


-- Bonus Exercise, 10 points. Write a function

treeFold :: (b -> b -> b) -> Tree' b -> b
treeFold _ (Leaf' x) = x
treeFold f (Fork' l r) = f (treeFold f l) (treeFold f r)

-- and show that it can be used to define a flattening function which returns a
-- list of elements in the leafs of the tree in order given by Depth First
-- Search algorithm.
--

myTree :: Tree' Integer
myTree = Fork' (Fork' (Leaf' 1) (Leaf' 2)) (Leaf' 3)

-- For example for the tree
--        .
--       / \
--      /   3
--     / \
--    1   2
-- it should return [1,2,3].
--
-- Hint: remember your lectures in Foundations 2 about tree traversals
-- (in-order, post-order, depth-first etc.)


treeFlatten :: Tree' a -> [a]
treeFlatten = treeFold (++) . treeMap' (:[]) 

