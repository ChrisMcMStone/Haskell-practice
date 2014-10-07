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

intValue :: Result -> Int
intValue (Value n) = n

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
div' (Value m) (Value 0) = Error
div' (Value m) (Value n) = Value (m `div` n)

expr1 :: Expr
expr1 = (Add (Constant 1) (Div (Constant 1) (Constant 0)))

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

cleverShow :: Expr -> String
cleverShow = undefined

-- Use cleverShow to define a function which for an expression outputs "E = V"
-- where E is just cleverly shown the original expression expression and V is
-- the result of evaluation. In case it is Error, V should be just "Error".
showWithOutput :: Expr -> String
showWithOutput = undefined

-- We can define our own type of sequences:

data Sequence a = Empty | Cons a (Sequence a)
     deriving (Eq, Show)

-- Exercise. Show that this type is isomorphic to [a], by
-- defining the following functions. 10 points.

toNormal :: Sequence a -> [a]
toNormal = undefined

fromNormal :: [a] -> Sequence a
fromNormal = undefined

-- Exercise. Define a sequenceMap function satisfying
--
-- sequenceMap f (Cons x0 (Cons x1 (Cons x2 ... Empty)))
--       = (Cons (f x0) (Cons (f x1) (Cons (f x2) ... Empty)))
--
-- E.g. sequenceMap odd (Cons 3 (Cons 4 (Cons 5 Empty))) = Cons True (Cons False (Cons True Empty))
-- 10 points.

sequenceMap :: (a -> b) -> Sequence a -> Sequence b
sequenceMap = undefined

-- Exercise. Define a function which for two sequences computes their
-- concatenation. 10 points.
sequenceAppend :: Sequence a -> Sequence a -> Sequence a
sequenceAppend = undefined

-- Exercise. Define a function which appends sequence of sequences together.
-- 10 points.
--
-- For example, for the sequence
--   Cons (Cons 1 (Cons 2 Empty)) (Cons (Cons 3 (Cons 4 Empty)) Empty)
-- return
--   Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty)))
sequenceFlatten :: Sequence (Sequence a) -> Sequence a
sequenceFlatten = undefined

-- Exercise. Define a function which splits a sequence into two halfs of equal
-- length, or with the first half one element longer if the length of the
-- sequence is odd. 10 points.
sequenceSplit :: Sequence a -> (Sequence a, Sequence a)
sequenceSplit = undefined

-- Now consider trees.

data Tree  a = Leaf    | Fork  a (Tree a)  (Tree a)  deriving (Show, Eq)
data Tree' a = Leaf' a | Fork'   (Tree' a) (Tree' a) deriving (Show, Eq)

-- Exercise. Write treeMap which applies a function from the argument to
-- every element in the Tree. 10 points:
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap = undefined

-- Exercise. The same as above but for Tree'.
-- 10 points:
treeMap' :: (a -> b) -> Tree' a -> Tree' b
treeMap' = undefined


-- Bonus Exercise, 10 points. Write a function

treeFold :: (b -> b -> b) -> Tree' b -> b
treeFold f = undefined

-- and show that it can be used to define a flattening function which returns a
-- list of elements in the leafs of the tree in order given by Depth First
-- Search algorithm.
--
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
treeFlatten t = undefined
