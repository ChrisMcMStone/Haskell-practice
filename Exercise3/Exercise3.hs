module Exercise3 where

{-

Exercise set 3, modified from interpreter1.hs

Martin Escardo 2014 for the module Functional Programming at SoCS UoB UK.

 A functional interpreter for a minimal imperative language.

   Arithmetical operators +, -, *, /
   Comparisons =, <, <=, >, >=
   Logical operators and, or not.
   Assignment statements variable := expression
   If-then-else statements
   If-then statements
   While statements
   Block statements

We don't have print or read statements in this simple version and we are about
to fix it (only for print this time).

This language can be seen as a small subset of Java or C.

We have integers variables only, we ignore errors such as undefined
variables and division by zero, and we work with abstract syntax (the
types OpName, Expr and Program defined below).

Like in the programming language C, true is interpreted as non-zero,
and everything else, including zero, codes false.

-}

type Value = Integer
type VariableName = String

{-

Exercise (10 points).
--------------------

We have added a ternary operator like the Java or C conditional
expression e1?e2:e3, here written Op Cond [e1, e2, e3].  But we didn't implement this in our interpreter. Add a suitable definition (where?).

-}

-- TERNARY example:

ternaryTest :: Program
ternaryTest = Block ["a" := Constant 345,"b" := Constant 678, "z" := Constant 10, "y" := Op Cond [Op Less [Var "x", Var "z"], Var "a", Var "b"]]


data OpName = Cond -- We are adding this new ternary operator for you to implement.
            | Add
            | Sub
            | Mul
            | Div
            | Neg
            | Eq
            | Leq
            | Less
            | Geq
            | Greater
            | And
            | Or
            | Not
            deriving (Eq, Show)

data Expr = Constant Value
          | Var VariableName
          | Op OpName [Expr]
          deriving (Eq, Show)

data Program = VariableName := Expr
             | IfThenElse Expr Program Program
--           | IfThen Expr Program              -- we are removing this
             | While Expr Program
             | Block [Program]
             | Print Expr                       -- we are adding this
             deriving (Eq, Show)

{-

Prelude to the next exercise. IfThen is not really necessary:

-}

skip :: Program
skip = Block []    -- does nothing.

ifThen :: Expr -> Program -> Program
ifThen e p = IfThenElse e p skip

{-

Exercise (0 points). 
-------------------

It was originally 10 points, but we accidentally gave away a sample
solution below.

Similar to the above, define a for-loop program constructor by
reduction to while-loops.

-}

for :: Program -> Expr -> Program -> Program -> Program
for init test update body = Block [init, While test (Block [body, update])]

-- -- another definition, using $ instead of lots of brackets: 
-- for init test update body = Block $ [init, While test $ Block [body,update]]

{-

    In concrete syntax, the program

            for (init; test; update)
                body;

    should be equivalent to the program

            init;
            while (test)
              {
                body;
                update;
              }
-}

{-

Example. A program for printing the infinite list of factorials:

  {
   y := 1;
   for (n := 1; 1; n := n + 1) {
    y := y * n;
    print y;
   }
  }

Exercise (10 points).
--------------------

Write this program in abstract syntax (using your for function):

-}

factorials :: Program
factorials = Block ["y" := Constant 1, for ("n" := Constant 1) (Constant 1) ("n" := Op Add [Var "n", Constant 1]) (Block ["y" := Op Mul [Var "y", Var "n"], Print (Var "y")])]

{-

Now, we will define two helper functions for converting Values to Bools
and back.

-}

code :: Bool -> Value
code False = 0
code True  = 1

decode :: Value -> Bool
decode 0 = False
decode _ = True

{-

Next, given an operator name and a list of integers, we apply the
corresponding operator to the list. In an impossible situation, we
give an error.

Exercise (10 points). Finish the definition of the following function.
--------------------

-}

opeval :: OpName -> [Value] -> Value
opeval Cond    [c, rt, rf] = if decode c then rt else rf
opeval Add     [x, y] = x + y
opeval Sub     [x, y] = x - y
opeval Mul     [x, y] = x * y
opeval Div     [x, y] = x `div` y
opeval Neg     [x]    = - x
opeval Eq      [x, y] = code(x == y)
opeval Leq     [x, y] = code(x <= y)
opeval Less    [x, y] = code(x < y)
opeval Geq     [x, y] = code(x >= y)
opeval Greater [x, y] = code(x > y)
opeval And     [x, y] = code(decode x && decode y)
opeval Or      [x, y] = code(decode x || decode y)
opeval Not     [x]    = code(not $ decode x)
opeval op      xs     = error ("Tried to apply " ++ show op ++ " to " ++ show xs)

{-

To evaluate a given expression, we need to know the values of the
variables, which are stored in the "store", which is represented as a
list of pairs recording the value of each variable.

            Remark. For a store, in order to save memory, the
            following invariant should hold:

            No variable name occurs twice in the store.

            We will check this when marking (and in the test bench). 

-}

type Store = [(VariableName,Value)]

{-

Example. For the following sequence of assignments

            x := 4
            y := 4
            z := 5
            x := 3

we get the following Store (the order is not important):

-}

sExample :: Store
sExample = [("x", 3), ("y", 4), ("z", 5)]

{-

The two things we can do with Store are

  (1) fetch the value of a variable from a store.

  (2) Update the value of a variable in a store.

When we try to fetch and there is no value assigned to a variable, it
is OK to just fail (we are not requiring error handling in this set of
exercises).

When we update, we create a new modified Store. In the following
function, given m :: Store, we get a new Store m' :: Store, and the
only difference is that if (i, x') is in the store, it should be
replaced by (i, x); otherwise m' is just m with (i, x) added.


Exercise (10 points). Write fetch and update functions as explained above.
--------------------

Hint: Keep in mind that you should not violate the invariant.  If you
      don't preserve the invariant, you will receive half of the points.

-}

fetch :: VariableName -> Store -> Value
fetch j ((k, v) : ps)   | j == k = v
                        | otherwise = fetch j ps

update :: VariableName -> Value -> Store -> Store
update i x [] = [(i, x)]
update i x ((k, v) : ps) | i == k = newStore
                         | otherwise = (k, v) : update i x ps
                         where
                                 newStore = (i, x) : ps
{-

sExample :: Store
sExample = [("x", 3), ("y", 4), ("z", 5)]


Once we have that, we can write eval function which fetches values of
variables from a given Store.

-}

eval :: Store -> Expr ->  Value
eval m (Constant x) = x
eval m (Var i)      = fetch i m
eval m (Op o es)    = opeval o [eval m e | e <- es]

{-

We now extend the interpreter to be able to print. For this purpose,
we use the type OutputStream, defined below, to record the (possibly
infinite) list of outputs.

Notice that when we add print in this way, not only do we have to add
more code to interpret the command print, but also we need to modify
the interpretation of all other commands to suitably propagate the
OutputStream.

Exercise (10 points).
--------------------

Complete the following definition, where only "while" is missing.

Hint. Study our given implementation of blocks, and take inspiration
from that to define "while".

-}

type OutputStream = [Value]

run :: Program -> OutputStream
run p = snd (runHelper p [])

runHelper :: Program -> Store -> (Store , OutputStream)

runHelper (Print e) m = (m , [eval m e])

runHelper (i := e) m = (update i (eval m e) m, [])

runHelper (IfThenElse e p q) m
    | decode (eval m e) = runHelper p m
    | otherwise         = runHelper q m

--  runHelper (IfThen e p) m
--      | decode (eval m e) = runHelper p m
--      | otherwise         = (m , [])

runHelper (While e p) m 
        | decode(eval m e) = (m'', xs ++ ys)
        | otherwise = (m, [])
        where
                (m', xs) = runHelper p m
                (m'', ys) = runHelper (While e p) m'

runHelper (Block []) m = (m, [])

runHelper (Block (p : ps)) m = (m'', xs ++ ys)
    where
      (m' , xs) = runHelper p m
      (m'', ys) = runHelper (Block ps) m'

{-

Example, continued from the above:

-}

fact :: OutputStream
fact = run factorials


{-

There is another way to represent the store. This time it is simply
a function that given a variable name returns its value:

-}

type Store' = VariableName -> Value

{-

Exercise (10 points).
--------------------

Fill the following undefined's for this new
definition of Store' (there are 2 undefined's)

-}

emptyStore' :: Store'
emptyStore' i = error("variable not present in the store: " ++ show i)

sExample' :: Store'
sExample' "x" = 3
sExample' "y" = 4
sExample' "z" = 5
sExample' i   = error("variable not present in the store: " ++ show i)

eval' :: Store' -> Expr -> Value
eval' m (Constant x) = x
eval' m (Var l)      = m l      -- fetch variable l from Store'
eval' m (Op o es)    = opeval o [eval' m e | e <- es]

{-

When we update', we change Store'. In the following function, given
m :: Store', we get a new Store' m' :: Store', with the only difference
is that m' i = x, no matter what m i was.

-}

update' :: VariableName -> Value -> Store' -> Store'
update' newKey newVal oldStore = newStore
        where
                newStore :: Store'
                newStore key | key == newKey = newVal
                             | otherwise = oldStore key

sExampleBis :: Store'
sExampleBis = update' "z" 5 (update' "y" 4 (update' "x" 3 emptyStore'))

{-

Notice that any Store can be converted to a Store'.

Exercise (10 points). Write such a conversion.
--------------------

-}

convertStore :: Store -> Store'
convertStore = foldr (\(k,v) acc -> update' k v acc) emptyStore'

{-

Exercise (0 points).
-------------------

What is the difficulty writing a conversion back? Convince yourself
that it's not possible to do it.

-}

{-

With this representation of store, if we want to run a program and
again get the resulting Store' as an output, we can then simply call
the previous function for Store and convert the result.

-}

runAndKeep :: Program -> (Store', OutputStream)
runAndKeep p = case runHelper p [] of
                (m, outStream) -> (convertStore m, outStream)

{-

But, since we know that it's not possible to convert Store' back to
Store, we can't hope that we can write runHelper' just by calling
runHelper with appropriate converted arguments (as in runKeep).

Exercise (10 points).
--------------------

Define runHelper' using only the functions update' and eval' as before.

-}

run' :: Program -> OutputStream
run' p = snd (runHelper' p emptyStore')

runHelper' :: Program -> Store' -> (Store' , OutputStream)

runHelper' (Print e) m = (m , [eval' m e])

runHelper' (i := e) m = (update' i (eval' m e) m, [])

runHelper' (IfThenElse e p q) m
    | decode (eval' m e) = runHelper' p m
    | otherwise         = runHelper' q m

--  runHelper' (IfThen e p) m
--      | decode (eval' m e) = runHelper' p m
--      | otherwise         = (m , [])

runHelper' (While e p) m 
        | decode(eval' m e) = (m'', xs ++ ys)
        | otherwise = (m, [])
        where
                (m', xs) = runHelper' p m
                (m'', ys) = runHelper' (While e p) m'

runHelper' (Block []) m = (m, [])

runHelper' (Block (p : ps)) m = (m'', xs ++ ys)
    where
      (m' , xs) = runHelper' p m
      (m'', ys) = runHelper' (Block ps) m'



{-

Simple program optimization. Say that an expression is constant if it
doesn't use variables. We can replace constant subexpresions by their
values, as follows:

-}

replaceConstantSubexpressions :: Expr ->  Expr
replaceConstantSubexpressions (Constant x) = Constant x
replaceConstantSubexpressions (Var i)      = Var i
replaceConstantSubexpressions (Op o es)
               | all isConstant vs = Constant(opeval o [n |  Constant n <- vs])
               | otherwise         = Op o vs
               where
                 vs = [replaceConstantSubexpressions e | e <- es]

isConstant :: Expr -> Bool
isConstant (Constant _) = True
isConstant _ = False

{-

Exercise (15 points). Hard. Similarly, if an IfThenElse program has
its condition constant, it can be replaced by a simpler program (one
of the two branches). The same applies to while loops (when the
condition is false). Moreover, we can apply the previous optimization
to every subexpression in a program.

-}

constantFolding :: Program -> Program

constantFolding (Print exp) = Print (replaceConstantSubexpressions exp)

constantFolding (name := exp) = name := replaceConstantSubexpressions exp

constantFolding (Block ps) = Block (map constantFolding ps)

constantFolding (IfThenElse e p1 p2) = case replaceConstantSubexpressions e of
                                               Constant 0 -> constantFolding p2
                                               Constant a -> constantFolding p1
                                               n -> IfThenElse n (constantFolding p1) (constantFolding p2)

constantFolding (While e p) = case replaceConstantSubexpressions e of
                                      Constant 0 -> Block []
                                      n -> While n $ constantFolding p

{-
 -VariableName := Expr
 -             | IfThenElse Expr Program Program
 -             | While Expr Program
 -             | Block [Program]
 -             | Print Expr 
 -}

{-

Challenge Exercise (0 points!). Extend Program type with appropriate
constructors for defining functions and handling exceptions and
modify run function accordingly.

Hint: You'll maybe need to modify more than just the Program type and
the run function.

-}
