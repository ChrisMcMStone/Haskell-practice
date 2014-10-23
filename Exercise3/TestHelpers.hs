module TestHelpers where

import Prelude hiding (catch)
import Control.Applicative
import Control.Exception
import Control.Monad hiding (replicateM)
import Data.List
import qualified Control.Monad
import Test.QuickCheck

import safe Exercise3(OpName(..), Expr(..), Program(..), Store, Store', VariableName, runHelper, runHelper', factorials, Value, fetch, update, OutputStream, convertStore)
import safe qualified Exercise3 as Solution

-- Some things we want to test

run_factorial n = take n facts1 == take n facts2
  where
    facts1 = extractStream (runHelper factorials [])
    extractStream (state, stream) = stream
    facts2 = [product [1..n] | n <- [1..]]

j = "non-existent variable "

randomStoreWithExpr :: Gen (Store, Expr)
randomStoreWithExpr = do
    s <- randomStore vs
    e <- exprWithDomain False vs
    return (s, e)

randomStoreWithVar :: Gen (Store, VariableName)
randomStoreWithVar = do
    s <- randomStore vs
    v <- elements vs
    return (s, v)

randomProgramWithStore :: Gen (Program, Store)
randomProgramWithStore = do
    s <- randomStore vs
    p <- (initialiseProgram ["x", "y"]
              <$> terminatingProgram ["x", "y"] (vs++lvs))
    return (p, s)

func_eval_expression :: (Store, Expr) -> Value
func_eval_expression (s, e) = Solution.eval s e

prop_fetch_update_1 :: String -> Integer -> Property
prop_fetch_update_1 x n = forAll (randomStore (x:vs)) $ \s ->
        fetch x (Solution.update x n s) == n

prop_fetch_update_2 :: String -> Integer -> Integer -> Property
prop_fetch_update_2 x n n' =
  forAll (randomStore vs) $ \s ->
        fetch x (Solution.update x n (Solution.update x n' s)) == n

prop_fetch_update_3 :: String -> String -> Property
prop_fetch_update_3 x y =
  forAll (randomStore (x:y:vs)) $ \s ->
        (fetch x (Solution.update y (fetch y s) s)
                == fetch x s)

prop_no_duplicate_variable_names :: String -> Property
prop_no_duplicate_variable_names x = 
  forAll (randomStore (x:vs)) $ \s -> do
    let s' = update x 3 s
    noDuplicates s'

noDuplicates :: Eq a => [a] -> Bool
noDuplicates l = l == nub l

prop_fetch'update'1 :: String -> Integer -> Property
prop_fetch'update'1 x n = forAll (randomStore (x:vs)) $ \s ->
        fetch' x (Solution.update' x n $ c s) == n

prop_fetch'update'2 :: String -> Integer -> Integer -> Property
prop_fetch'update'2 x n n' =
  forAll (randomStore vs) $ \s ->
        fetch' x (Solution.update' x n (Solution.update' x n' $ c s)) == n

prop_fetch'update'3 :: String -> String -> Property
prop_fetch'update'3 x y =
  forAll (randomStore (x:y:vs)) $ \s ->
        (fetch' x (Solution.update' y (fetch' y $ c s) $ c s)
                == fetch' x (c s))

func_convertStore :: (Store, VariableName) -> Value
func_convertStore (s, v) = s' v
  where
    s' = convertStore s



-- Some example programs

translation_skip10 :: () -> Program
translation_skip10 = const skip10
translation_impSum :: () -> Program
translation_impSum = const impSum
translation_impSqSum :: () -> Program
translation_impSqSum = const impSqSum
translation_fibonacci :: () -> Program
translation_fibonacci = const fibonacci

-- skip 10 times
{-
  for (n := 0; n <= x; n = n + 1)
  {
    skip;
  }

-}
skip10 :: Program
skip10 =
  Block
    [
      "y" := Constant 27,
      Solution.for ("n" := Constant 0)
             (Op Leq [Var "n", Var "x"])
             ("n" := Op Add [Var "n", Constant 1])
             Solution.skip
    ]

-- imperative sum function
{-
  {
    y := 0;
    for (n := 0; n <= x; n = n + 1)
    {
      y := n + y
    }
  }
-}
impSum :: Program
impSum =
  Block
    [
      "y" := Constant 0,
       Solution.for ("n" := Constant 0)
                    (Op Leq [Var "n", Var "x"])
                    ("n" := Op Add [Var "n", Constant 1])
                    ("y" := Op Add [Var "y", Var "n"])
    ]

-- imperative square sum
{-
  {
    y := 0;
    for (n := 0; n == x; n = n + 1)
    {
      y := n * n + y
    }
  }
-}

impSqSum :: Program
impSqSum =
  Block
    [
      "y" := Constant 0,
      Solution.for ("n" := Constant 0)
                   (Op Leq [Var "n", Var "x"])
                   ("n" := Op Add [Var "n", Constant 1])
                   ("y" := Op Add [Op Mul [Var "n", Var "n"], Var "y"])
    ]

-- imperative fibonacci number

fibonacci :: Program
fibonacci =
  Block
   [
     "a" := Constant 0,
     "b" := Constant 1,
     Solution.for ("n" := Var "x")
                  (Op Greater [Var "n", Constant 0])
                  ("n" := Op Sub [Var "n", Constant 1])
       (Block
        [
          "c" := Op Add [Var "a" , Var "b"],
          "a" := Var "b",
          "b" := Var "c"
        ]),
     "y" := Var "a"
   ]


-- Helper functions for the test bench.

replicateM :: Monad m => Integer -> m a -> m [a]
replicateM = Control.Monad.replicateM . fromIntegral

func_cond :: [Integer] -> Integer
func_cond = Solution.opeval Cond

instance Arbitrary OpName where
  arbitrary = elements
    [Cond, Add, Sub, Mul, Neg, Eq, Leq, Less, Geq, Greater, And, Or, Not]

arity :: OpName -> Integer
arity op = case op of
  Cond -> 3
  Neg  -> 1
  Not  -> 1
  _    -> 2

instance Arbitrary Expr where
  arbitrary = exprWithDomain False vs
  shrink (Constant _) = []
  shrink (Var _) = []
  shrink (Op _ args) = concat $ shrink args

exprWithDomain :: Bool -> [VariableName] -> Gen Expr
exprWithDomain allowCond vars = sized go
    where
      go n = frequency
        [(1, Constant <$> arbitrary)
        ,(1, Var <$> elements vars)
        ,(n, do
          op <- (arbitrary `suchThat` opAcceptable)
          args <- replicateM (arity op) (go (n `div` 2))
          return $ Op op args)
        ]
      opAcceptable op = allowCond || op /= Cond



removeSkips :: Program -> Program
removeSkips (i := e) = i := e
removeSkips (Print e) = Print e
removeSkips (IfThenElse e p p') = IfThenElse e (removeSkips p) (removeSkips p')
removeSkips (While e p) = While e $ removeSkips p
removeSkips (Block ps) = Block $ filter (not . isSkip) ps
  where
    isSkip (Block []) = True
    isSkip _          = False

instance Arbitrary Program where
  arbitrary = terminatingProgram vs lvs
  shrink (v := e) = map (v :=) $ shrink e
  shrink (IfThenElse e p q) = shrink p ++ shrink q
      ++ [IfThenElse e' p' q' | e' <- shrink e, p' <- shrink p, q' <- shrink q]
  shrink (While e p) = p : [While e' p' | e' <- shrink e, p' <- shrink p]
  shrink (Block ps) = ps ++ [Block ps' | ps' <- shrink ps]

terminatingProgram :: [VariableName] -> [VariableName] -> Gen Program
terminatingProgram vars loopVars = sized (go vars)
  -- Precondition: vars and loopVars are disjoint
  where
    go xs n = frequency
      [(1, (:=) <$> elements vars <*> exprWithDomain False xs)
      ,(1, Print <$> exprWithDomain False xs)
      ,(n, IfThenElse <$> exprWithDomain False xs <*> rec <*> rec)
      ,(n, do
        v    <- elements loopVars
        body <- go (v:xs) 0
        iterations <- (min 10 . abs) <$> arbitrary
        return $ Block [ v := Constant iterations
                       , While (Var v) $ Block
                         [ body
                         , v := Op Sub [Var v, Constant 1]
                         ]
                       ]
       )
      ,(n, Block <$> vectorOf (min 5 n) rec)
      ]
      where
        rec = go xs (n `div` 2)

-- Random program without While.
boringProgram :: [VariableName] -> Gen Program
boringProgram vars = sized (go vars)
  where
    go xs n = frequency
      [(1, (:=) <$> elements vars <*> exprWithDomain False xs)
      ,(1, Print <$> exprWithDomain False xs)
      ,(n, IfThenElse <$> exprWithDomain False xs <*> rec <*> rec)
      ,(n, Block <$> vectorOf (min 5 n) rec)
      ]
      where
        rec = go xs (n `div` 2)

initialiseProgram :: [VariableName] -> Program -> Program
initialiseProgram xs p = Block $ [x := Constant 1 | x <- xs] ++ [p]

randomStore :: [VariableName] -> Gen Store
randomStore vs = mapM (\x -> (,) x <$> arbitrary) vs >>= randomRotate

randomStore' :: [VariableName] -> Gen Store'
randomStore' = fmap c . randomStore

randomRotate :: [a] -> Gen [a]
randomRotate xs = do
    n <- elements [1..(length xs)]
    return (take (length xs) $ drop n $ cycle xs)

fetch' :: VariableName -> Store' -> Value
fetch' l s = s l

c = foldr (uncurry $ \d e f g -> if (length d - 2 * (length g) == - length d) && (d++g == g ++ d) then e else f g) (\h -> error $ j ++ h)

vs, lvs :: [VariableName]
vs  = map (:"") ['a'..'z']
lvs = map (:"l") ['a'..'z']

programGen :: Gen Program
programGen = (initialiseProgram ["x", "y"]
              <$> terminatingProgram ["x", "y"] lvs)

boringProgramGen :: Gen Program
boringProgramGen = (initialiseProgram ["x", "y"]
              <$> boringProgram ["x", "y"])

prop_eval_matches_eval' :: Property
prop_eval_matches_eval' = forAll (randomStore vs) $ \m ->
    forAll (exprWithDomain False vs) $ \e ->
    Solution.eval m e == Solution.eval' (c m) e

prop_run_matches_run' :: Property
prop_run_matches_run' = forAll programGen $ \p ->
    Solution.run p == Solution.run' p

prop_constantFolding_does_not_change_meaning :: Property
prop_constantFolding_does_not_change_meaning = forAll boringProgramGen $ \p ->
    Solution.run p == Solution.run (Solution.constantFolding p)

func_constantFolding_folds_enough :: Program -> Program
func_constantFolding_folds_enough p =
    removeSkips (Solution.constantFolding p)

func_runHelper :: (Program, Store) -> (Store, OutputStream)
func_runHelper (p, s) = runHelper p s

func_runHelper' :: (Program, Store) -> (OutputStream)
func_runHelper' (p, s) = snd $ runHelper' p $c s

dummyCheck :: Bool
dummyCheck = True
