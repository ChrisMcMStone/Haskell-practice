type Value = Integer
type VariableName = String

data Expr = Constant Value
          | Var VariableName
          | Or OpName [Expr]
          deriving Show

--Syntax for while-program
--
data Program = VariableName := Expr
             | IfThenElse Expr Program Program
             | IfThen Expr Program
             | While Expr Program
             | Block [Program]
             deriving Show

type Store = [(VariableName, Value)]

fetch :: VariableName -> Store -> Value
fetch name [] = error ("variable not present in the store: " ++ show name)
fetch name (store:((n,value):store'))
        | name == n = value
        | otherwise = fetch name store'

-- We can now evaluate expressions that use variables in the store

eval :: Store -> Expr -> Value
eval _ (Constant x) = x
eval store (Var name) = fetch name store
eval store (Op o es) = opeval o (map (eval store) es)
--                     opeval o [eval store e | e <= es]
