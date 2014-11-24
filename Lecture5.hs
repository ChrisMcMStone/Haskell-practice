{-
 -MORE PARSING
 -
 -<Expr> ::= number
 -        | <Expr> + <Expr>
 -        | <Expr> * <Expr>
 -
 -
 -5 + 6 * 7
 -
 -TO DEFINE A MONAD
 -
 -"return" -----> list monad: 
 -                        return x =[x]
 -                maybe monad:
 -                        return x = Just x
 -
 -"bind"
 -
 -}




eval4 :: Expr -> Value

eval4 (Constant n) = return n

eval4 (Add e d) = do x <- eval4 e
                   y <- eval4 d
                   return(x + y)

eval4 (Sub e d) = do x <- eval4 e
                   y <- eval4 d
                   return(x - y)

eval4 (Mul e d) = do x <- eval4 e
                   y <- eval4 d
                   return(x * y)

eval4 (Div e d) = do x <- eval4 e
                   y <- eval4 d
                   guard(y /= 0)
                   return(x `div` y)

-- WHAT IS A MONAD????
--
-- It is simply a type constructor that supports certain additional
-- operations:
--
--
class Monad m where
                (>>=) :: m a -> (a -> m b) -> m b
                (>>) :: m a -> m b -> m b
                return :: a -> m a
                m >> k = m >>= \_ -> k

instance Monad [] where
        return x = [x]
        xs >>= f = concat (map f xs)
