eval5 (Add e d) = do x <- eval5 e
                      y <- eval5 d
                      return(x+y)

eval5 (Add e d) = eval5 e >>= (\x -> eval5 d >>= \y -> return(x + y))



