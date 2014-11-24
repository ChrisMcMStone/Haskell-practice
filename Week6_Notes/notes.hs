{-
    class Monad m where  

        return :: a -> m a  
        (>>=) :: m a -> (a -> m b) -> m b  
        (>>) :: m a -> m b -> m b  
        x >> y = x >>= \_ -> y  

        fail :: String -> m a  
        fail msg = error msg 



    instance Monad Maybe where  
        return x = Just x  
        Nothing >>= f = Nothing  
        Just x >>= f  = f x  
        fail _ = Nothing  
-}


instance Monad Rand where
        return x = Box(\g -> (x,g))
        Box h >>= f = Box(\g -> let (x, g') = h g
                                    (Box h') = f x
                                in h' g')

