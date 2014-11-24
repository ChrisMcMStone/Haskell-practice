module YayApplicative where

{-
f here stands for FUNCTOR, not FUNCTION

class Functor f where
  fmap :: (a -> b) -> (f a -> f b)
-}

data Sequence a = Nil | Cons a (Sequence a)
  deriving (Show)

instance Functor Sequence where
--And here, f is FUNCTION not FUNCTOR
  fmap f Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

fmapDefault :: Applicative f => (a -> b) -> (f a -> f b)
fmapDefault f xs = pure f <*> xs

{-
fmap  :: (a -> b) -> [a] -> [b]
(<*>) :: [a -> b] -> [a] -> [b]
--          m         n     m*n
-}

instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]

instance Applicative Maybe where
  pure x = Just x
  Nothing <*> _ = Nothing
  _ <*> Nothing = Nothing
  Just f <*> Just x = Just $ f x

{-
f        :: Student -> Marks
x        :: Maybe Student
fmap f   :: Maybe Student -> Maybe Marks
fmap f x :: Maybe Marks

f       :: Maybe (Student -> Marks)
x       :: Maybe Student
f <*> x :: Maybe Marks
-}

{-
(<$>) :: (a -> b) -> [a] -> [b]
(<*>) :: [a -> b] -> [a] -> [b]

(<$>) ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b


1:  a ->  b ->   c -> d
2:  a -> (b ->  (c -> d))
3: (a ->  b) -> (c -> d)

1 and 2 are the same, 3 is different
-}

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

add :: Maybe Int -> Maybe Int -> Maybe Int
add Nothing _ = Nothing
add _ Nothing = Nothing
add (Just x) (Just y) = Just $ x + y

add' :: Maybe Int -> Maybe Int -> Maybe Int
add' x y = (+) <$> x <*> y

add5 :: Maybe Int -> Maybe Int
add5 x = (+5) <$> x

{-
(+) :: Int ->  Int -> Int
(+) :: Int -> (Int -> Int)

fmap (+)  :: Maybe Int -> Maybe (Int -> Int)
((+) <$>) :: Maybe Int -> Maybe (Int -> Int)

(+) <$> Just 5 :: Maybe (Int -> Int)
(+) <$> pure 5 :: Maybe (Int -> Int)

(((+) <$> pure 5) <*>) :: Maybe Int -> Maybe Int

(((+) <$> pure 5) <*> Just 2) :: Maybe Int
(((+) <$> pure 5) <*> pure 2) :: Maybe Int

(+) <$> pure 5 <*> pure 2 :: Maybe Int
(+) <$> Just 5 <*> Just 2 :: Maybe Int

Just 7 :: Maybe Int

(<*>) :: f (a -> b) -> f a -> f b

(<*>) :: f (a -> b) -> (f a -> f b)


f (a -> b)
f a -> f b

3*(1 + 2)
3*1 + 3*2
-}

data Expr
  = Lit Int
  | Add Expr Expr

eval :: Expr -> Int
eval (Lit x) = x
eval (Add e1 e2) = (+) (eval e1) (eval e2)

eval' :: Expr -> Maybe Int
eval' (Lit x) = pure x
eval' (Add e1 e2) = pure (+) <*> eval' e1 <*> eval' e2

{-
 -pure (+) :: f (Int -> Int -> Int)
 -pure (+) <*> eval' e1 :: f (Int -> Int)
 -pure (+) <*> eval' e1 <*> eval' e2 :: f Int
 -}


{-
 -(<*>) :: f (a -> b) -> f a -> f b
 -(<$>) ::   (a -> b) -> f a -> f b
 -}
