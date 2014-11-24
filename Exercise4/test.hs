import Data.Char

data Token = Number Integer  -- 9999 and -9999 are numbers.
           | Word String     -- 'milk' is a word, but milk is not
           | OpenTag String  -- matches "<tag>" where tag is a string (with no quotes)
           | CloseTag String
    deriving (Show, Eq)


wordDelimiter :: Char -> Bool                                 
wordDelimiter c = c == '<'  ||  c == '>'  ||  isSpace c

wordEnd :: String -> Bool
wordEnd [] = True
wordEnd (x:xs) = wordDelimiter x

lexicalError :: String -> a                   
lexicalError xs = error ("Lexical error: " ++ xs)


lexicalAnalysis :: String -> [Token]

lexicalAnalysis [] = []

lexicalAnalysis ('<' : xs) 
        | head zs == '>' = OpenTag ys : lexicalAnalysis zs
        | otherwise = lexicalError xs
        where
        (ys, zs) = span (not . wordDelimiter) xs


