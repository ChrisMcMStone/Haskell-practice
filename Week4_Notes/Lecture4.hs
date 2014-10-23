module Lecture4 where

import Data.Char

{-

Later we will learn monadic parser combinators, which will make our
task both easier and more modular. To begin with, we do things by
hand.

A simple tree parser. 

  * Concrete syntax consists of strings of characters.
  * Abstract syntax consists of trees.
  * A parser converts from concrete syntax to abstract syntax. 

In order to illustrate the essence of the problem, we work with very
simple trees to begin with:

-}

data Tree = Empty | Fork Int Tree Tree
          deriving Show    -- you can also derive "Read"

{-

We construct a function 

    parseTree :: String -> Tree 

that will satisfy, for example,

    parseTree "Fork 3 Empty (Fork -3 (Fork -7 Empty Empty) Empty)"
  = Fork 3 Empty (Fork (-3) (Fork (-7) Empty Empty) Empty)

Later we will consider parsing of arithmetical expressions and then
programs in our simple programming language for which we have written
an interpreter (for programs given in abstract syntax).

Parsing is usually split into two processes:
  
  (1) We first perform lexical analysis, 
      getting a list of tokens (or lexemes).
  
  (2) Then we perform syntactical analysis, 
      getting a tree from the list of tokens.

See a picture at https://en.wikipedia.org/wiki/Parsing#Overview_of_process

There are many approaches to parsing, and you will learn about this in
more detail in the modules Models of Computation (next term) and
Compilers (next academic year).

In one approach, one describes the lexical structure using regular
expressions. Then the regular expressions are converted to
finite-state automata, which recognize the lexical structure.  One
describes syntactical structure using context-free grammars (aka BNF,
or Backus-Naur form). Then such a grammar is automatically converted
to a parser (of various kinds, such as top-down, bottom-up, LR,
LALR(1), recursive descendent). For example, the Unix tools lex and
yacc, or the Haskell tools Alex and Happy, automate this process.

Another approach, in Haskell, is to use a certain parsing monad, and
define suitable parsing combinators to describe grammatical structure
and parse it. We will study this later.

In this introduction, we will do "parsing by hand", in
recursive-descendent style.

Unparsing is much easier than parsing:

-}
                     
bracket :: String -> String                      
bracket s = "(" ++ s ++ ")"
                      
intUnparse :: Int -> String            
intUnparse = show

treeUnparse :: Tree -> String
treeUnparse Empty = "Empty" 
treeUnparse (Fork x l r)  = "Fork" ++ intUnparse x 
                                   ++ bracket(treeUnparse l) 
                                   ++ bracket(treeUnparse r)

{-

Unparsing is slightly more difficult if one avoids unnecessary
brackets (in our case, around the empty tree).  

Unassesssed Exercise. Define cleverTreeUnparse :: Tree -> String that
doesn't produce unnecessary brackets. This is similar to a bonus
exercise we had before, but easier. 

-}

{-

Back to parsing. 

We have seen that parsing is sensibly split in two phases: lexical
analysis and then syntactical analysis. 

We start with lexical analysis.

-}

data Token = ConstantT Int 
           | EmptyT
           | ForkT
           | OpenBracketT
           | CloseBracketT
           deriving Show
                     
{-                     

We will define a function lexicalAnalysis :: String -> [Token].

We will use the built-in function span, but we define it again:

-}

span' :: (a -> Bool) -> [a] -> ([a],[a])  
span' p [] = ([],[])
span' p (x:xs) 
          | p x       =  (x:ys,zs) 
          | otherwise =  ([],x:xs)
          where (ys,zs) = span' p xs
        
{-                

This is what hoogle says:
http://hackage.haskell.org/package/base-4.6.0.1/docs/Prelude.html#v:span

 span, applied to a predicate p and a list xs, returns a tuple where
 first element is longest prefix (possibly empty) of xs of elements
 that satisfy p and second element is the remainder of the list:

 span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
 span (< 9) [1,2,3] == ([1,2,3],[])
 span (< 0) [1,2,3] == ([],[1,2,3])

-}
                
{-                

We need to know when a word has ended:

-}
                
wordDelimiter :: Char -> Bool                                 
wordDelimiter c = c == '('  ||  c == ')'  ||  isSpace c

wordEnd :: String -> Bool
wordEnd [] = True
wordEnd (x:xs) = wordDelimiter x
                                 
lexicalError :: String -> a                   
lexicalError xs = error ("Lexical error: " ++ xs)
                   
{-

We can now do lexical analysis:

-}

lexicalAnalysis :: String -> [Token]

lexicalAnalysis [] = []

lexicalAnalysis ('(' : xs) = OpenBracketT  : lexicalAnalysis xs

lexicalAnalysis (')' : xs) = CloseBracketT : lexicalAnalysis xs

lexicalAnalysis ('E':'m':'p':'t':'y':xs) 
           | wordEnd xs = EmptyT : lexicalAnalysis xs
           | otherwise  = lexicalError("Empty" ++ xs)

lexicalAnalysis ('F':'o':'r':'k':xs)     
           | wordEnd xs = ForkT  : lexicalAnalysis xs
           | otherwise  = lexicalError("Fork" ++ xs)

lexicalAnalysis (x:xs)   
           | (isDigit x || x == '-') && wordEnd zs = ConstantT n : lexicalAnalysis zs
           | otherwise = lexicalError 
           where 
             (ys, zs) = span' isDigit xs
             n = read(x : ys)    


{-

This is an opportunity for introducing @ patterns (pronounced "as
patterns"): 

-}

lexicalAnalysis' :: String -> [Token]

lexicalAnalysis' [] = []

lexicalAnalysis' ('(' : xs) = OpenBracketT  : lexicalAnalysis' xs

lexicalAnalysis' (')' : xs) = CloseBracketT : lexicalAnalysis' xs

lexicalAnalysis' ys@('E':'m':'p':'t':'y':xs) 
           | wordEnd xs = EmptyT : lexicalAnalysis' xs
           | otherwise  = lexicalError ys

lexicalAnalysis' ys@('F':'o':'r':'k':xs)     
           | wordEnd xs = ForkT  : lexicalAnalysis' xs
           | otherwise  = lexicalError ys

lexicalAnalysis' (x:xs) 
           | isDigit x || x == '-' = ConstantT n : lexicalAnalysis' zs
           where 
             (ys, zs) = span' isDigit xs
             n = read(x:ys)    

lexicalAnalysis' (x:xs) 
           | isSpace x = lexicalAnalysis' xs

lexicalAnalysis' (x:xs) 
           | otherwise = lexicalError(x:xs)


{-
 
Example:

*Main> lexicalAnalysis "Fork 3 Empty (Fork 4 (Fork 7 Empty Empty) Empty)"
[ForkT,ConstantT 3,EmptyT,OpenBracketT,ForkT,ConstantT 4,OpenBracketT,
 ForkT,ConstantT 7,EmptyT,EmptyT,CloseBracketT,EmptyT,CloseBracketT]

-}

{-

Now we do syntactical analysis.

-}

{-                 

The function we aim to define is

    syntacticalAnalysis :: [Token] -> Tree

Once this is defined, we can define

    parseTree :: String -> Tree
    parseTree = syntacticalAnalysis . lexicalAnalysis 

But it is easier to first define

    scanTree : [Token] -> (Tree, [Token])

The result is a tree produced by consuming some of the given
tokens. The remaining tokens are returned. 

For example, to parse

    Fork 8 Empty (Fork 7 Empty),

once one has seen the token ForkT, one has to first scan an Int,
which then gives

    Empty (Fork 7 Empty)

left, then scan a left subtree, which gives
    
    (Fork 7 Empty) 

left, and a right subtree, which gives nothing left.

-}
                 
syntacticalAnalysis :: [Token] -> Tree
syntacticalAnalysis xs = 
  let (t, ys) = scanTree xs 
  in case ys of
       [] -> t 
       _  -> syntaxError("Expected end of input but found " ++ show ys) 

scanTree :: [Token] -> (Tree, [Token])

scanTree [] = syntaxError "Expected a tree but found end of input"

scanTree (EmptyT:xs) = (Empty, xs)

scanTree (ForkT:xs) = (Fork n l r, ws)
  where
    (n, ys) = scanInt xs
    (l, zs) = scanTree ys
    (r, ws) = scanTree zs

scanTree (OpenBracketT:xs) = (t, zs)  
  where
    (t, ys) = scanTree xs
    zs = scanCloseBracket ys

scanTree xs = syntaxError("Expected a tree but found " ++ show xs)    
    
{-

We used the following functions in the above definition:

-}

scanInt :: [Token] -> (Int, [Token])                 
scanInt [] = syntaxError "Expected integer but got end of input"
scanInt (ConstantT n : xs) = (n, xs)
scanInt xs = syntaxError("Expected integer but found " ++ show xs)

scanCloseBracket :: [Token] -> [Token]
scanCloseBracket [] = syntaxError "Expected ')' but found end of input"
scanCloseBracket (CloseBracketT : xs) = xs
scanCloseBracket (x:xs) = syntaxError("Expected ')' but found " ++ show(x:xs))
    
syntaxError :: String -> a                   
syntaxError xs = error ("Syntax error: " ++ xs)
 
{-

Finally, as discussed above, parsing consists of lexical analysis
followed by syntactical analysis (we use the function-composition
symbol "."):

-}

parseTree :: String -> Tree
parseTree = syntacticalAnalysis . lexicalAnalysis    

{-

Examples to try:

parseTree "Fork 3 Empty (Fork 4 (Fork 7 Empty Empty) Empty)"

parseTree "Fork 3 (Fork 4 Empty Empty) (Fork -3 (Fork -7 Empty Empty) Empty)"

parseTree "Fork 3 Empty (Fork 4 (Fork 7 Empty Empty) Empty)"

parseTree "Fork 3 Empty (Fork 4 (Fork 7 Empty) Empty)"

parseTree "Fork 3 Empty (Fork 4 (Fork 7 Empty Empty) Empty"

parseTree "Fork 3 Empty (Fork 4 (Fork 7 Empty) Empty)"

parseTree "Fork 3 Empty (Fork 4 (Fork7 Empty Empty) Empty"

parseTree "Fork 3 E mpty (Fork 4 (Fork 7 Empty Empty) Empty"

parseTree "Fork 3 Empty (Fork 4 (Fork 7 Empty Empty) Empty Empty"

parseTree "Fork 3 Empty (Fork 4 (Fork 7 Empty Empty Empty"

parseTree "Fork 3 Empty (Fork (Fork 7 Empty Empty) Empty Empty"

parseTree "Fork 3 Empty (Fork (Fork -7 Empty Empty) Empty Empty"

parseTree "Fork 3 Empty (Fork -3 (Fork -7 Empty Empty) Empty Empty"

parseTree "Fork 3 Empty (Fork -3 (Fork -7 Empty Empty) Empty Empty)"

-}
    
{-

This approach has a number of drawbacks. To begin with, using "error"
is not useful. One should use the Maybe type or a similar
technique. Secondly, if we had slightly different trees, we would need
a lot of rewriting.

Unassessed exercise. Consider 

  data Tree' = Leaf Int | Branch Tree' Tree'

  Adapt the above code to parse such trees. You should have lexical
  analysis and syntactical analysis as presented above. This is to be
  solved in the lab, in the file Solution.hs as usual, which we will
  provide.

Later we will see how monads can help.

-}

{-

"Cheating":

-}

data Rose a = RBranch a [Rose a] deriving (Show, Read)

parseRoseTree :: String -> Rose a
parseRoseTree = read

{-

*Lecture4> read "RBranch 3 []" :: Rose Int
RBranch 3 []
*Lecture4> :type read
read :: Read a => String -> a
*Lecture4> read "blah" :: Rose Int
*** Exception: Prelude.read: no parse
*Lecture4> 

-}

{-

Can you do it yourself?

-}
