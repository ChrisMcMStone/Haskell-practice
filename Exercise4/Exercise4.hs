
{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
module Exercise4 where

import Data.Char

data OurXML = NumElement Integer
            | StringElement String
            | Tag String [OurXML]
    deriving (Show, Read, Eq)

{-

This input

 <MyShoppingList>
    <Item> 'milk' 1 </Item>
    <Item> 'orange' 3 </Item>
    <Item> 'bread' 'five' </Item>
 </MyShoppingList>

should produce this tree

Tag "MyShoppingList" [
    Tag "Item" [
        StringElement "milk",
        NumElement 1],
    Tag "Item" [
        StringElement "orange",
        NumElement 3],
    Tag "Item" [
        StringElement "bread",
        StringElement "five"]]

This input

<MyShoppingList>
    <Item> 
'milk' 1 </Item>
    <Item> 'orange' 
3 </Item>
    <Item> 'bread' 'five' </Item>  </MyShoppingList>

should produce the same tree as above (that is, spaces and new lines
don't matter).

This input

 <MyShoppingList>
 </MyShoppingList>

should produce this tree

 Tag "MyShoppingList" []

On the other hand, the following should give a lexical error, because
the word milk won't be recognized:

 <MyShoppingList>
    <Item> milk 1 </Item>
    <Item> orange 3 </Item>
    <Item> bread 'five' </Item>
 </MyShoppingList>

Similarly, something such as 345A should also fail.

Also, if the thing between < and > does not consist solely of letters, you
should give a lexical error. Same for the thing between </ and >. For
instance, these strings should give a lexical error:

- < MyShoppingList></ MyShoppingList>
- <My ShoppingList></My ShoppingList>

The above are all lexical errors. The following is an example of a
syntactical error:

 <MyShoppingList>
    <Item> 'milk' 1 </Item>
    <Item> 'orange' 3 </Item>
    <Item> 'bread' 'five' </Item>
 </MyShoppinglist>

That's because the tags MyShoppingList and MyShoppinglist are not
quite the same. 

Another example of a syntactical error is this:

 <MyShoppingList>
    <Item> 'milk' 1 </Item>
    <Item> 'orange' 3 
    <Item> 'bread' 'five' </Item>
 </MyShoppingList>

A final syntactical error is the following:

- 3 5

The reason is that the string we're parsing contains multiple XML nodes:
NumElement 3 and NumElement 5. The string must contain exactly one node.

-----

As in the lecture, we will split up recognising this language into two parts: 

- Lexical analysis, which converts a string into a list of tokens
- Syntactical analysis, which converts a list of tokens into an OurXML value.

It's up to you to make both functions. For inspiration, use the material
discussed in the lecture, which will be available on the module web site.

-}

data Token = Number Integer  -- 9999 and -9999 are numbers.
           | Word String     -- 'milk' is a word, but milk is not
           | OpenTag String  -- matches "<tag>" where tag is a string (with no quotes)
           | CloseTag String
    deriving (Show, Eq)

{-

In the following two exercises (lexical analysis and syntactical
analysis), points will be given for accepting correct input, and
points will be deducted for accepting incorrect input. 

Exercise (50 points). Define lexicalAnalysis.

  Split as follows: 

  25 points for accepting correct input and producing correct output.

  25 points for rejecting incorrect input and producing error.


-}

lexicalAnalysis :: String -> [Token]

lexicalAnalysis [] = []

lexicalAnalysis ('<':'/' : xs) = case word of
    []-> error "Empty tag name"
    _-> case rest of
        ('>' : _) -> CloseTag word : lexicalAnalysis (tail rest)
        _ -> error "You forgot ur tag"
  where
    (word,rest) = span isLetter xs 

lexicalAnalysis ('<' : xs) = case word of
    []-> error "Empty tag name"
    _-> case rest of
        ('>' : _) -> OpenTag word : lexicalAnalysis (tail rest)
        _ -> error "You forgot ur tag"
  where
    (word,rest) = span isLetter xs 

lexicalAnalysis (x : xs)
        | isDigit x = Number n : lexicalAnalysis zs
        | x == '-' = case ys of [] -> error "..."
                                _ -> Number (-m) : lexicalAnalysis zs
        where
         (ys, zs) = span isDigit xs
         n = read (x:ys)
         m = read ys

lexicalAnalysis ('\'' : xs) = case rest of
    ('\'' : _) -> Word word : lexicalAnalysis (tail rest)
    where
            (word,rest) = span (/='\'') xs

lexicalAnalysis (x:xs)        
        | isSpace x = lexicalAnalysis xs
        | otherwise = error "error"

{-

Exercise (50 points). Define syntacticalAnalysis.

  Split as follows: 

  25 points for accepting correct input and producing correct output.

  25 points for rejecting incorrect input and producing error.


It is only necessary to syntactically analyse token lists that could have been
returned by lexicalAnalysis. So you don't have to be able to
recognize [OpenTag " hi"].

-}

syntacticalAnalysis :: [Token] -> OurXML
syntacticalAnalysis tokens = let (xs, ys) = scanTag tokens in case ys of
                              [] -> xs
                              _ -> error ("Invalid Token " ++ show ys)
                               
scanTag :: [Token] -> (OurXML, [Token])
scanTag [] = error "No Program"
scanTag (Word x : rest) = (StringElement x, rest)
scanTag (Number x : rest) = (NumElement x, rest)

scanTag (OpenTag x : xs) = (Tag x item1, rest)
     where
     (item1, rest) = tags (OpenTag x : xs)
 
tags :: [Token] -> ([OurXML], [Token])
tags [] = ([],[]) 
tags (OpenTag x:CloseTag y:xs) | x == y = ([], xs) 
                               | otherwise = error "Tags don't match"
tags (OpenTag x : xs) = (y : ys, zs)
     where
     (y, as) = scanTag xs
     (ys, zs) = tags (OpenTag x : as)


{-

When we combine the two functions, we get a parser for our XML dialect.

-}

parseXML :: String -> OurXML
parseXML = syntacticalAnalysis . lexicalAnalysis
