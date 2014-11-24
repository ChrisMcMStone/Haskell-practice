{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}
module TestHelpers where

import Prelude hiding (catch)
import Control.Applicative
import Control.Exception (SomeException, catch)
import Control.Monad hiding (replicateM)
import Data.List
import Data.Char (isDigit)
import qualified Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic (run, monadicIO, assert)

import Exercise4
            (OurXML(..), Token(..), lexicalAnalysis, syntacticalAnalysis)

import qualified Exercise4 as Solution

instance Arbitrary OurXML where
    arbitrary = sized go
        where
            go n = frequency
                [ (1, NumElement    <$> arbitrary)
                , (1, StringElement <$> arbString)
                , (n, Tag <$> arbString' <*> resize (n `div` 3) arbitrary)
                ]

            arbString' = listOf1 $ elements chars

arbString :: Gen String
arbString  = arbitrary `suchThat` (not . elem '\'')  :: Gen String


-- Positive tests:
------------------

test_lexicalAnalysis :: Property
test_lexicalAnalysis = forAll arbitrary $ \tree ->
    Solution.lexicalAnalysis (xmlToString tree) == (xmlToTokens tree)

test_syntacticalAnalysis :: Property
test_syntacticalAnalysis = forAll arbitrary $ \tree ->
    Solution.syntacticalAnalysis (xmlToTokens tree) == tree


-- Negative tests:
------------------

test_fail_syntacticalAnalysis :: Property
test_fail_syntacticalAnalysis =
        test_syntacticalAnalysis .&&. failForAll (sized gen) (testFailure fun)
    where
        fun tokens = Solution.syntacticalAnalysis tokens

        gen :: Int -> Gen [Token]
        gen 0 = return []
        gen 1 = listOf1 (oneof [Number <$> arbitrary, Word <$> arbString])
                `suchThat` (\xs -> length xs > 1)
        gen _ = genFailingTokens


test_fail_lexicalAnalysis :: Property
test_fail_lexicalAnalysis =
        test_lexicalAnalysis .&&. failForAll genFailingString (testFailure fun)
    where
        fun str = length (Solution.lexicalAnalysis str) < 0


genFailingTokens :: Gen [Token]
genFailingTokens = do
        notFlatXML <- arbitrary `suchThat` \tree -> case tree of
                                            Tag _ _ -> True
                                            _       -> False
        let tokens       = xmlToTokens notFlatXML
        let numOfOC      = length $ filter isOpenOrClosed tokens
        toModify        <- choose (0, numOfOC - 1)
        let (tok1, tok2) = findSplit toModify [] tokens

        justDelete <- arbitrary :: Gen Bool  -- Delete or rename?
        if justDelete
            then return $ tok1 ++ tail tok2
            else do
                newTag <- case head tok2 of
                    OpenTag  s -> do s' <- modify s; return $ OpenTag  s'
                    CloseTag s -> do s' <- modify s; return $ CloseTag s'

                return $ tok1 ++ [newTag] ++ tail tok2

    where
        findSplit :: Int -> [Token] -> [Token] -> ([Token], [Token])
        findSplit n l1 []        = error "Something went wrong in genFailingTokens."
        findSplit n l1 l2@(x:xs)
            | isOpenOrClosed x = if n == 0 then (l1, l2)
                                           else findSplit (n-1) (l1 ++ [x]) xs
            | otherwise        = findSplit n (l1 ++ [x]) xs

modify :: String -> Gen String
modify s = do
        place <- choose (0, l-1)
        let (s1, x:s2) = splitAt place s

        justDelete <- arbitrary :: Gen Bool
        if justDelete
            then return $ s1 ++ s2
            else do
                c <- arbitrary `suchThat` (\c' -> c' /= x)
                return $ s1 ++ [c] ++ s2
    where
        l = length s


data ParsingState = InTag | InString | InNumber | Other


genFailingString :: Gen String
genFailingString = do
        string <- xmlToString `fmap` arbitrary
        (_, numOfPossible, _) <- modifyString (-1) string  -- (-1) means, we only count
        toModify <- choose (1, numOfPossible)
        (_, _, res) <- modifyString toModify string
        return res
    where
        modifyString whenModify string =
            foldM (countChangable $ skipper whenModify) (Other, 0, "") string


-- We expect to be working with a string generated from an OurXML tree.
countChangable :: (String -> Int -> Char -> ParsingState -> Gen String)
               ->  (ParsingState, Int, String) -> Char -> Gen (ParsingState, Int, String)
countChangable fun (s, n, str) c
    = case (s, c) of
        (Other, '<')  -> f (InTag,    n+1) InTag
        (Other, '\'') -> f (InString, n+1) InString
        (Other, _)  | isDigit c || c == '-' -> f (InNumber, n+1) InNumber
                    | otherwise             -> f (Other,      n) s

        (InTag, '>') -> f (Other, n+1) InTag
        (InTag, _  ) -> f (InTag, n+1) s  -- we ignore the case </...>

        (InNumber, _) | isDigit c -> f (InNumber, n+1) s
                      | otherwise -> countChangable fun (Other, n, str) c

        (InString, '\'') -> f (Other,  n+1) InString
        (InString, _ )   -> f (InString, n) s
    where
        f (newState, newIndex) actState = do
            newString <- fun str newIndex c actState
            return (newState, newIndex, newString)

skipper :: Int                 -- ^ when modify
        -> String              -- ^ previously generated string
        -> Int                 -- ^ current modifiable symbol index
        -> Char                -- ^ Char to modify and append
        -> ParsingState        -- ^ in which state we are
        -> Gen String
skipper n1 str n c sBefore
    | n1 /= n   = return $ str ++ [c]
    | otherwise = do
        c' <- case sBefore of
            InTag    -> elements spaces
            InString -> elements $ chars ++ nums
            InNumber -> elements chars
            Other    -> error "Something went wrong in genFailingString."

        return $ str ++ [c']



-- Helpers
----------

xmlToString :: OurXML -> String
xmlToString t = fld t
    where
        fld (NumElement    i) = show i
        fld (StringElement s) = "'" ++ s ++ "'"
        fld (Tag s list) = "<" ++ s ++ ">" ++ (intercalate " " $ map fld list) ++ "</" ++ s ++ ">"


xmlToTokens :: OurXML -> [Token]
xmlToTokens t = fld t
    where
        fld (NumElement    i) = [Number i]
        fld (StringElement s) = [Word   s]
        fld (Tag s list) = [OpenTag s] ++ (concatMap fld list) ++ [CloseTag s]


eval_synAnal :: OurXML -> Bool
eval_synAnal tree =
    case Solution.syntacticalAnalysis (xmlToTokens tree) of
        NumElement _    -> True
        StringElement _ -> True
        Tag _ _         -> True


isOpenOrClosed :: Token -> Bool
isOpenOrClosed (OpenTag  _) = True
isOpenOrClosed (CloseTag _) = True
isOpenOrClosed           _  = False


failForAll :: Show a => Gen a -> (a -> IO Bool) -> Property
failForAll gen prop =
    forAll gen $ \el -> monadicIO $ do
        res <- run $ catch (prop el) successOnFailure
        assert res

successOnFailure :: SomeException -> IO Bool
successOnFailure _ = return True

testFailure :: (a -> b) -> a -> IO Bool
testFailure f x = seq (f x) $ return False


chars, nums, spaces :: [Char]
chars  = ['a' .. 'z'] ++ ['A' .. 'Z']
nums   = ['0' .. '9']
spaces = [' ', '\n', '\t']

