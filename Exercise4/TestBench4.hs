{-# LANGUAGE Unsafe #-}
module TestBench4 where

import safe Exercise4(OurXML(..), Token(..), lexicalAnalysis, syntacticalAnalysis)
import TestHelpers
import Prelude hiding (catch)
import Control.Exception
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess, output)

-- This file was automatically generated
-- by GenerateTests4.hs
-- on 2014-10-21 17:29:04.913167 UTC

main :: IO ()
main = do
    putStrLn "Exercise: lexicalAnalysis (25 points)"
    quickTest "test_lexicalAnalysis" test_lexicalAnalysis
    putStrLn "Exercise: lexicalAnalysis fail when it is supposed to fail (25 points)"
    quickTest "test_fail_lexicalAnalysis" test_fail_lexicalAnalysis
    putStrLn "Exercise: syntacticalAnalysis (25 points)"
    quickTest "test_syntacticalAnalysis" test_syntacticalAnalysis
    putStrLn "Exercise: syntacticalAnalysis fail when it is supposed to fail (25 points)"
    quickTest "test_fail_syntacticalAnalysis" test_fail_syntacticalAnalysis

-- START automatically generated inputs/outputs --


-- START appendix --




unitTest :: (Eq b, Show a, Show b) => String -> (a -> b) -> a -> b -> IO Bool
unitTest name f x spec = do
  -- Using QuickCheck here for convenience (since it catches exceptions and
  -- timeouts for us)
  result <- quickCheckWithResult stdArgs {chatty = False} (f x == spec)
  case result of
    Failure {} -> do
      putStrLn "*** Failed!"
      putStrLn $ "Input: " ++ show x
      catch (putStrLn $ "Output: " ++ show (f x))
            (\msg -> putStrLn $ "Exception: " ++ show (msg :: SomeException))
      putStrLn $ "Expected output: " ++ show spec
      return False
    GaveUp {} -> do
      putStrLn "Gave up."
      return False
    _ -> return True


unitTests :: (Eq b, Show a, Show b) => String -> (a -> b) -> [a] -> [b] -> IO ()
unitTests name f inputs spec = do
  putStr $ "  [testing] " ++ name ++ "... "
  res <- run $ zipWith (unitTest name f) inputs spec
  if res then putStrLn $ "ok."
  else return ()
  where
    run [] = return True
    run (x:xs) = do
      r <- x
      if r then run xs
      else return False

quickTest :: Testable prop => String -> prop -> IO ()
quickTest name prop = do
  putStr $ "  [checking] " ++ name ++ "... "
  let args = stdArgs {chatty = False}
  result <- quickCheckWithResult args prop 
  if not (isSuccess result)
  then do
      putStrLn "eh, nope."
      putStrLn (output result)
  else putStrLn "ok."
  
