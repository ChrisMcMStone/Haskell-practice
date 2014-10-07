module TestBench2 where

import Prelude hiding (catch)
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import Test.QuickCheck

import Exercise2(Expr(..), Sequence(..), Tree(..), Tree'(..), Result(Value, Error), toNormal, fromNormal, sequenceMap, sequenceAppend, sequenceFlatten, sequenceSplit, treeMap, treeMap')
import qualified Exercise2 as Solution
import Helpers
import TestsGenInputsOutputs

main :: IO ()
main = do
  unitTests "eval'" Solution.eval' eval'_in eval'_out
  unitTests "(BONUS) cleverShow" Solution.cleverShow cleverShow_in cleverShow_out
  unitTests "showWithOutput" Solution.showWithOutput showWithOutput_in showWithOutput_out

  quickTest "fromNormal (toNormal xs) = xs" prop_normal1
  quickTest "toNormal (fromNormal xs) = xs" prop_normal2
  --unitTests "listShow" Solution.listShow seqs showLists
  unitTests "sequenceMap odd" (sequenceMap odd) seqs sequenceMapOddLists
  unitTests "sequenceMap even" (sequenceMap even) seqs sequenceMapEvenLists
  unitTests "sequenceMap (*2)" (sequenceMap (*2)) seqs sequenceMapDoubleLists


  quickTest "append commutes with toNormal" prop_seqAppend
  quickTest "flatten commutes with toNormal" prop_seqFlatten
  unitTests "sequenceAppend" func_seqAppend func_seqAppend_in func_seqAppend_out
  unitTests "sequenceFlatten" sequenceFlatten seqFlatten_in seqFlatten_out
  quickTest "sequenceSplit: length" prop_seqSplit_length
  quickTest "sequenceSplit: contents" prop_seqSplit_contents
  quickTest "treeMap on the identity function" prop_treeMap1
  quickTest "treeMap' on the identity function" prop_treeMap'1
  quickTest "treeMap: +1 after +1 is +2" prop_treeMap2
  quickTest "treeMap': +1 after +1 is +2" prop_treeMap'2
  quickTest "treeMap: on different types" prop_treeMap3
  quickTest "treeMap': on different types" prop_treeMap'3
  unitTests "treeFold" func_treeFold func_treeFold_in func_treeFold_out
  unitTests "treeFlatten" Solution.treeFlatten treeFlatten_in treeFlatten_out

instance Arbitrary a => Arbitrary (Sequence a) where
  arbitrary = go <$> arbitrary
    where go [] = Empty
          go (x:xs) = Cons x (go xs)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized go
    where
      go n = frequency
        [ (1, return Leaf)
        , (n, Fork <$> arbitrary <*> go (n `div` 2) <*> go (n `div` 2))
        ]

instance Arbitrary a => Arbitrary (Tree' a) where
  arbitrary = sized go
    where
      go n = frequency
        [ (1, Leaf' <$> arbitrary)
        , (n, Fork' <$> go (n `div` 2) <*> go (n `div` 2))
        ]
instance Arbitrary Expr where
  arbitrary = sized go
    where
      go n = frequency $
        (1, Constant <$> arbitrary) :
        map (\c -> (n, c <$> go (n `div` 2) <*> go (n `div` 2)))
            [Add, Sub, Mul, Div]

instance Arbitrary a => Arbitrary (Wrap a) where
  arbitrary = Wrap <$> arbitrary
instance Functor Wrap where
  fmap f (Wrap x) = Wrap (f x)

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
  putStrLn $ "[testing] " ++ name ++ "..."
  res <- run $ zipWith (unitTest name f) inputs spec
  if res then putStrLn $ "+++ OK, passed " ++ (show $ length inputs) ++ " tests."
  else return ()
  where
    run [] = return True
    run (x:xs) = do
      r <- x
      if r then run xs
      else return False

quickTest :: Testable prop => String -> prop -> IO ()
quickTest name prop = do
  putStrLn $ "[checking] " ++ name ++ "..."
  quickCheck prop

-- * QuickCheck properties
prop_normal1 :: Sequence Integer -> Bool
prop_normal1 xs = fromNormal (toNormal xs) == xs

prop_normal2 :: [Integer] -> Bool
prop_normal2 xs = toNormal (fromNormal xs) == xs

prop_seqAppend :: Sequence Integer -> Sequence Integer -> Bool
prop_seqAppend xs ys = zs == (xs `sequenceAppend` ys)
  where
    zs = fromNormal (toNormal xs ++ toNormal ys)

prop_seqFlatten :: Sequence (Sequence Integer) -> Bool
prop_seqFlatten xs = sequenceFlatten xs == ys
  where
    ys = (fromNormal . concat . map toNormal . toNormal) xs

prop_seqSplit_length :: Sequence Integer -> Bool
prop_seqSplit_length xs = length (toNormal as) - length (toNormal bs) `elem` [0, 1]
  where 
    (as, bs) = sequenceSplit xs

prop_seqSplit_contents :: Sequence Integer -> Bool
prop_seqSplit_contents xs = toNormal as ++ toNormal bs == toNormal xs
  where 
    (as, bs) = sequenceSplit xs

prop_treeMap1 :: Tree Integer -> Bool
prop_treeMap1 t = t == treeMap id t

prop_treeMap'1 :: Tree' Integer -> Bool
prop_treeMap'1 t = t == treeMap' id t

prop_treeMap2 :: Tree Integer -> Bool
prop_treeMap2 t = u == v
  where
    u = treeMap (+2) t
    v = treeMap (+1) (treeMap (+1) t)

prop_treeMap'2 :: Tree' Integer -> Bool
prop_treeMap'2 t = u == v
  where
    u = treeMap' (+2) t
    v = treeMap' (+1) (treeMap' (+1) t)

-- Also tests on different types.
prop_treeMap3 :: Tree (Wrap Integer) -> Bool
prop_treeMap3 t = u == v
  where
    u = treeMap (fmap (\x -> [x + 1])) t
    v = treeMap (fmap list) (treeMap (fmap (+1)) t )

    list x = [x]

prop_treeMap'3 :: Tree' (Wrap Integer) -> Bool
prop_treeMap'3 t = u == v
  where
    u = treeMap' (fmap (\x -> [x + 1])) t
    v = treeMap' (fmap list) (treeMap' (fmap (+1)) t )

    list x = [x]



-- * Precomputed tests

seqs :: [Sequence Integer]
seqs =
  [ Empty
  , Cons 0 Empty
  , Cons (-3) Empty
  , Cons 1 (Cons 8 (Cons (-6) (Cons (-4) Empty)))
  , Cons 23 Empty
  , Cons (-8) (Cons 57 Empty)
  , Cons (-19) (Cons 24 (Cons 40 (Cons (-22) (Cons 78 (Cons (-57) (Cons (-19) (Cons 2 (Cons (-12) Empty))))))))
  , Cons 135 Empty
  , Cons (-28) (Cons (-730) (Cons (-386) (Cons (-694) (Cons 45 (Cons (-281) (Cons 662 (Cons 216 (Cons (-78) (Cons 973 (Cons 20 (Cons (-902) (Cons (-205) Empty))))))))))))
  , Cons 61 (Cons (-1779) (Cons 63 (Cons (-1999) (Cons (-949) (Cons (-1872) (Cons 1433 (Cons (-1825) (Cons 424 (Cons (-1639) (Cons (-1819) (Cons (-1061) (Cons (-1624) (Cons 1415 (Cons 1008 (Cons 1305 (Cons 2023 Empty))))))))))))))))
  , Cons 670 (Cons 1687 (Cons 586 (Cons (-445) (Cons (-326) (Cons (-22) (Cons (-327) (Cons 3909 (Cons (-2447) (Cons (-112) (Cons 1020 Empty))))))))))
  , Cons 2 (Cons (-2) Empty)
  , Cons (-2) (Cons (-3) Empty)
  , Cons (-1) Empty
  , Cons (-29) (Cons (-12) (Cons 12 (Cons (-27) (Cons 13 (Cons (-8) Empty)))))
  , Cons 29 (Cons 3 Empty)
  , Cons (-36) (Cons (-85) (Cons (-14) (Cons 17 (Cons (-76) (Cons 9 (Cons (-45) (Cons 15 (Cons (-116) Empty))))))))
  , Cons 132 (Cons 217 Empty)
  , Cons (-1021) (Cons 80 (Cons 968 (Cons (-186) (Cons 375 (Cons (-395) (Cons 243 (Cons (-879) (Cons 426 (Cons (-441) (Cons 637 (Cons (-75) (Cons (-549) (Cons (-795) (Cons (-491) Empty))))))))))))))
  , Cons (-1793) (Cons (-1448) (Cons 1364 (Cons 1574 (Cons (-2012) (Cons 1786 (Cons 1965 (Cons 1470 (Cons 1992 (Cons 198 (Cons 806 (Cons 600 (Cons (-1297) (Cons (-858) (Cons (-382) (Cons 1832 (Cons (-638) Empty))))))))))))))))
  , Cons 2244 (Cons (-2084) (Cons 201 Empty))
  ]

showLists :: [String]
showLists =
  ["{}","{0}","{-3}","{1; 8; -6; -4}","{23}","{-8; 57}","{-19; 24; 40; -22; 78; -57; -19; 2; -12}","{135}","{-28; -730; -386; -694; 45; -281; 662; 216; -78; 973; 20; -902; -205}","{61; -1779; 63; -1999; -949; -1872; 1433; -1825; 424; -1639; -1819; -1061; -1624; 1415; 1008; 1305; 2023}","{670; 1687; 586; -445; -326; -22; -327; 3909; -2447; -112; 1020}","{2; -2}","{-2; -3}","{-1}","{-29; -12; 12; -27; 13; -8}","{29; 3}","{-36; -85; -14; 17; -76; 9; -45; 15; -116}","{132; 217}","{-1021; 80; 968; -186; 375; -395; 243; -879; 426; -441; 637; -75; -549; -795; -491}","{-1793; -1448; 1364; 1574; -2012; 1786; 1965; 1470; 1992; 198; 806; 600; -1297; -858; -382; 1832; -638}","{2244; -2084; 201}"]

sequenceMapOddLists :: [Sequence Bool]
sequenceMapOddLists =
  [Empty,Cons False Empty,Cons True Empty,Cons True (Cons False (Cons False (Cons False Empty))),Cons True Empty,Cons False (Cons True Empty),Cons True (Cons False (Cons False (Cons False (Cons False (Cons True (Cons True (Cons False (Cons False Empty)))))))),Cons True Empty,Cons False (Cons False (Cons False (Cons False (Cons True (Cons True (Cons False (Cons False (Cons False (Cons True (Cons False (Cons False (Cons True Empty)))))))))))),Cons True (Cons True (Cons True (Cons True (Cons True (Cons False (Cons True (Cons True (Cons False (Cons True (Cons True (Cons True (Cons False (Cons True (Cons False (Cons True (Cons True Empty)))))))))))))))),Cons False (Cons True (Cons False (Cons True (Cons False (Cons False (Cons True (Cons True (Cons True (Cons False (Cons False Empty)))))))))),Cons False (Cons False Empty),Cons False (Cons True Empty),Cons True Empty,Cons True (Cons False (Cons False (Cons True (Cons True (Cons False Empty))))),Cons True (Cons True Empty),Cons False (Cons True (Cons False (Cons True (Cons False (Cons True (Cons True (Cons True (Cons False Empty)))))))),Cons False (Cons True Empty),Cons True (Cons False (Cons False (Cons False (Cons True (Cons True (Cons True (Cons True (Cons False (Cons True (Cons True (Cons True (Cons True (Cons True (Cons True Empty)))))))))))))),Cons True (Cons False (Cons False (Cons False (Cons False (Cons False (Cons True (Cons False (Cons False (Cons False (Cons False (Cons False (Cons True (Cons False (Cons False (Cons False (Cons False Empty)))))))))))))))),Cons False (Cons False (Cons True Empty))]

sequenceMapEvenLists :: [Sequence Bool]
sequenceMapEvenLists =
  [Empty,Cons True Empty,Cons False Empty,Cons False (Cons True (Cons True (Cons True Empty))),Cons False Empty,Cons True (Cons False Empty),Cons False (Cons True (Cons True (Cons True (Cons True (Cons False (Cons False (Cons True (Cons True Empty)))))))),Cons False Empty,Cons True (Cons True (Cons True (Cons True (Cons False (Cons False (Cons True (Cons True (Cons True (Cons False (Cons True (Cons True (Cons False Empty)))))))))))),Cons False (Cons False (Cons False (Cons False (Cons False (Cons True (Cons False (Cons False (Cons True (Cons False (Cons False (Cons False (Cons True (Cons False (Cons True (Cons False (Cons False Empty)))))))))))))))),Cons True (Cons False (Cons True (Cons False (Cons True (Cons True (Cons False (Cons False (Cons False (Cons True (Cons True Empty)))))))))),Cons True (Cons True Empty),Cons True (Cons False Empty),Cons False Empty,Cons False (Cons True (Cons True (Cons False (Cons False (Cons True Empty))))),Cons False (Cons False Empty),Cons True (Cons False (Cons True (Cons False (Cons True (Cons False (Cons False (Cons False (Cons True Empty)))))))),Cons True (Cons False Empty),Cons False (Cons True (Cons True (Cons True (Cons False (Cons False (Cons False (Cons False (Cons True (Cons False (Cons False (Cons False (Cons False (Cons False (Cons False Empty)))))))))))))),Cons False (Cons True (Cons True (Cons True (Cons True (Cons True (Cons False (Cons True (Cons True (Cons True (Cons True (Cons True (Cons False (Cons True (Cons True (Cons True (Cons True Empty)))))))))))))))),Cons True (Cons True (Cons False Empty))]

sequenceMapDoubleLists :: [Sequence Integer]
sequenceMapDoubleLists =
  [Empty,Cons 0 Empty,Cons (-6) Empty,Cons 2 (Cons 16 (Cons (-12) (Cons (-8) Empty))),Cons 46 Empty,Cons (-16) (Cons 114 Empty),Cons (-38) (Cons 48 (Cons 80 (Cons (-44) (Cons 156 (Cons (-114) (Cons (-38) (Cons 4 (Cons (-24) Empty)))))))),Cons 270 Empty,Cons (-56) (Cons (-1460) (Cons (-772) (Cons (-1388) (Cons 90 (Cons (-562) (Cons 1324 (Cons 432 (Cons (-156) (Cons 1946 (Cons 40 (Cons (-1804) (Cons (-410) Empty)))))))))))),Cons 122 (Cons (-3558) (Cons 126 (Cons (-3998) (Cons (-1898) (Cons (-3744) (Cons 2866 (Cons (-3650) (Cons 848 (Cons (-3278) (Cons (-3638) (Cons (-2122) (Cons (-3248) (Cons 2830 (Cons 2016 (Cons 2610 (Cons 4046 Empty)))))))))))))))),Cons 1340 (Cons 3374 (Cons 1172 (Cons (-890) (Cons (-652) (Cons (-44) (Cons (-654) (Cons 7818 (Cons (-4894) (Cons (-224) (Cons 2040 Empty)))))))))),Cons 4 (Cons (-4) Empty),Cons (-4) (Cons (-6) Empty),Cons (-2) Empty,Cons (-58) (Cons (-24) (Cons 24 (Cons (-54) (Cons 26 (Cons (-16) Empty))))),Cons 58 (Cons 6 Empty),Cons (-72) (Cons (-170) (Cons (-28) (Cons 34 (Cons (-152) (Cons 18 (Cons (-90) (Cons 30 (Cons (-232) Empty)))))))),Cons 264 (Cons 434 Empty),Cons (-2042) (Cons 160 (Cons 1936 (Cons (-372) (Cons 750 (Cons (-790) (Cons 486 (Cons (-1758) (Cons 852 (Cons (-882) (Cons 1274 (Cons (-150) (Cons (-1098) (Cons (-1590) (Cons (-982) Empty)))))))))))))),Cons (-3586) (Cons (-2896) (Cons 2728 (Cons 3148 (Cons (-4024) (Cons 3572 (Cons 3930 (Cons 2940 (Cons 3984 (Cons 396 (Cons 1612 (Cons 1200 (Cons (-2594) (Cons (-1716) (Cons (-764) (Cons 3664 (Cons (-1276) Empty)))))))))))))))),Cons 4488 (Cons (-4168) (Cons 402 Empty))]
