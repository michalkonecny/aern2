module FnReps.Polynomial.UnaryPower.IntPoly.Bench where

import AERN2.Num -- alternative Prelude
--import qualified Prelude as P

import qualified Data.List as List
--import Data.Ratio

import System.Random (randomRIO)

import Criterion.Main

import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)
import Test.QuickCheck.Gen (Gen(..))

--import FnReps.Polynomial.UnaryPower.IntPoly.Basics
import FnReps.Polynomial.UnaryPower.IntPoly.EvaluationRootFinding (isolateRoots)

import FnReps.Polynomial.UnaryPower.IntPoly.Tests

benchMain :: IO ()
benchMain = defaultMain 
    [
       bgroup "rootSetSize" 
        [
            bench (show i) $ nfIO (benchmarkRootIsolationByRootSetSize i)
            | i <- [1..10]
        ]
    ,
       bgroup "rootMultiSetSize" 
        [
            bench (show i) $ nfIO (benchmarkRootIsolationByRootMultiSetSize i)
            | i <- [1..10]
        ]
    ]

benchmarkRootIsolationByRootMultiSetSize :: Integer -> IO [(Rational, Rational)]
benchmarkRootIsolationByRootMultiSetSize rootMultiSetSize =
    benchmarkRootIsolationUsingPolys $ polysWithRootMultiSetSize rootMultiSetSize 100
    
benchmarkRootIsolationByRootSetSize :: Integer -> IO [(Rational, Rational)]
benchmarkRootIsolationByRootSetSize rootSetSize =
    benchmarkRootIsolationUsingPolys $ polysWithRootSetSize rootSetSize 100
    
benchmarkRootIsolationUsingPolys :: 
    [IntPolyWithRoots] -> IO [(Rational, Rational)]
benchmarkRootIsolationUsingPolys polys =
    do
    index <- randomRIO (1,100)
    return $ runWithIndex index
    where
    runWithIndex index =
        map (\(Interval l r) -> (l,r)) isolateRootsResult
--        integer $ length isolateRootsResult
        where
        (IntPolyWithRoots intpoly _denom rootsMSorted) = polys !!! index
        rootsSorted = map fst rootsMSorted
        isolateRootsResult = isolateRoots l r intpoly
            where
            l = -1 + (minimum rootsSorted')
            r = 1 + (maximum rootsSorted')
            rootsSorted' 
                | null rootsSorted = [0.0]
                | otherwise = rootsSorted
        
polysWithRootMultiSetSize :: Integer -> Integer -> [IntPolyWithRoots]
polysWithRootMultiSetSize rootMultiSetSize coeffSize =
    polysFromGen $ arbitraryRootMultiSet rootMultiSetSize coeffSize

polysWithRootSetSize :: Integer -> Integer -> [IntPolyWithRoots]
polysWithRootSetSize rootSetSize coeffSize =
    polysFromGen $ arbitraryRootSet rootSetSize coeffSize

polysFromGen :: 
    Gen [(Rational, RootMultiplicity)] -> [IntPolyWithRoots]
polysFromGen rootGen =
    map (roots2poly) rootsList
    where
    rootsList =
        map genOne [1..]
        where
        genOne n =
            unGen rootGen qcGen (int n)
    qcGen = mkQCGen (int 148548830)
    
arbitraryRootMultiSet :: Integer -> Integer -> Gen [(Rational, RootMultiplicity)]
arbitraryRootMultiSet rootMultiSetSize coeffSize =
    do
    roots <- vectorOf (int rootMultiSetSize) (resize (int coeffSize) arbitraryRational) 
    multiplicities <- vectorOf (length (roots)) arbitrary
    return $ trimToOnly rootMultiSetSize $ zip roots multiplicities
    where
    trimToOnly _n [] = []
    trimToOnly 0 _ = []
    trimToOnly n ((r,RootMultiplicity m) : rest)
        | n < m = [(r, RootMultiplicity n)]
        | otherwise = (r, RootMultiplicity m) : (trimToOnly (n - m) rest)

    
arbitraryRootSet :: Integer -> Integer -> Gen [(Rational, RootMultiplicity)]
arbitraryRootSet rootSetSize coeffSize =
    do
    roots <- vectorOf (int $ rootSetSize * 2) (resize (int coeffSize) arbitraryRational)
    return $ map (\r -> (r, RootMultiplicity 1)) $ take (int rootSetSize) $ List.nub roots
    