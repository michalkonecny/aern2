module FnReps.Polynomial.UnaryPower.IntPoly.Bench where

import AERN2.Num -- alternative Prelude
import qualified Prelude as P

import qualified Data.List as List
--import Data.Ratio

import System.Random (randomRIO)

import Math.NumberTheory.Logarithms (integerLog2)

import Criterion.Main

import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)
import Test.QuickCheck.Gen (Gen(..))

import FnReps.Polynomial.UnaryPower.IntPoly.Basics
import FnReps.Polynomial.UnaryPower.IntPoly.EvaluationRootFinding (isolateRoots)

import FnReps.Polynomial.UnaryPower.IntPoly.Tests

benchMain :: IO ()
benchMain = defaultMain 
    [
       bgroup "rootMultiSetSize" 
        [
            bench (show i) $ nfIO (benchmarkRootIsolationByRootMultiSetSize i)
            | i <- [1..6]
        ]
    ,
       bgroup "rootSetSize" 
        [
            bench (show i) $ nfIO (benchmarkRootIsolationByRootSetSize i)
            | i <- [1..6]
        ]
    ,
       bgroup "oneNRoot3OneRoots" 
        [
            bench (show i) $ nfIO (benchmarkRootIsolationOneNRoot3OneRoots i)
            | i <- [1..6]
        ]
    ,
       bgroup "oneNRootNOneRoots" 
        [
            bench (show i) $ nfIO (benchmarkRootIsolationOneNRootNOneRoots i)
            | i <- [1..6]
        ]
    ]

benchmarkRootIsolationByRootSetSize :: Integer -> IO [(Rational, Rational)]
benchmarkRootIsolationByRootSetSize rootSetSize =
    benchmarkRootIsolationUsingPolys $ polysWithRootSetSize rootSetSize 7
    
benchmarkRootIsolationOneNRootNOneRoots :: Integer -> IO [(Rational, Rational)]
benchmarkRootIsolationOneNRootNOneRoots n =
    benchmarkRootIsolationUsingPolys $ polysOneNRootNOneRoots n 7
    
benchmarkRootIsolationOneNRoot3OneRoots :: Integer -> IO [(Rational, Rational)]
benchmarkRootIsolationOneNRoot3OneRoots n =
    benchmarkRootIsolationUsingPolys $ polysOneNRoot3OneRoots n 7
    
benchmarkRootIsolationByRootMultiSetSize :: Integer -> IO [(Rational, Rational)]
benchmarkRootIsolationByRootMultiSetSize rootMultiSetSize =
    benchmarkRootIsolationUsingPolys $ polysWithRootMultiSetSize rootMultiSetSize 7
    
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
        
polysWithRootSetSize :: Integer -> Integer -> [IntPolyWithRoots]
polysWithRootSetSize rootSetSize coeffSize =
    polysFromGen coeffSize $ arbitraryRootSet rootSetSize  (10*coeffSize)

polysOneNRootNOneRoots :: Integer -> Integer -> [IntPolyWithRoots]
polysOneNRootNOneRoots n coeffSize =
    polysFromGen coeffSize $ arbitraryOneNRootNOneRoots n  (10*coeffSize)

polysOneNRoot3OneRoots :: Integer -> Integer -> [IntPolyWithRoots]
polysOneNRoot3OneRoots n coeffSize =
    polysFromGen coeffSize $ arbitraryOneNRoot3OneRoots n  (10*coeffSize)

polysWithRootMultiSetSize :: Integer -> Integer -> [IntPolyWithRoots]
polysWithRootMultiSetSize rootMultiSetSize coeffSize =
    polysFromGen coeffSize $ arbitraryRootMultiSet rootMultiSetSize (10*coeffSize)

polysFromGen :: 
    Integer -> Gen [(Rational, RootMultiplicity)] -> [IntPolyWithRoots]
polysFromGen coeffSize rootGen =
    map (roundCoefficients . roots2poly) rootsList
    where
    rootsList =
        map genOne [1..]
        where
        genOne n =
            unGen rootGen qcGen (int n)
    qcGen = mkQCGen (int 148548830)
    roundCoefficients (IntPolyWithRoots (IntPoly terms) denom roots) =
        IntPolyWithRoots polyRounded newDenom roots
        where
        newDenom = 2^coeffSize
        polyRounded = 
            fromList $ map roundCoeff $ terms_toList terms
            where
            roundCoeff (d,cf) = (d, roundOffBinaryDigits $ scaleToNewDenom cf)
            scaleToNewDenom cf = P.round (newDenom * cf/denom)
            roundOffBinaryDigits cf 
                -- keep up to coeffsize-many binary significant digits and set all others to 0 to simulate MPFR with precision coeffsize:
                | digits <= coeffSize = cf
                | otherwise =
                    unit * ((P.round $ cf / unit) :: Integer)
                where
                unit = 2^(digits - coeffSize)
                digits = 1 + (integer $ integerLog2 $ max 1 (abs cf))
                
    
arbitraryRootSet :: Integer -> Integer -> Gen [(Rational, RootMultiplicity)]
arbitraryRootSet rootSetSize coeffSize =
    do
    roots <- vectorOf (int $ rootSetSize * 2) (resize (int coeffSize) arbitraryRational)
    return $ map (\r -> (r, RootMultiplicity 1)) $ take (int rootSetSize) $ List.nub roots
    
arbitraryOneNRootNOneRoots :: Integer -> Integer -> Gen [(Rational, RootMultiplicity)]
arbitraryOneNRootNOneRoots n coeffSize =
    do
    roots <- vectorOf (int $ n * 2) (resize (int coeffSize) arbitraryRational)
    return $ trimAndAddMultiplicities $ List.nub roots
    where
    trimAndAddMultiplicities [] = error "in arbitraryOneNRootNOneRoots" -- no non-exhaustive matches warning
    trimAndAddMultiplicities (r1 : roots) =
        (r1, RootMultiplicity n) : rest
        where
        rest =
            map (\r -> (r, RootMultiplicity 1)) $ take (int n) $ List.nub roots
    
arbitraryOneNRoot3OneRoots :: Integer -> Integer -> Gen [(Rational, RootMultiplicity)]
arbitraryOneNRoot3OneRoots n coeffSize =
    do
    roots <- vectorOf (int 6) (resize (int coeffSize) arbitraryRational)
    return $ trimAndAddMultiplicities $ List.nub roots
    where
    trimAndAddMultiplicities [] = error "in arbitraryOneNRootNOneRoots" -- no non-exhaustive matches warning
    trimAndAddMultiplicities (r1 : roots) =
        (r1, RootMultiplicity n) : rest
        where
        rest =
            map (\r -> (r, RootMultiplicity 1)) $ take (int 3) $ List.nub roots
    
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

    