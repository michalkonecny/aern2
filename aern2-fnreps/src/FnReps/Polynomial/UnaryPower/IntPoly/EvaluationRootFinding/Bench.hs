module FnReps.Polynomial.UnaryPower.IntPoly.EvaluationRootFinding.Bench 
(benchMainTwoStages, benchMainRootIsolation)
where

import AERN2.Num -- alternative Prelude
import qualified Prelude as P

import qualified Data.List as List
--import Data.Ratio

import System.Random (randomRIO)
import Control.DeepSeq (force)

import Math.NumberTheory.Logarithms (integerLog2)

import Criterion.Main

import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)
import Test.QuickCheck.Gen (Gen(..))

import FnReps.Polynomial.UnaryPower.IntPoly.Basics
import FnReps.Polynomial.UnaryPower.IntPoly.EvaluationRootFinding
import FnReps.Polynomial.UnaryPower.IntPoly.EvaluationRootFinding.Tests

import System.IO.Unsafe (unsafePerformIO)

benchMainTwoStages :: IO ()
benchMainTwoStages = defaultMain
    [
       bgroup "testTwoStages" 
        [
            bench (show i) $ nf (\ n -> (unsafePerformIO (factorialIO i))/n ) 3
                -- this should measure only the time it takes to divide by 3, not the factorial
                -- the factorials take around 1 millisecond while the divisions takes around 1 microsecond. 
            | i <- [1000,2000,3000]
        ]
    ]

factorialIO :: Integer -> IO Integer
factorialIO n = 
    do
    index <- randomRIO (1,100)
    return $ List.foldl' (*) (index - index +1) [1..n] -- hopefully ghc will not optimise the IO away... 


benchMainRootIsolation :: IO ()
benchMainRootIsolation = defaultMain
    [ 
--       bgroup "12RootsPolySelectionOnly"
--        [
--            bench (show coeffPrec) $
--                nf 
--                    (\p -> 
--                        (polysWithRootMultiSetSize 12 coeffPrec)
--                            !!! (unsafePerformIO $ randomRIO (1+p-p,maxIndex))) 
--                    coeffPrec
--            | coeffPrec <- [60]
--        ]
--      ,
--       bgroup "12RootsPolyOnly"
--        [
--            bench (show coeffPrec) $
--                nf 
--                    (\p -> 
--                        (polysFromGen p $ arbitraryRootMultiSet 12 coeffSize) !!! 0)
--                    coeffPrec
--            | coeffPrec <- [60]
--        ]
--      ,
       bgroup "12RootsCoeffPrec" 
        [
            bench (show coeffPrec) $
                nf 
                    (benchmarkRootIsolation 
                        ((polysWithRootMultiSetSize 12 coeffPrec) 
                            !!! (unsafePerformIO $ randomRIO (1,maxIndex)))) 
                    coeffPrec
            | coeffPrec <- [20,30..80]
        ]
      ,
       bgroup "rootMultiSetSize" 
        [
            bench (show i) $ 
                nf 
                    (benchmarkRootIsolation 
                        ((polysWithRootMultiSetSize i 60) 
                            !!! (unsafePerformIO $ randomRIO (1,maxIndex)))) 
                    i
            | i <- [4,6..12]
        ]
    ,
       bgroup "rootSetSize" 
        [
            bench (show i) $ 
                nf 
                    (benchmarkRootIsolation 
                        ((polysWithRootSetSize i 60) 
                            !!! (unsafePerformIO $ randomRIO (1,maxIndex)))) 
                    i
            | i <- [4,6..12]
        ]
    ,
       bgroup "oneNRoot2OneRoots" 
        [
            bench (show i) $
                nf 
                    (benchmarkRootIsolation 
                        ((polysOneNRoot2OneRoots i 60) 
                            !!! (unsafePerformIO $ randomRIO (1,maxIndex)))) 
                    i
            | i <- [2,4..10]
        ]
    ,
       bgroup "oneNRootNOneRoots" 
        [
            bench (show i) $
                nf 
                    (benchmarkRootIsolation 
                        ((polysOneNRootNOneRoots i 60) 
                            !!! (unsafePerformIO $ randomRIO (1,maxIndex)))) 
                    i
            | i <- [2..6]
        ]
    ]

maxIndex = 100
coeffSize = 100

benchmarkRootIsolation :: 
    IntPolyWithRoots -> Integer -> [(Rational, Rational)]
benchmarkRootIsolation poly dummy =
    map (\(Interval l r) -> (l,r)) isolateRootsResult
    where
    (IntPolyWithRoots intpoly _denom rootsMSorted) = poly 
    rootsSorted = map fst rootsMSorted
    isolateRootsResult = isolateRoots l r intpoly
        where
        l = -1 + (minimum rootsSorted')
        r = (dummy - dummy) + 1 + (maximum rootsSorted') -- it has to depend on dummy to prevent caching
        rootsSorted' 
            | null rootsSorted = [0.0]
            | otherwise = rootsSorted
        
polysWithRootSetSize :: Integer -> Integer -> [IntPolyWithRoots]
polysWithRootSetSize rootSetSize coeffPrec =
    force $ take (int $ maxIndex + 1) $
    polysFromGen coeffPrec $ arbitraryRootSet rootSetSize coeffSize

polysOneNRootNOneRoots :: Integer -> Integer -> [IntPolyWithRoots]
polysOneNRootNOneRoots n coeffPrec =
    force $ take (int $ maxIndex + 1) $
    polysFromGen coeffPrec $ arbitraryOneNRootNOneRoots n coeffSize

polysOneNRoot2OneRoots :: Integer -> Integer -> [IntPolyWithRoots]
polysOneNRoot2OneRoots n coeffPrec =
    force $ take (int $ maxIndex + 1) $
    polysFromGen coeffPrec $ arbitraryOneNRoot2OneRoots n coeffSize

polysWithRootMultiSetSize :: Integer -> Integer -> [IntPolyWithRoots]
polysWithRootMultiSetSize rootMultiSetSize coeffPrec =
    force $ take (int $ maxIndex + 1) $
    polysFromGen coeffPrec $ arbitraryRootMultiSet rootMultiSetSize coeffSize

polysFromGen :: 
    Integer -> Gen [(Rational, RootMultiplicity)] -> [IntPolyWithRoots]
polysFromGen coeffPrec rootGen =
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
        newDenom = 2^coeffPrec
        polyRounded = 
            fromList $ map roundCoeff $ terms_toList terms
            where
            roundCoeff (d,cf) = (d, roundOffBinaryDigits $ scaleToNewDenom cf)
            scaleToNewDenom cf = P.round (newDenom * cf/denom)
            roundOffBinaryDigits cf 
                -- keep up to coeffsize-many binary significant digits and set all others to 0 to simulate MPFR with precision coeffsize:
                | digits <= coeffPrec = cf
                | otherwise =
                    unit * ((P.round $ cf / unit) :: Integer)
                where
                unit = 2^(digits - coeffPrec)
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
    
arbitraryOneNRoot2OneRoots :: Integer -> Integer -> Gen [(Rational, RootMultiplicity)]
arbitraryOneNRoot2OneRoots n coeffSize =
    do
    roots <- vectorOf (int 6) (resize (int coeffSize) arbitraryRational)
    return $ trimAndAddMultiplicities $ List.nub roots
    where
    trimAndAddMultiplicities [] = error "in arbitraryOneNRootNOneRoots" -- no non-exhaustive matches warning
    trimAndAddMultiplicities (r1 : roots) =
        (r1, RootMultiplicity n) : rest
        where
        rest =
            map (\r -> (r, RootMultiplicity 1)) $ take (int 2) $ List.nub roots
    
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

    