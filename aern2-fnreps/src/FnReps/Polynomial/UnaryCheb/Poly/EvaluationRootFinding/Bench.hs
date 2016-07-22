module FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding.Bench 
(benchMainRange)
where

import AERN2.Num -- alternative Prelude
--import qualified Prelude as P

import qualified Data.List as List
--import Data.Ratio

import System.Random (randomRIO)
--import Control.DeepSeq (force)

--import Math.NumberTheory.Logarithms (integerLog2)

import Criterion.Main

import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)
import Test.QuickCheck.Gen (Gen(..))

--import FnReps.Polynomial.UnaryCheb.Poly.Basics
import FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding
import FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding.Tests

import System.IO.Unsafe (unsafePerformIO)

benchMainRange :: IO ()
benchMainRange = defaultMain
    [ 
       bgroup "12RootsAccuracy" 
        [
            bench (show i) $
                nf 
                    (benchmarkRange 
                        ((rootsWithRootMultiSetSize 12) 
                            !!! (unsafePerformIO $ randomRIO (1,maxIndex)))) 
                    (bits i)
            | i <- [20,40..80]
        ]
      ,
       bgroup "rootMultiSetSize" 
        [
            bench (show i) $ 
                nf 
                    (benchmarkRange 
                        ((rootsWithRootMultiSetSize i) 
                            !!! (unsafePerformIO $ randomRIO (1,maxIndex)))) 
                    (bits 60)
            | i <- [4,6..12]
        ]
    ,
       bgroup "rootSetSize" 
        [
            bench (show i) $ 
                nf 
                    (benchmarkRange 
                        ((rootsWithRootSetSize i) 
                            !!! (unsafePerformIO $ randomRIO (1,maxIndex)))) 
                    (bits 60)
            | i <- [4,6..12]
        ]
    ,
       bgroup "oneNRoot2OneRoots" 
        [
            bench (show i) $
                nf 
                    (benchmarkRange 
                        ((rootsOneNRoot2OneRoots i) 
                            !!! (unsafePerformIO $ randomRIO (1,maxIndex)))) 
                    (bits 60)
            | i <- [2,4..10]
        ]
    ,
       bgroup "oneNRootNOneRoots" 
        [
            bench (show i) $
                nf 
                    (benchmarkRange 
                        ((rootsOneNRootNOneRoots i) 
                            !!! (unsafePerformIO $ randomRIO (1,maxIndex)))) 
                    (bits 60)
            | i <- [2,3..6]
        ]
    ]

maxIndex :: Integer
maxIndex = 100
defaultRootSize :: Integer
defaultRootSize = 100

benchmarkRange :: 
    [(Rational, RootMultiplicity)] -> Accuracy -> Rational
benchmarkRange roots accuracy =
    getRationalRadius $ 
    findAccurate $ map rangeWithPrec precisions
    where
    getRationalRadius (Interval l r) =
        toRationalUp (ballRadius l) + toRationalUp (ballRadius r)
    precisions = map (\n -> prec (accuracyI + n)) $ fib 5 10
    accuracyI = fromAccuracy accuracy
    fib a b = a : (fib b (a + b))
    rangeWithPrec p =
        approxRange (-1.0) (1.0) rangeAcc poly
        where
        PolyWithRoots poly _ = roots2poly p roots
        rangeAcc = bits (prec2integer p - 20) 
            -- TODO: ideally range would take this to be the same as our accuracy parameter
    findAccurate [] = error "in benchmarkRange"
    findAccurate (x@(Interval l r) : xs) 
        | getAccuracy l >= accuracy && getAccuracy r >= accuracy = x
        | otherwise = findAccurate xs  

rootsWithRootMultiSetSize :: Integer -> [[(Rational, RootMultiplicity)]]
rootsWithRootMultiSetSize rootMultiSetSize =
    rootsFromGen $ arbitraryRootMultiSet rootMultiSetSize defaultRootSize

rootsWithRootSetSize :: Integer -> [[(Rational, RootMultiplicity)]]
rootsWithRootSetSize rootSetSize =
    rootsFromGen $ arbitraryRootSet rootSetSize defaultRootSize

rootsOneNRootNOneRoots :: Integer -> [[(Rational, RootMultiplicity)]]
rootsOneNRootNOneRoots n =
    rootsFromGen $ arbitraryOneNRootNOneRoots n defaultRootSize

rootsOneNRoot2OneRoots :: Integer -> [[(Rational, RootMultiplicity)]]
rootsOneNRoot2OneRoots n =
--    force $ take (int $ maxIndex + 1) $
    rootsFromGen $ arbitraryOneNRoot2OneRoots n defaultRootSize

rootsFromGen ::
    Gen [(Rational, RootMultiplicity)] -> [[(Rational, RootMultiplicity)]]
rootsFromGen rootGen =
    rootsList
    where
    rootsList =
        map genOne [1..]
        where
        genOne n =
            unGen rootGen qcGen (int n)
    qcGen = mkQCGen (int 148548830)

    
arbitraryRootSet :: Integer -> Integer -> Gen [(Rational, RootMultiplicity)]
arbitraryRootSet rootSetSize rootSize =
    do
    roots <- vectorOf (int $ rootSetSize * 2) (resize (int rootSize) arbitraryRational)
    return $ map (\r -> (r, RootMultiplicity 1)) $ take (int rootSetSize) $ List.nub roots
    
arbitraryOneNRootNOneRoots :: Integer -> Integer -> Gen [(Rational, RootMultiplicity)]
arbitraryOneNRootNOneRoots n rootSize =
    do
    roots <- vectorOf (int $ n * 2) (resize (int rootSize) arbitraryRational)
    return $ trimAndAddMultiplicities $ List.nub roots
    where
    trimAndAddMultiplicities [] = error "in arbitraryOneNRootNOneRoots" -- no non-exhaustive matches warning
    trimAndAddMultiplicities (r1 : roots) =
        (r1, RootMultiplicity n) : rest
        where
        rest =
            map (\r -> (r, RootMultiplicity 1)) $ take (int n) $ List.nub roots
    
arbitraryOneNRoot2OneRoots :: Integer -> Integer -> Gen [(Rational, RootMultiplicity)]
arbitraryOneNRoot2OneRoots n rootSize =
    do
    roots <- vectorOf (int 6) (resize (int rootSize) arbitraryRational)
    return $ trimAndAddMultiplicities $ List.nub roots
    where
    trimAndAddMultiplicities [] = error "in arbitraryOneNRootNOneRoots" -- no non-exhaustive matches warning
    trimAndAddMultiplicities (r1 : roots) =
        (r1, RootMultiplicity n) : rest
        where
        rest =
            map (\r -> (r, RootMultiplicity 1)) $ take (int 2) $ List.nub roots
    
arbitraryRootMultiSet :: Integer -> Integer -> Gen [(Rational, RootMultiplicity)]
arbitraryRootMultiSet rootMultiSetSize rootSize =
    do
    roots <- vectorOf (int rootMultiSetSize) (resize (int rootSize) arbitraryRational) 
    multiplicities <- vectorOf (length (roots)) arbitrary
    return $ trimToOnly rootMultiSetSize $ zip roots multiplicities
    where
    trimToOnly _n [] = []
    trimToOnly 0 _ = []
    trimToOnly n ((r,RootMultiplicity m) : rest)
        | n < m = [(r, RootMultiplicity n)]
        | otherwise = (r, RootMultiplicity m) : (trimToOnly (n - m) rest)

    