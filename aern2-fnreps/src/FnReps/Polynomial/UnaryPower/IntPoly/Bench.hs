module FnReps.Polynomial.UnaryPower.IntPoly.Bench where

import AERN2.Num -- alternative Prelude
--import qualified Prelude as P

--import qualified Data.List as List
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
       bgroup "num of roots" 
        [
            bench (show i) $ nfIO (benchmarkRootIsolationByRootCount i)
            | i <- [1..10]
        ]
    ]

benchmarkRootIsolationByRootCount :: Integer -> IO [(Rational, Rational)]
benchmarkRootIsolationByRootCount numOfRoots =
    do
    index <- randomRIO (1,100)
    return $ runWithIndex index
    where
    runWithIndex index =
        map (\(Interval l r) -> (l,r)) isolateRootsResult
--        integer $ length isolateRootsResult
        where
        (IntPolyWithRoots intpoly _denom rootsMSorted) = 
            (polysOfSize numOfRoots 100) !!! index 
        rootsSorted = map fst rootsMSorted
        isolateRootsResult = isolateRoots l r intpoly
            where
            l = -1 + (minimum rootsSorted')
            r = 1 + (maximum rootsSorted')
            rootsSorted' 
                | null rootsSorted = [0.0]
                | otherwise = rootsSorted
        
    

polysOfSize :: Integer -> Integer -> [IntPolyWithRoots]
polysOfSize numOfRoots coeffSize =
    map (roots2poly) rootsList
    where
    rootsList =
        map genOne [1..]
        where
        genOne n =
            unGen (arbitraryRootsWith numOfRoots coeffSize) qcGen (int n)
    qcGen = mkQCGen (int 148548830)
    
arbitraryRootsWith :: Integer -> Integer -> Gen [(Rational, RootMultiplicity)]
arbitraryRootsWith numOfRoots coeffSize =
    do
    roots <- vectorOf (int numOfRoots) (resize (int coeffSize) arbitraryRational) 
    multiplicities <- vectorOf (length (roots)) arbitrary
    return $ trimToOnly numOfRoots $ zip roots multiplicities
    where
    trimToOnly _n [] = []
    trimToOnly 0 _ = []
    trimToOnly n ((r,RootMultiplicity m) : rest)
        | n < m = [(r, RootMultiplicity n)]
        | otherwise = (r, RootMultiplicity m) : (trimToOnly (n - m) rest)
    