{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
module FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding.Tests where

import AERN2.Num -- alternative Prelude
import qualified Prelude as P

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import qualified Data.List as List
--import Data.Ratio

import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)

import FnReps.Polynomial.UnaryCheb.Poly.Basics
import FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding

import Debug.Trace

data PolyWithRoots =
    PolyWithRoots
    {
        intPolyWithRoots_poly :: Poly,
        intPolyWithRoots_rootsSorted :: [(Rational, RootMultiplicity)]
    }
    deriving (Show, Generic)

instance NFData PolyWithRoots

{- TODO: adapt for testing range instead of root isolation

testIsolateRootsRepeatable :: Integer -> Bool -> IO ()
testIsolateRootsRepeatable seedI isVerbose 
    | isVerbose =
        verboseCheckWith args isolateRootsIsCorrect
    | otherwise =
        quickCheckWith args isolateRootsIsCorrect
    where
    seed = int seedI
    args = stdArgs { replay = Just (mkQCGen seed, int 1), maxSuccess = int 100 }

isolateRootsIsCorrect :: PolyWithRoots -> Property
isolateRootsIsCorrect (PolyWithRoots intpoly _denom rootsMSorted) =
--    trace ("isolateRootsIsCorrect: "
--        ++ "\n intpoly = " ++ show intpoly
--        ++ "\n rootsSorted = " ++ show rootsSorted
--        ++ "\n isolateRootsResult = " ++ show isolateRootsResult
--    ) $
    allRootsContainedInResult
    .&&.
    eachIntervalContainsOneRoot
    where
    allRootsContainedInResult =
        and [ inResult root | root <- rootsSorted ]
    inResult root =
        or [ root `containedIn` interval | interval <- isolateRootsResult ]
    
    eachIntervalContainsOneRoot =
        and [ hasOneRoot interval | interval <- isolateRootsResult ]
    hasOneRoot interval =
        oneTrue [ root `containedIn` interval | root <- rootsSorted ]
        where
        oneTrue [] = False
        oneTrue (x:xs) 
            | x = and (map not xs)
            | otherwise = oneTrue xs
    
    containedIn :: Rational -> (Interval Rational) -> Bool
    a `containedIn` (Interval l r) = l <= a && a <= r
          
    isolateRootsResult = isolateRoots l r intpoly
        where
        l = -1 + (minimum rootsSorted')
        r = 1 + (maximum rootsSorted')
        rootsSorted' 
            | null rootsSorted = [0.0]
            | otherwise = rootsSorted
     
    rootsSorted = map fst rootsMSorted

instance Arbitrary PolyWithRoots where
    arbitrary =
        do
        -- a number of rational roots:
        rootsPre <- sizedListOf 1 0.25 arbitraryRational
        let roots = List.nub $ List.sort rootsPre -- remove duplicate roots
        -- multiplicities for the roots:
        multiplicities <- vectorOf (length (0.0 : roots)) arbitrary 
        -- TODO: generate binomials with no real roots
        return $ roots2poly $ zip roots multiplicities
        where
        sizedListOf offset scaling gen = 
            sized $ \s -> resize (P.round $ offset+(integer s)*scaling) $ listOf (resize s gen)

-}


roots2poly :: Precision -> [(Rational, RootMultiplicity)] -> PolyWithRoots
roots2poly p roots =
    PolyWithRoots poly roots
    where
    poly = List.foldl' (*) (fromList [(0,integer2BallP p 1)]) monomials
    monomials = concat $ map monoms roots
        where
        monoms (root, RootMultiplicity i) = 
            replicate (int i) $ 
                fromList [(1, integer2BallP p 1), (0, rational2BallP p $ -root)] -- (x - root)^i
    

arbitraryRational :: Gen Rational
arbitraryRational =
    do
    a <- scale (int . (\n -> n*n) . integer) arbitrary
    Positive b <- arbitrary
    let _ = [a,b] :: [Integer]
    return $ a/b

newtype RootMultiplicity = RootMultiplicity Integer
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

instance Arbitrary RootMultiplicity
    where
    arbitrary = 
        frequency $ 
            [(int $ (4-i)^3, elements [RootMultiplicity i]) | i <- [1..4]]
            -- 1 is more likely than 2 etc.
