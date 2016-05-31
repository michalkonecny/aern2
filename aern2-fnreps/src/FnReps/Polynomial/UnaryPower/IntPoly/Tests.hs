{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FnReps.Polynomial.UnaryPower.IntPoly.Tests where

import AERN2.Num -- alternative Prelude
import qualified Prelude as P

import qualified Data.List as List
import Data.Ratio

import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)

import FnReps.Polynomial.UnaryPower.IntPoly.Basics
import FnReps.Polynomial.UnaryPower.IntPoly.EvaluationRootFinding

import Debug.Trace

data IntPolyWithRoots =
    IntPolyWithRoots
    {
        intPolyWithRoots_poly :: IntPoly,
        intPolyWithRoots_denominator :: Integer,
        intPolyWithRoots_rootsSorted :: [(Rational, RootMultiplicity)]
    }
    deriving (Show)

testIsolateRootsRepeatable :: Integer -> Bool -> IO ()
testIsolateRootsRepeatable seedI isVerbose 
    | isVerbose =
        verboseCheckWith args isolateRootsIsCorrect
    | otherwise =
        quickCheckWith args isolateRootsIsCorrect
    where
    seed = int seedI
    args = stdArgs { replay = Just (mkQCGen seed, int 1), maxSuccess = int 100 }

isolateRootsIsCorrect :: IntPolyWithRoots -> Property
isolateRootsIsCorrect (IntPolyWithRoots intpoly _denom rootsMSorted) =
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

{-
    Selection of real polynomials + their roots.
    
    First, randomly select: 
        * a small number of rational monomials (roots)
        * a small number of rational binomials (complex root pairs) 
        * the multiplicity of these roots/root pairs

    Then multiply these monomials and binomials,
    then convert integer polynomial + denominator of the form 2^n.
-}

instance Arbitrary IntPolyWithRoots where
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

roots2poly :: [(Rational, RootMultiplicity)] -> IntPolyWithRoots
roots2poly roots =
    IntPolyWithRoots poly denom roots
    where
    poly = List.foldl' (*) (fromList [(0,1)]) monomials
    monomials = concat $ map monoms roots
        where
        monoms (root, RootMultiplicity i) = 
            replicate (int i) $ 
                fromList [(1,denom), (0, numerator $ -root*denom)] -- (x - root)^i
    denominators = map (denominator . fst) roots
    denom = List.foldl' lcm 1 denominators 
    

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


{- TODO The following section in not needed in this file. It should be moved. -}  

{- Selection of real numbers. 
    Integers and rational number should be generated with a significant probability.
    -1,0,1,pi,2pi,pi/2 should be also generated with a significant probability.
    Sometimes randomly generate an integer part + signed-binary sequence.
-}

newtype CauchyRealForArbitrary = CauchyRealForArbitrary CauchyReal

data SignedBinaryDigit = SBPos | SBZer | SBNeg
data SignedBinaryReal =
    SignedBinaryReal
    {
        sbReal_integerPart :: Integer,
        sbReal_digits :: [SignedBinaryDigit]
    }
    
    