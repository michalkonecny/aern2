{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FnReps.Polynomial.UnaryPower.IntPoly.Tests where

import AERN2.Num -- alternative Prelude

import qualified Data.List as List
import Data.Ratio

import Test.QuickCheck

import FnReps.Polynomial.UnaryPower.IntPoly.Basics
import FnReps.Polynomial.UnaryPower.IntPoly.EvaluationRootFinding

data IntPolyWithRoots =
    IntPolyWithRoots
    {
        intPolyWithRoots_poly :: IntPoly,
        intPolyWithRoots_denominator :: Integer,
        intPolyWithRoots_roots :: [Rational]
    }
    deriving (Show)

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
        rootsPre <- longerListOf 5 arbitraryRational
        let roots = List.nub $ List.sort rootsPre -- remove duplicate roots
        -- multiplicities for the roots:
        multiplicities <- vectorOf (length roots) arbitrary 
        let rootsM = applyMuliplicities multiplicities roots
        -- TODO: generate binomials with no real roots
        return $ roots2poly rootsM
        where
        applyMuliplicities (RootMultiplicity m:ms) (r:rs) =
            (replicate (int m) r) ++ (applyMuliplicities ms rs)
        applyMuliplicities _ _ = []
        longerListOf offset gen = 
            sized $ \s -> resize (int (offset+(integer s))) $ listOf (resize s gen)
        roots2poly roots =
            IntPolyWithRoots poly denom roots
            where
            poly = List.foldl' (*) (fromList [(0,1)]) monomials
            monomials = map (\r -> fromList [(0, numerator $ -r*denom), (1,denom)]) roots
            denominators = map denominator roots
            denom = foldl lcm 1 denominators 
            

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
        growingElements $ 
            map RootMultiplicity [i | i <- [1..10], _repetition <- [1..(11-i)*2-1]]
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
    
    