{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module FnReps.Polynomial.UnaryPowerBase 
(UnaryPowerBase(..))
where

import AERN2.Real

import qualified Data.List as List

{-|
    An unary interval polynomial in the power base.  This type exists mainly for printing
    of unary interval polynomials in a more advanced basis.  The operations are not efficient. 
-}
data UnaryPowerBase = 
    UnaryPowerBase [MPBall] -- coefficients of degree 0,1,2..  has to have at least one
    
instance Show UnaryPowerBase where
    show (UnaryPowerBase terms) =
        List.intercalate " + " $
            map showTerm $ filter (not . coeffIsZero) $ reverse $ zip [0..] terms
        where
        coeffIsZero (_, coeff) = 
            (coeff == 0) == Just True
        showTerm (deg, coeff) = show coeff ++ showPower
            where
            showPower 
                | deg == 0 = ""
                | deg == 1 = "*x"
                | otherwise = "*x^" ++ show deg  
    
instance CanNeg UnaryPowerBase where
    neg (UnaryPowerBase terms) = UnaryPowerBase $ map neg terms
    
instance CanNegSameType UnaryPowerBase

instance CanAdd UnaryPowerBase UnaryPowerBase where
    (UnaryPowerBase termsL) `add` (UnaryPowerBase termsR) =
        UnaryPowerBase (termsL `addTerms` termsR) 

addTerms :: [MPBall] -> [MPBall] -> [MPBall]
addTerms termsL termsR
    | length termsL < length termsR =
        zipWith (+) (termsL ++ repeat (integer2Ball 0)) termsR
    | otherwise =
        zipWith (+) termsL (termsR ++ repeat (integer2Ball 0))

instance CanAddThis UnaryPowerBase UnaryPowerBase
instance CanAddSameType UnaryPowerBase
    
instance CanSub UnaryPowerBase UnaryPowerBase
instance CanSubThis UnaryPowerBase UnaryPowerBase
instance CanSubSameType UnaryPowerBase

instance CanMul UnaryPowerBase UnaryPowerBase where
    (UnaryPowerBase termsL) `mul` (UnaryPowerBase termsR) =
        UnaryPowerBase $ terms
        where
        terms = foldl1 addTerms (map mulTermsLBy $ zip [0..] termsR)
        mulTermsLBy (deg, coeff) = 
            (replicate (toInt deg) (integer2Ball 0)) ++ map (* coeff) termsL

instance CanMulBy UnaryPowerBase UnaryPowerBase
instance CanMulSameType UnaryPowerBase
    
instance HasEq UnaryPowerBase UnaryPowerBase where
    equalTo = error "UnaryPowerBase equalTo not implemented yet."
instance HasOrder UnaryPowerBase UnaryPowerBase where
    lessThan = error "UnaryPowerBase lessThan not implemented yet."
    leq = error "UnaryPowerBase leq not implemented yet."
instance HasIntegers UnaryPowerBase where
    integer n = UnaryPowerBase [integer n]
    
instance Ring UnaryPowerBase


instance CanAdd UnaryPowerBase Integer where
    type AddType UnaryPowerBase Integer = UnaryPowerBase
    (UnaryPowerBase (c : rest)) `add` n =
        UnaryPowerBase (n + c : rest)  
    (UnaryPowerBase []) `add` _ =
        error "UnaryPowerBase has to have a constant term."  
instance CanAdd Integer UnaryPowerBase where
    type AddType Integer UnaryPowerBase = UnaryPowerBase
    n `add` p = p + n 
instance CanAddThis UnaryPowerBase Integer

instance CanSub UnaryPowerBase Integer
instance CanSub Integer UnaryPowerBase
instance CanSubThis UnaryPowerBase Integer

instance CanMul UnaryPowerBase Integer where
    type MulType UnaryPowerBase Integer = UnaryPowerBase
    (UnaryPowerBase terms) `mul` n =
        UnaryPowerBase (map (* n) terms)  
instance CanMul Integer UnaryPowerBase where
    type MulType Integer UnaryPowerBase = UnaryPowerBase
    n `mul` p = p * n 
instance CanMulBy UnaryPowerBase Integer

instance CanDiv UnaryPowerBase Integer where
    type DivType UnaryPowerBase Integer = UnaryPowerBase
    (UnaryPowerBase terms) `div` n =
        UnaryPowerBase (map (/ n) terms)  
instance CanDivBy UnaryPowerBase Integer

instance CanAddMulScalar UnaryPowerBase Integer
instance CanAddMulDivScalar UnaryPowerBase Integer


instance CanAdd UnaryPowerBase Rational where
    type AddType UnaryPowerBase Rational = UnaryPowerBase
    (UnaryPowerBase (c : rest)) `add` n =
        UnaryPowerBase (n + c : rest)  
    (UnaryPowerBase []) `add` _ =
        error "UnaryPowerBase has to have a constant term."  
instance CanAdd Rational UnaryPowerBase where
    type AddType Rational UnaryPowerBase = UnaryPowerBase
    n `add` p = p + n 
instance CanAddThis UnaryPowerBase Rational

instance CanSub UnaryPowerBase Rational
instance CanSub Rational UnaryPowerBase
instance CanSubThis UnaryPowerBase Rational

instance CanMul UnaryPowerBase Rational where
    type MulType UnaryPowerBase Rational = UnaryPowerBase
    (UnaryPowerBase terms) `mul` n =
        UnaryPowerBase (map (* n) terms)  
instance CanMul Rational UnaryPowerBase where
    type MulType Rational UnaryPowerBase = UnaryPowerBase
    n `mul` p = p * n 
instance CanMulBy UnaryPowerBase Rational

instance CanDiv UnaryPowerBase Rational where
    type DivType UnaryPowerBase Rational = UnaryPowerBase
    (UnaryPowerBase terms) `div` n =
        UnaryPowerBase (map (/ n) terms)  
instance CanDivBy UnaryPowerBase Rational

instance CanAddMulScalar UnaryPowerBase Rational
instance CanAddMulDivScalar UnaryPowerBase Rational

instance CanAdd UnaryPowerBase MPBall where
    type AddType UnaryPowerBase MPBall = UnaryPowerBase
    (UnaryPowerBase (c : rest)) `add` n =
        UnaryPowerBase (n + c : rest)  
    (UnaryPowerBase []) `add` _ =
        error "UnaryPowerBase has to have a constant term."  
instance CanAdd MPBall UnaryPowerBase where
    type AddType MPBall UnaryPowerBase = UnaryPowerBase
    n `add` p = p + n 
instance CanAddThis UnaryPowerBase MPBall

instance CanSub UnaryPowerBase MPBall
instance CanSub MPBall UnaryPowerBase
instance CanSubThis UnaryPowerBase MPBall

instance CanMul UnaryPowerBase MPBall where
    type MulType UnaryPowerBase MPBall = UnaryPowerBase
    (UnaryPowerBase terms) `mul` n =
        UnaryPowerBase (map (* n) terms)  
instance CanMul MPBall UnaryPowerBase where
    type MulType MPBall UnaryPowerBase = UnaryPowerBase
    n `mul` p = p * n 
instance CanMulBy UnaryPowerBase MPBall

instance CanDiv UnaryPowerBase MPBall where
    type DivType UnaryPowerBase MPBall = UnaryPowerBase
    (UnaryPowerBase terms) `div` n =
        UnaryPowerBase (map (/ n) terms)  
instance CanDivBy UnaryPowerBase MPBall

instance CanAddMulScalar UnaryPowerBase MPBall
instance CanAddMulDivScalar UnaryPowerBase MPBall
