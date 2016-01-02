{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module FnReps.Polynomial.UnaryPowerBase 
(UnaryPowerBase(..))
where

import AERN2.Num

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
    
instance CanNegA (->) UnaryPowerBase where
    negA (UnaryPowerBase terms) = UnaryPowerBase $ map neg terms
    
instance CanNegSameType UnaryPowerBase

instance CanAddA (->) UnaryPowerBase UnaryPowerBase where
    addA (UnaryPowerBase termsL, UnaryPowerBase termsR) =
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

instance CanMulA (->) UnaryPowerBase UnaryPowerBase where
    mulA (UnaryPowerBase termsL, UnaryPowerBase termsR) =
        UnaryPowerBase $ terms
        where
        terms = foldl1 addTerms (map mulTermsLBy $ zip [0..] termsR)
        mulTermsLBy (deg, coeff) = 
            (replicate (int deg) (integer2Ball 0)) ++ map (* coeff) termsL

instance CanMulBy UnaryPowerBase UnaryPowerBase
instance CanMulSameType UnaryPowerBase
    
instance HasEqA (->) UnaryPowerBase UnaryPowerBase where
    equalToA = error "UnaryPowerBase equalTo not implemented yet."
instance HasOrderA (->) UnaryPowerBase UnaryPowerBase where
    lessThanA = error "UnaryPowerBase lessThan not implemented yet."
    leqA = error "UnaryPowerBase leq not implemented yet."

instance ConvertibleA (->) Integer UnaryPowerBase where
    convertA n = UnaryPowerBase [mpBall n]
    
instance Ring UnaryPowerBase


instance CanAddA (->) UnaryPowerBase Integer where
    type AddTypeA (->) UnaryPowerBase Integer = UnaryPowerBase
    addA (UnaryPowerBase (c : rest), n) =
        UnaryPowerBase (n + c : rest)  
    addA (UnaryPowerBase [], _) =
        error "UnaryPowerBase has to have a constant term."  
instance CanAddA (->) Integer UnaryPowerBase where
    type AddTypeA (->) Integer UnaryPowerBase = UnaryPowerBase
    addA (n, p) = p + n 
instance CanAddThis UnaryPowerBase Integer

instance CanSub UnaryPowerBase Integer
instance CanSub Integer UnaryPowerBase
instance CanSubThis UnaryPowerBase Integer

instance CanMulA (->) UnaryPowerBase Integer where
    type MulTypeA (->) UnaryPowerBase Integer = UnaryPowerBase
    mulA (UnaryPowerBase terms, n) =
        UnaryPowerBase (map (* n) terms)  
instance CanMulA (->) Integer UnaryPowerBase where
    type MulTypeA (->) Integer UnaryPowerBase = UnaryPowerBase
    mulA (n, p) = p * n 
instance CanMulBy UnaryPowerBase Integer

instance CanDivA (->) UnaryPowerBase Integer where
    type DivTypeA (->) UnaryPowerBase Integer = UnaryPowerBase
    divA (UnaryPowerBase terms, n) =
        UnaryPowerBase (map (/ n) terms)  
instance CanDivBy UnaryPowerBase Integer

instance CanAddMulScalar UnaryPowerBase Integer
instance CanAddMulDivScalar UnaryPowerBase Integer


instance CanAddA (->) UnaryPowerBase Rational where
    type AddTypeA (->) UnaryPowerBase Rational = UnaryPowerBase
    addA (UnaryPowerBase (c : rest), n) =
        UnaryPowerBase (n + c : rest)  
    addA (UnaryPowerBase [], _) =
        error "UnaryPowerBase has to have a constant term."  
instance CanAddA (->) Rational UnaryPowerBase where
    type AddTypeA (->) Rational UnaryPowerBase = UnaryPowerBase
    addA (n, p) = p + n 
instance CanAddThis UnaryPowerBase Rational

instance CanSub UnaryPowerBase Rational
instance CanSub Rational UnaryPowerBase
instance CanSubThis UnaryPowerBase Rational

instance CanMulA (->) UnaryPowerBase Rational where
    type MulTypeA (->) UnaryPowerBase Rational = UnaryPowerBase
    mulA (UnaryPowerBase terms, n) =
        UnaryPowerBase (map (* n) terms)  
instance CanMulA (->) Rational UnaryPowerBase where
    type MulTypeA (->) Rational UnaryPowerBase = UnaryPowerBase
    mulA (n, p) = p * n 
instance CanMulBy UnaryPowerBase Rational

instance CanDivA (->) UnaryPowerBase Rational where
    type DivTypeA (->) UnaryPowerBase Rational = UnaryPowerBase
    divA (UnaryPowerBase terms, n) =
        UnaryPowerBase (map (/ n) terms)  
instance CanDivBy UnaryPowerBase Rational

instance CanAddMulScalar UnaryPowerBase Rational
instance CanAddMulDivScalar UnaryPowerBase Rational

instance CanAddA (->) UnaryPowerBase MPBall where
    type AddTypeA (->) UnaryPowerBase MPBall = UnaryPowerBase
    addA (UnaryPowerBase (c : rest), n) =
        UnaryPowerBase (n + c : rest)  
    addA (UnaryPowerBase [], _) =
        error "UnaryPowerBase has to have a constant term."  
instance CanAddA (->) MPBall UnaryPowerBase where
    type AddTypeA (->) MPBall UnaryPowerBase = UnaryPowerBase
    addA (n, p) = p + n 
instance CanAddThis UnaryPowerBase MPBall

instance CanSub UnaryPowerBase MPBall
instance CanSub MPBall UnaryPowerBase
instance CanSubThis UnaryPowerBase MPBall

instance CanMulA (->) UnaryPowerBase MPBall where
    type MulTypeA (->) UnaryPowerBase MPBall = UnaryPowerBase
    mulA (UnaryPowerBase terms, n) =
        UnaryPowerBase (map (* n) terms)  
instance CanMulA (->) MPBall UnaryPowerBase where
    type MulTypeA (->) MPBall UnaryPowerBase = UnaryPowerBase
    mulA (n, p) = p * n 
instance CanMulBy UnaryPowerBase MPBall

instance CanDivA (->) UnaryPowerBase MPBall where
    type DivTypeA (->) UnaryPowerBase MPBall = UnaryPowerBase
    divA (UnaryPowerBase terms, n) =
        UnaryPowerBase (map (/ n) terms)  
instance CanDivBy UnaryPowerBase MPBall

instance CanAddMulScalar UnaryPowerBase MPBall
instance CanAddMulDivScalar UnaryPowerBase MPBall
