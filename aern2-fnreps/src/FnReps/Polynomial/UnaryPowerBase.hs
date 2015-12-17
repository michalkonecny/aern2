module FnReps.Polynomial.UnaryPowerBase where

import AERN2.Real

{-|
    An unary interval polynomial in the power base.  This type exists mainly for printing
    of unary interval polynomials in a more advanced basis.  The operations are not efficient. 
-}
data UnaryPowerBase = 
    UnaryPowerBase [MPBall] -- coefficients of degree 0,1,2..  has to have at least one
    
instance CanNeg UnaryPowerBase where
    neg (UnaryPowerBase terms) = UnaryPowerBase $ map neg terms
    
instance CanNegSameType UnaryPowerBase

instance CanAdd UnaryPowerBase UnaryPowerBase where
    (UnaryPowerBase termsL) `add` (UnaryPowerBase termsR) 
        | length termsL < length termsR =
            UnaryPowerBase $ zipWith (+) (termsL ++ repeat (integer2Ball 0)) termsR
        | otherwise =
            UnaryPowerBase $ zipWith (+) termsL (termsR ++ repeat (integer2Ball 0))

instance CanAddThis UnaryPowerBase UnaryPowerBase
instance CanAddSameType UnaryPowerBase
    
instance CanSub UnaryPowerBase UnaryPowerBase
instance CanSubThis UnaryPowerBase UnaryPowerBase
instance CanSubSameType UnaryPowerBase

instance CanMul UnaryPowerBase UnaryPowerBase where
    (UnaryPowerBase termsL) `mul` (UnaryPowerBase termsR) =
        UnaryPowerBase $ terms
        where
        terms = foldl1 (zipWith (+)) (map mulTermsLBy $ zip [0..] termsR)
        mulTermsLBy (deg, coeff) = 
            (replicate (toInt deg) (integer2Ball 0)) ++ map (* coeff) termsL

instance CanMulBy UnaryPowerBase UnaryPowerBase
instance CanMulSameType UnaryPowerBase
    
instance HasEq UnaryPowerBase UnaryPowerBase
instance HasOrder UnaryPowerBase UnaryPowerBase
instance HasIntegers UnaryPowerBase where
    integer = error "HasIntegers.integer not yet implemented for UnaryPowerBase"
    
instance Ring UnaryPowerBase


    