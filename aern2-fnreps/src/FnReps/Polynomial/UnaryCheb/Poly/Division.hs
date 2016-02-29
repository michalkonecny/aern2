module FnReps.Polynomial.UnaryCheb.Poly.Division where

import Math.NumberTheory.Logarithms (integerLog2)

import FnReps.Polynomial.UnaryCheb.Poly.Basics
import FnReps.Polynomial.UnaryCheb.Poly.DCTMultiplication


divideDCT_terms :: Terms -> Terms -> Terms
divideDCT_terms termsA termsB =
    terms_fromList $ zip [0..] (c0Double / 2 : c)
    {- 
        TODO: add an estimate of the interpolation error
    -}
    where
    (c0Double : c) = map (* (2 / cN)) (tDCT_I_nlogn cT) -- interpolate the values using a polynomial 
     
    cT = zipWith (/) aT bT -- division of the cN+1 values of the polynomials on the grid
    
    aT = tDCT_I_nlogn a -- compute the values of the polynomial termsA on a grid
    bT = tDCT_I_nlogn b -- compute the values of the polynomial termsB on a grid
    
    -- convert from sparse to dense representation:
    a = pad0 $ (2 * a0) : [terms_lookupCoeff termsA i | i <- [1..dA]]
    a0 = terms_lookupCoeff termsA 0
    b = pad0 $ (2 * b0) : [terms_lookupCoeff termsB i | i <- [1..dB]]
    b0 = terms_lookupCoeff termsB 0
    pad0 list = take (int $ cN + 1) $ list ++ (repeat (mpBall 0))
    
    cN = 2 ^ (1 + (integer $ integerLog2 $ max 1 (dA + dB)))
    dA = maximum $ terms_degrees termsA
    dB = maximum $ terms_degrees termsB


