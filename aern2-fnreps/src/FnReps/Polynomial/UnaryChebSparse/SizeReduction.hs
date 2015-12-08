module FnReps.Polynomial.UnaryChebSparse.SizeReduction 

where

import FnReps.Polynomial.UnaryChebSparse.Basics

{-|
    Drop all terms that whose degree is above the given limit or whose norm is at or below the threshold.
    Compensate for the drops in the constant term.
-}
reduceDegreeAndSweep :: Degree -> NormLog -> UnaryChebSparse -> UnaryChebSparse
reduceDegreeAndSweep maxDegree thresholdNormLog (UnaryChebSparse terms) =
    UnaryChebSparse $ reduceDegreeAndSweepTerms maxDegree thresholdNormLog terms
    
{-|
    Drop all terms that whose degree is above the given limit or whose norm is at or below the threshold.
    Compensate for the drops in the constant term.
-}
reduceDegreeAndSweepTerms :: Degree -> NormLog -> Terms -> Terms
reduceDegreeAndSweepTerms maxDegree thresholdNormLog terms 
    | terms_size koTerms == 0 = terms
    | otherwise = 
        terms_insertWith (+) 0 errorBall (terms_filter isOK terms)
    where
    errorBall =
        sum $ map (* unitInterval) $ terms_coeffs koTerms
        where 
        unitInterval = rationalBall2BallP (prec 2) (0.0,1.0) -- [0+-1]
    koTerms = terms_filter isNotOK terms
    isOK deg coeff =
        deg <= maxDegree && (deg == 0 || getNormLog coeff > thresholdNormLog)
    isNotOK deg coeff = 
        not $ isOK deg coeff
    