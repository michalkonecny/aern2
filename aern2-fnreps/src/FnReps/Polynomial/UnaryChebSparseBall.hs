module FnReps.Polynomial.UnaryChebSparseBall 
(
    UnaryChebSparseBall,
    Degree
)
where

import AERN2.Real
import FnReps.Polynomial.UnaryChebSparse

data UnaryChebSparseBall =
    UnaryChebSparseBall
    {
        ucsBall_poly :: UnaryChebSparse,
        ucsBall_maxDegree :: Degree,
        ucsBall_thresholdNormLog :: NormLog  
    }

instance CanNeg UnaryChebSparseBall where
    neg b = b { ucsBall_poly = neg (ucsBall_poly b) }
    
instance CanNegSameType UnaryChebSparseBall

instance CanAdd UnaryChebSparseBall UnaryChebSparseBall where
    add = ucsLift2 addAndReduce
        where
        addAndReduce maxDegree thresholdNormLog a b =
            reduceDegreeAndSweep maxDegree thresholdNormLog $ a + b

instance CanAddThis UnaryChebSparseBall UnaryChebSparseBall
instance CanAddSameType UnaryChebSparseBall
        
instance CanMul UnaryChebSparseBall UnaryChebSparseBall where
    mul = ucsLift2 addAndReduce
        where
        addAndReduce maxDegree thresholdNormLog a b =
            reduceDegreeAndSweep maxDegree thresholdNormLog $ a * b

instance CanMulBy UnaryChebSparseBall UnaryChebSparseBall
instance CanMulSameType UnaryChebSparseBall
        
ucsLift2 :: 
    (Degree -> NormLog -> UnaryChebSparse -> UnaryChebSparse -> UnaryChebSparse)
    -> 
    (UnaryChebSparseBall -> UnaryChebSparseBall -> UnaryChebSparseBall)
ucsLift2 polyOpWithSizeLimits a b =
    UnaryChebSparseBall
    {
        ucsBall_poly = polyOpWithSizeLimits maxDegree thresholdNormLog aPoly bPoly,
        ucsBall_maxDegree = maxDegree,
        ucsBall_thresholdNormLog = thresholdNormLog
    }
    where
    maxDegree = max (ucsBall_maxDegree a) (ucsBall_maxDegree b)
    thresholdNormLog = min (ucsBall_thresholdNormLog a) (ucsBall_thresholdNormLog b)
    aPoly = ucsBall_poly a
    bPoly = ucsBall_poly b
    