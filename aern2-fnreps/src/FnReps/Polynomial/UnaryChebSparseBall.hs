module FnReps.Polynomial.UnaryChebSparseBall 
(
    UnaryChebSparseBall,
    getMaxDegree,
    setMaxDegree,
    getThresholdNormLog,
    setThresholdNormLog,
    setMaxDegreeNormLog,
    Degree
)
where

import AERN2.Real
import FnReps.Polynomial.UnaryChebSparse

_example1 :: UnaryChebSparseBall
_example1 = 
    UnaryChebSparseBall poly 100 NormZero
    where
    poly = fromList [(0, rationalBall 1.0),(1, rationalBall (1/100)),(3, rationalBall 1.0)]
    rationalBall x = rational2BallP p x
    p = prec 100

_example1Reduced1 :: UnaryChebSparseBall
_example1Reduced1 = setMaxDegree 1 _example1

_example2 :: UnaryChebSparseBall
_example2 = _example1 * _example1

data UnaryChebSparseBall =
    UnaryChebSparseBall
    {
        ucsBall_poly :: UnaryChebSparse, -- enclosure over the domain [-1,1]
        ucsBall_domain :: (MPBall, MPBall), -- an interval; the domain to translate into
        ucsBall_maxDegree :: Degree,
        ucsBall_thresholdNormLog :: NormLog  
    }
    deriving (Show)

getMaxDegree :: UnaryChebSparseBall -> Degree
getMaxDegree = ucsBall_maxDegree

setMaxDegree :: Degree -> UnaryChebSparseBall -> UnaryChebSparseBall
setMaxDegree maxDegree b =
    b 
    { ucsBall_poly = update $ ucsBall_poly b, 
      ucsBall_maxDegree = maxDegree
    }
    where
    update
        | maxDegree < bMaxDegree = 
            reduceDegreeAndSweep maxDegree bThresholdNormLog
        | otherwise = id 
    bMaxDegree = ucsBall_maxDegree b
    bThresholdNormLog = ucsBall_thresholdNormLog b

getThresholdNormLog :: UnaryChebSparseBall -> NormLog
getThresholdNormLog = ucsBall_thresholdNormLog

setThresholdNormLog :: NormLog -> UnaryChebSparseBall -> UnaryChebSparseBall
setThresholdNormLog normLog b =
    b 
    { ucsBall_poly = update $ ucsBall_poly b, 
      ucsBall_thresholdNormLog = normLog
    }
    where
    update
        | normLog > bThresholdNormLog = 
            reduceDegreeAndSweep bMaxDegree normLog
        | otherwise = id 
    bMaxDegree = ucsBall_maxDegree b
    bThresholdNormLog = ucsBall_thresholdNormLog b


setMaxDegreeNormLog :: Degree -> NormLog -> UnaryChebSparseBall -> UnaryChebSparseBall
setMaxDegreeNormLog maxDegree normLog b =
    b 
    { ucsBall_poly = update $ ucsBall_poly b, 
      ucsBall_maxDegree = maxDegree,
      ucsBall_thresholdNormLog = normLog
    }
    where
    update
        | maxDegree < bMaxDegree || normLog > bThresholdNormLog = 
            reduceDegreeAndSweep maxDegree bThresholdNormLog
        | otherwise = id 
    bMaxDegree = ucsBall_maxDegree b
    bThresholdNormLog = ucsBall_thresholdNormLog b


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

instance CanSub UnaryChebSparseBall UnaryChebSparseBall
instance CanSubThis UnaryChebSparseBall UnaryChebSparseBall
instance CanSubSameType UnaryChebSparseBall
        
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
    