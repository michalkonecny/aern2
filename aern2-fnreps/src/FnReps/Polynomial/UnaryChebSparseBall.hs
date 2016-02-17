{-# LANGUAGE UndecidableInstances #-}
module FnReps.Polynomial.UnaryChebSparseBall 
(
    -- * examples
    ucsBall_2,
    ucsBall_x,
    eval_ucsBall_x,
    -- * data type definition
    UnaryChebSparseBall,
    getMaxDegree,
    setMaxDegree,
    defaultMaxDegree,
    getThresholdNormLog,
    setThresholdNormLog,
    defaultSweepThresholdNormLog,
    setMaxDegreeNormLog,
    Degree
)
where

import AERN2.Num
import AERN2.RealFunction

import Control.Arrow

import FnReps.Polynomial.UnaryChebSparse

{- examples -}

_ucsBall1 :: UnaryChebSparseBall
_ucsBall1 = 
    UnaryChebSparseBall poly (Interval (-1.0) 1.0) 100 NormZero
    where
    poly = fromList [(0, b 1.0),(1, b (1/100)),(3, b 1.0)]
    b x = rational2BallP (prec 100) x

_ucsBall1Reduced1 :: UnaryChebSparseBall
_ucsBall1Reduced1 = setMaxDegree 1 _ucsBall1

_ucsBall2 :: UnaryChebSparseBall
_ucsBall2 = _ucsBall1 * _ucsBall1

ucsBall_2 :: UnaryChebSparseBall
ucsBall_2 =
    c2
    where
    c2 = constUnaryFnA (Interval 0.0 2.0, mpBall 2) 

ucsBall_x :: UnaryChebSparseBall
ucsBall_x =
    x
    where
    x = projUnaryFnA (Interval 0.0 2.0) 

eval_ucsBall_x :: Rational -> MPBall
eval_ucsBall_x v =
    evalAtInPointUnaryFnA (ucsBall_x, v)

{- type definition -} 

data UnaryChebSparseBall =
    UnaryChebSparseBall
    {
        ucsBall_poly :: UnaryChebSparse, -- enclosure over the domain [-1,1]
--        ucsBall_coeffPrec :: Precision,
        ucsBall_domain :: Interval Rational, -- an interval; the domain to translate into
        ucsBall_maxDegree :: Degree,
        ucsBall_sqeepThresholdNormLog :: NormLog  
    }
    deriving (Show)

--defaultCoeffPrecision :: Precision
--defaultCoeffPrecision = prec 100

defaultMaxDegree :: Degree
defaultMaxDegree = 100

defaultSweepThresholdNormLog :: NormLog
defaultSweepThresholdNormLog = NormZero

--getCoeffPrecision :: UnaryChebSparseBall -> Precision
--getCoeffPrecision = ucsBall_coeffPrec

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
    bThresholdNormLog = ucsBall_sqeepThresholdNormLog b

getThresholdNormLog :: UnaryChebSparseBall -> NormLog
getThresholdNormLog = ucsBall_sqeepThresholdNormLog

setThresholdNormLog :: NormLog -> UnaryChebSparseBall -> UnaryChebSparseBall
setThresholdNormLog normLog b =
    b 
    { ucsBall_poly = update $ ucsBall_poly b, 
      ucsBall_sqeepThresholdNormLog = normLog
    }
    where
    update
        | normLog > bThresholdNormLog = 
            reduceDegreeAndSweep bMaxDegree normLog
        | otherwise = id 
    bMaxDegree = ucsBall_maxDegree b
    bThresholdNormLog = ucsBall_sqeepThresholdNormLog b


setMaxDegreeNormLog :: Degree -> NormLog -> UnaryChebSparseBall -> UnaryChebSparseBall
setMaxDegreeNormLog maxDegree normLog b =
    b 
    { ucsBall_poly = update $ ucsBall_poly b, 
      ucsBall_maxDegree = maxDegree,
      ucsBall_sqeepThresholdNormLog = normLog
    }
    where
    update
        | maxDegree < bMaxDegree || normLog > bThresholdNormLog = 
            reduceDegreeAndSweep maxDegree bThresholdNormLog
        | otherwise = id 
    bMaxDegree = ucsBall_maxDegree b
    bThresholdNormLog = ucsBall_sqeepThresholdNormLog b


{- basic function operations -} 

instance
    (ArrowReal to MPBall, ArrowReal to CauchyReal) => 
    RealUnaryFnA to UnaryChebSparseBall
    where
    type UnaryFnIn UnaryChebSparseBall = Rational
    type UnaryFnOut UnaryChebSparseBall = MPBall
    getDomainUnaryFnA =
        arr ucsBall_domain
    constUnaryFnA =
        proc (dom, value) ->
            do
            poly <- constUnaryFnA -< (ucsFixedDomain, value)
            let maxDeg = defaultMaxDegree
            let sweepThreshold = defaultSweepThresholdNormLog
            returnA -< UnaryChebSparseBall poly dom maxDeg sweepThreshold 
    projUnaryFnA =
        proc dom@(Interval l r) ->
            do
            a1 <- convertA -< (r-l)/2
            a0 <- convertA -< (r+l)/2
--            let p = (getPrecision a0) `max` (getPrecision a1)
            let maxDeg = defaultMaxDegree
            let sweepThreshold = defaultSweepThresholdNormLog
            let poly = normaliseCoeffs $ fromList [(0,a0),(1,a1)]
            returnA -< UnaryChebSparseBall poly dom maxDeg sweepThreshold 
    evalOnIntervalUnaryFnA =
        error "UnaryChebSparseBall evalOnIntervalUnaryFnA not implemented yet"
    evalAtInPointUnaryFnA =
        proc (f, x) ->
            do
            xB <- convertA -< x
            evalAtOutPointUnaryFnA -< (f,xB)
    evalAtOutPointUnaryFnA =
        proc (fB, x) ->
            do
            let fP = ucsBall_poly fB
            let (Interval l r) = ucsBall_domain fB
            let xU = (2*x - r - l)/(r-l)
            case getAccuracy x of
                Exact ->
                    returnA -< evalDirectOnBall fP xU
                _ ->  
                    returnA -< evalLipschitzOnBall fP xU

{- pointwise arithmetic -} 


instance CanNegA (->) UnaryChebSparseBall where
    negA b = b { ucsBall_poly = neg (ucsBall_poly b) }
    
instance CanNegSameType UnaryChebSparseBall

instance CanAddA (->) UnaryChebSparseBall UnaryChebSparseBall where
    addA = ucsLift2 addAndReduce
        where
        addAndReduce maxDegree sqeepThresholdNormLog a b =
            reduceDegreeAndSweep maxDegree sqeepThresholdNormLog $ a + b

instance CanAddThis UnaryChebSparseBall UnaryChebSparseBall
instance CanAddSameType UnaryChebSparseBall

instance CanSub UnaryChebSparseBall UnaryChebSparseBall
instance CanSubThis UnaryChebSparseBall UnaryChebSparseBall
instance CanSubSameType UnaryChebSparseBall
        
instance CanMulA (->) UnaryChebSparseBall UnaryChebSparseBall where
    mulA = ucsLift2 addAndReduce
        where
        addAndReduce maxDegree sqeepThresholdNormLog a b =
            reduceDegreeAndSweep maxDegree sqeepThresholdNormLog $ a * b

instance CanMulBy UnaryChebSparseBall UnaryChebSparseBall
instance CanMulSameType UnaryChebSparseBall
        
ucsLift2 :: 
    (Degree -> NormLog -> UnaryChebSparse -> UnaryChebSparse -> UnaryChebSparse)
    -> 
    (UnaryChebSparseBall, UnaryChebSparseBall) -> UnaryChebSparseBall
ucsLift2 polyOpWithSizeLimits (a, b) =
    UnaryChebSparseBall
    {
        ucsBall_poly = polyOpWithSizeLimits maxDegree sqeepThresholdNormLog aPoly bPoly,
        ucsBall_domain = aDom,
        ucsBall_maxDegree = maxDegree,
        ucsBall_sqeepThresholdNormLog = sqeepThresholdNormLog
    }
    where
    maxDegree = max (ucsBall_maxDegree a) (ucsBall_maxDegree b)
    sqeepThresholdNormLog = min (ucsBall_sqeepThresholdNormLog a) (ucsBall_sqeepThresholdNormLog b)
    aPoly = ucsBall_poly a
    bPoly = ucsBall_poly b
    aDom = ucsBall_domain a
    _bDom = ucsBall_domain b -- TODO check for domain equality
    