{-# LANGUAGE UndecidableInstances #-}
module FnReps.Polynomial.UnaryCheb.PolyBall 
(
    -- * examples
    ball_2,
    ball_x,
    eval_ball_x,
    -- * data type definition
    PolyBall(..),
    ApproxPolyBall(..),
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

import FnReps.Polynomial.UnaryCheb.Poly
import qualified FnReps.Polynomial.UnaryPower.Poly as PowerBasis (translate, scale)
import Data.Ratio

{- examples -}

_ball1 :: PolyBall
_ball1 = 
    PolyBall poly (Interval (-1.0) 1.0) 100 NormZero
    where
    poly = fromList [(0, b 1.0),(1, b (1/100)),(3, b 1.0)]
    b x = rational2BallP (prec 100) x

_ball1Reduced1 :: PolyBall
_ball1Reduced1 = setMaxDegree 1 _ball1

_ball2 :: PolyBall
_ball2 = _ball1 * _ball1

ball_2 :: PolyBall
ball_2 =
    c2
    where
    c2 = constUnaryFnA (Interval 0.0 2.0, mpBall 2) 

ball_x :: PolyBall
ball_x =
    x
    where
    x = projUnaryFnA (Interval 0.0 2.0) 

eval_ball_x :: Rational -> MPBall
eval_ball_x v =
    evalAtInPointUnaryFnA (ball_x, v)

{- type definition -} 

data PolyBall =
    PolyBall
    {
        ball_poly :: Poly, -- enclosure over the domain [-1,1]
--        ball_coeffPrec :: Precision,
        ball_domain :: Interval Rational, -- an interval; the domain to translate into
        ball_maxDegree :: Degree,
        ball_sweepThresholdNormLog :: NormLog  
    }

instance Show PolyBall where
    show (PolyBall poly dom maxDeg sweepT) =
        "(\\x∈" ++ showDom ++ " -> " ++ show polyOnDom 
            ++ " {mxd:" ++ show maxDeg ++ ";swp:" ++ show sweepT ++ "})"
        where
        polyOnDom =
            PowerBasis.translate ((rD+lD)/2) $
            PowerBasis.scale (2/(rD-lD)) $
            cheb2Power poly
        (Interval lD rD) = dom
        showDom =
            "[" ++ showQ lD ++ "," ++ showQ rD ++ "]"
        showQ q     
            | numerator q == 0 = "0"
            | denominator q == 1 = show $ numerator q
            | otherwise = (show $ numerator q) ++ "/" ++ (show $ denominator q)

data ApproxPolyBall = ApproxPolyBall Accuracy PolyBall

instance HasApproximate PolyBall where
    type Approximate PolyBall = ApproxPolyBall
    getApproximate = ApproxPolyBall

instance Show ApproxPolyBall where
    show (ApproxPolyBall ac (PolyBall poly dom maxDeg sweepT)) =
        "(\\x∈" ++ showDom ++ " -> " ++ show (getApproximate ac polyOnDom) 
            ++ " {mxd:" ++ show maxDeg ++ ";swp:" ++ show sweepT ++ "})"
        where
        polyOnDom =
            PowerBasis.translate ((rD+lD)/2) $
            PowerBasis.scale (2/(rD-lD)) $
            cheb2Power poly
        (Interval lD rD) = dom
        showDom =
            "[" ++ showQ lD ++ "," ++ showQ rD ++ "]"
        showQ q     
            | numerator q == 0 = "0"
            | denominator q == 1 = show $ numerator q
            | otherwise = (show $ numerator q) ++ "/" ++ (show $ denominator q)
    

--defaultCoeffPrecision :: Precision
--defaultCoeffPrecision = prec 100

defaultMaxDegree :: Degree
defaultMaxDegree = 100

defaultSweepThresholdNormLog :: NormLog
defaultSweepThresholdNormLog = NormZero

--getCoeffPrecision :: PolyBall -> Precision
--getCoeffPrecision = ball_coeffPrec

getMaxDegree :: PolyBall -> Degree
getMaxDegree = ball_maxDegree

setMaxDegree :: Degree -> PolyBall -> PolyBall
setMaxDegree maxDegree b =
    b 
    { ball_poly = update $ ball_poly b, 
      ball_maxDegree = maxDegree
    }
    where
    update
        | maxDegree < bMaxDegree = 
            reduceDegreeAndSweep maxDegree bThresholdNormLog
        | otherwise = id 
    bMaxDegree = ball_maxDegree b
    bThresholdNormLog = ball_sweepThresholdNormLog b

getThresholdNormLog :: PolyBall -> NormLog
getThresholdNormLog = ball_sweepThresholdNormLog

setThresholdNormLog :: NormLog -> PolyBall -> PolyBall
setThresholdNormLog normLog b =
    b 
    { ball_poly = update $ ball_poly b, 
      ball_sweepThresholdNormLog = normLog
    }
    where
    update
        | normLog > bThresholdNormLog = 
            reduceDegreeAndSweep bMaxDegree normLog
        | otherwise = id 
    bMaxDegree = ball_maxDegree b
    bThresholdNormLog = ball_sweepThresholdNormLog b


setMaxDegreeNormLog :: Degree -> NormLog -> PolyBall -> PolyBall
setMaxDegreeNormLog maxDegree normLog b =
    b 
    { ball_poly = update $ ball_poly b, 
      ball_maxDegree = maxDegree,
      ball_sweepThresholdNormLog = normLog
    }
    where
    update
        | maxDegree < bMaxDegree || normLog > bThresholdNormLog = 
            reduceDegreeAndSweep maxDegree bThresholdNormLog
        | otherwise = id 
    bMaxDegree = ball_maxDegree b
    bThresholdNormLog = ball_sweepThresholdNormLog b


instance HasPrecision PolyBall
    where
    getPrecision = getPrecision . ball_poly

instance HasAccuracy PolyBall
    where
    getAccuracy = getAccuracy . ball_poly

{- basic function operations -} 

instance
    (ArrowReal to MPBall, ArrowReal to CauchyReal) => 
    RealUnaryFnA to PolyBall
    where
    type UnaryFnIn PolyBall = Rational
    type UnaryFnOut PolyBall = MPBall
    getDomainUnaryFnA =
        arr ball_domain
    constUnaryFnA =
        proc (dom, value) ->
            do
            poly <- constUnaryFnA -< (ucsFixedDomain, value)
            let maxDeg = defaultMaxDegree
            let sweepThreshold = defaultSweepThresholdNormLog
            returnA -< PolyBall poly dom maxDeg sweepThreshold 
    projUnaryFnA =
        proc dom@(Interval l r) ->
            do
            a1 <- convertA -< (r-l)/2
            a0 <- convertA -< (r+l)/2
--            let p = (getPrecision a0) `max` (getPrecision a1)
            let maxDeg = defaultMaxDegree
            let sweepThreshold = defaultSweepThresholdNormLog
            let poly = normaliseCoeffs $ fromList [(0,a0),(1,a1)]
            returnA -< PolyBall poly dom maxDeg sweepThreshold 
    evalOnIntervalUnaryFnA =
        error "UnaryChebSparse.PolyBall evalOnIntervalUnaryFnA not implemented yet"
    evalAtInPointUnaryFnA =
        proc (f, x) ->
            do
            xB <- convertA -< x
            evalAtOutPointUnaryFnA -< (f,xB)
    evalAtOutPointUnaryFnA =
        proc (fB, x) ->
            do
            let fP = ball_poly fB
            let (Interval l r) = ball_domain fB
            let xU = (2*x - r - l)/(r-l)
            case getAccuracy x of
                Exact ->
                    returnA -< evalDirectOnBall fP xU
                _ ->  
                    returnA -< evalLipschitzOnBall fP xU

{- pointwise arithmetic -} 


instance CanNegA (->) PolyBall where
    negA b = b { ball_poly = neg (ball_poly b) }
    
instance CanNegSameType PolyBall

instance CanAddA (->) PolyBall PolyBall where
    addA = ucsLift2 addAndReduce
        where
        addAndReduce maxDegree sweepThresholdNormLog a b =
            reduceDegreeAndSweep maxDegree sweepThresholdNormLog $ a + b

instance CanAddThis PolyBall PolyBall
instance CanAddSameType PolyBall

instance CanSub PolyBall PolyBall
instance CanSubThis PolyBall PolyBall
instance CanSubSameType PolyBall
        
instance CanMulA (->) PolyBall PolyBall where
    mulA = ucsLift2 addAndReduce
        where
        addAndReduce maxDegree sweepThresholdNormLog a b =
            reduceDegreeAndSweep maxDegree sweepThresholdNormLog $ a * b

instance CanMulBy PolyBall PolyBall
instance CanMulSameType PolyBall
        
ucsLift2 :: 
    (Degree -> NormLog -> Poly -> Poly -> Poly)
    -> 
    (PolyBall, PolyBall) -> PolyBall
ucsLift2 polyOpWithSizeLimits (a, b) =
    PolyBall
    {
        ball_poly = polyOpWithSizeLimits maxDegree sweepThresholdNormLog aPoly bPoly,
        ball_domain = aDom,
        ball_maxDegree = maxDegree,
        ball_sweepThresholdNormLog = sweepThresholdNormLog
    }
    where
    maxDegree = max (ball_maxDegree a) (ball_maxDegree b)
    sweepThresholdNormLog = min (ball_sweepThresholdNormLog a) (ball_sweepThresholdNormLog b)
    aPoly = ball_poly a
    bPoly = ball_poly b
    aDom = ball_domain a
    _bDom = ball_domain b -- TODO check for domain equality
    