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
    getDegree,
    Degree
)
where

import AERN2.Num
import AERN2.RealFunction

import Control.Arrow

import FnReps.Polynomial.UnaryCheb.Poly
import qualified FnReps.Polynomial.UnaryPower.Poly as PowerBasis (translate, scale)
import Data.Ratio

import Debug.Trace (trace)


shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id

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

_ball_DivDCT :: Degree -> PolyBall -> PolyBall -> PolyBall
_ball_DivDCT d a b =
    ucsLift2 (divideDCT_poly d) (a, b)
--    where
--    maxDeg = getDegree a `max` getDegree b

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

getDegree :: PolyBall -> Degree
getDegree = poly_degree . ball_poly

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
defaultMaxDegree = 10

defaultSweepThresholdNormLog :: NormLog
defaultSweepThresholdNormLog = NormBits (-100)

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
            poly <- constUnaryFnA -< (polyFixedDomain, value)
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
    evalOnIntervalUnaryFnA = arr aux
        where
        aux (fB, Interval lDom rDom) =
            evalOnIntervalUnaryFnA (ball_poly fB, Interval lUnit rUnit)
            where
            lUnit = (2 * lDom - domL - domR) / (domR - domL)
            rUnit = (2 * rDom - domL - domR) / (domR - domL)
            (Interval domL domR) = ball_domain fB
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
    addA = ucsLift2 (+)

instance CanAddThis PolyBall PolyBall
instance CanAddSameType PolyBall

instance CanSub PolyBall PolyBall
instance CanSubThis PolyBall PolyBall
instance CanSubSameType PolyBall
        
instance CanMulA (->) PolyBall PolyBall where
    mulA = ucsLift2 (*)

instance CanMulBy PolyBall PolyBall
instance CanMulSameType PolyBall
        
instance CanDivA (->) PolyBall PolyBall where
    type DivTypeA (->) PolyBall PolyBall = PolyBall
    divA (b1,b2) =
        maybeTrace ("divA: degree = " ++ show degree) $
        ucsLift2 (divideDCT_poly degree) (b1,b2)
        where
        degree = (ball_maxDegree b1) `max` (ball_maxDegree b2)
        
instance CanDivA (->) Integer PolyBall where
    type DivTypeA (->) Integer PolyBall = PolyBall
    divA (n, a) = nP / a 
        where
        nP = constUnaryFnA (ball_domain a, mpBall n) :: PolyBall
    
instance CanDivA (->) Rational PolyBall where
    type DivTypeA (->) Rational PolyBall = PolyBall
    divA (n, a) = nP / a 
        where
        nP = constUnaryFnA (ball_domain a, mpBall n) :: PolyBall
    
instance CanDivA (->) MPBall PolyBall where
    type DivTypeA (->) MPBall PolyBall = PolyBall
    divA (n, a) = nP / a
        where
        nP = constUnaryFnA (ball_domain a, n) :: PolyBall
    

{- Mixed operations with Integer -}
    
instance CanAddA (->) PolyBall Integer where
    type AddTypeA (->) PolyBall Integer = PolyBall
    addA (a, n) = ucsLift1 (+n) a
    
instance CanAddA (->) Integer PolyBall where
    type AddTypeA (->) Integer PolyBall = PolyBall
    addA (n, a) = ucsLift1 (+n) a

instance CanAddThis PolyBall Integer

instance CanSub PolyBall Integer
instance CanSubThis PolyBall Integer

instance CanSubA (->) Integer PolyBall where
    type SubTypeA (->) Integer PolyBall = PolyBall
    subA (n, a) = addA (n, neg a)

instance CanMulA (->) PolyBall Integer where
    type MulTypeA (->) PolyBall Integer = PolyBall
    mulA (a, n) = ucsLift1 (*n) a
    
instance CanMulA (->) Integer PolyBall where
    type MulTypeA (->) Integer PolyBall = PolyBall
    mulA (n, a) = ucsLift1 (*n) a

instance CanMulBy PolyBall Integer

instance CanDivA (->) PolyBall Integer where
    type DivTypeA (->) PolyBall Integer = PolyBall
    divA (a, n) = ucsLift1 (/n) a
    
instance CanDivBy PolyBall Integer

{- Mixed operations with Rational -}
    
instance CanAddA (->) PolyBall Rational where
    type AddTypeA (->) PolyBall Rational = PolyBall
    addA (a, n) = ucsLift1 (+n) a
    
instance CanAddA (->) Rational PolyBall where
    type AddTypeA (->) Rational PolyBall = PolyBall
    addA (n, a) = ucsLift1 (+n) a

instance CanAddThis PolyBall Rational

instance CanSub PolyBall Rational
instance CanSubThis PolyBall Rational

instance CanSubA (->) Rational PolyBall where
    type SubTypeA (->) Rational PolyBall = PolyBall
    subA (n, a) = addA (n, neg a)

instance CanMulA (->) PolyBall Rational where
    type MulTypeA (->) PolyBall Rational = PolyBall
    mulA (a, n) = ucsLift1 (*n) a
    
instance CanMulA (->) Rational PolyBall where
    type MulTypeA (->) Rational PolyBall = PolyBall
    mulA (n, a) = ucsLift1 (*n) a

instance CanMulBy PolyBall Rational

instance CanDivA (->) PolyBall Rational where
    type DivTypeA (->) PolyBall Rational = PolyBall
    divA (a, n) = ucsLift1 (/n) a
    
instance CanDivBy PolyBall Rational

{- Mixed operations with MPBall -}
    
instance CanAddA (->) PolyBall MPBall where
    type AddTypeA (->) PolyBall MPBall = PolyBall
    addA (a, n) = ucsLift1 (+n) a
    
instance CanAddA (->) MPBall PolyBall where
    type AddTypeA (->) MPBall PolyBall = PolyBall
    addA (n, a) = ucsLift1 (+n) a

instance CanAddThis PolyBall MPBall

instance CanSub PolyBall MPBall
instance CanSubThis PolyBall MPBall

instance CanSubA (->) MPBall PolyBall where
    type SubTypeA (->) MPBall PolyBall = PolyBall
    subA (n, a) = addA (n, neg a)

instance CanMulA (->) PolyBall MPBall where
    type MulTypeA (->) PolyBall MPBall = PolyBall
    mulA (a, n) = ucsLift1 (*n) a
    
instance CanMulA (->) MPBall PolyBall where
    type MulTypeA (->) MPBall PolyBall = PolyBall
    mulA (n, a) = ucsLift1 (*n) a

instance CanMulBy PolyBall MPBall

instance CanDivA (->) PolyBall MPBall where
    type DivTypeA (->) PolyBall MPBall = PolyBall
    divA (a, n) = ucsLift1 (/n) a
    
instance CanDivBy PolyBall MPBall

{- utilities -}

ucsLift1 :: 
    (Poly -> Poly)
    -> 
    PolyBall -> PolyBall
ucsLift1 polyOpWithSizeLimits a =
    PolyBall
    {
        ball_poly = reduceDegreeAndSweep maxDegree sweepThresholdNormLog $ polyOpWithSizeLimits aPoly,
        ball_domain = aDom,
        ball_maxDegree = maxDegree,
        ball_sweepThresholdNormLog = sweepThresholdNormLog
    }
    where
    maxDegree = ball_maxDegree a
    sweepThresholdNormLog = ball_sweepThresholdNormLog a
    aPoly = ball_poly a
    aDom = ball_domain a

        
ucsLift2 :: 
    (Poly -> Poly -> Poly)
    -> 
    (PolyBall, PolyBall) -> PolyBall
ucsLift2 polyOpWithSizeLimits (a, b) =
    PolyBall
    {
        ball_poly = 
            reduceDegreeAndSweep maxDegree sweepThresholdNormLog $ polyOpWithSizeLimits aPoly bPoly,
        ball_domain = aDom,
        ball_maxDegree = maxDegree,
        ball_sweepThresholdNormLog = sweepThresholdNormLog
    }
    where
    maxDegree = (ball_maxDegree a) `max` (ball_maxDegree b)
    sweepThresholdNormLog = min (ball_sweepThresholdNormLog a) (ball_sweepThresholdNormLog b)
    aPoly = ball_poly a
    bPoly = ball_poly b
    aDom = ball_domain a
    _bDom = ball_domain b -- TODO check for domain equality
    