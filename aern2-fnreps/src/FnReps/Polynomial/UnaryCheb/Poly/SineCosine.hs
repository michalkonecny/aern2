module FnReps.Polynomial.UnaryCheb.Poly.SineCosine 
(
    sine_poly,
    _testSine10X, _testSine10Xe, _testSine10X2,
    _testSine10XSine20X2
)
where

--import AERN2.RealFunction
--import Control.Arrow

import qualified Data.Map as Map
import qualified Data.List as List

import FnReps.Polynomial.UnaryCheb.Poly.Basics
import FnReps.Polynomial.UnaryCheb.Poly.Cheb2Power (cheb2Power)
import FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding (sampledRange)
import FnReps.Polynomial.UnaryCheb.Poly.DCTMultiplication ()
import FnReps.Polynomial.UnaryCheb.Poly.SizeReduction (reduceDegreeAndSweep)

import Debug.Trace (trace)


shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id


_testSine10X :: Poly
_testSine10X = sine_poly 100 NormZero (10*x)
    where
    x = xPoly (prec 100)
    
{- http://fooplot.com/plot/les2ndy4cm -}
_testSine10Xe :: Poly
_testSine10Xe = sine_poly 100 NormZero (polyAddToRadius (10*x) (mpBall 0.1))
    where
    x = xPoly (prec 100)
    
_testSine10X2 :: Poly
_testSine10X2 = sine_poly 100 NormZero (10*x*x)
    where
    x = xPoly (prec 100)
    
_testSine10XSine20X2 :: Poly
_testSine10XSine20X2 =
    sine_poly 150 NormZero $
    (+ 10*x) $
    sine_poly 150 NormZero $
    20*x*x
    where
    x = xPoly (prec 100)

{-
    To compute sin(xC+-xE):
    
    * compute (rC+-rE) = range(xC)
    * compute k = round(rC/(pi/2))
    * compute sin or cos of txC = xC-k*pi/2 using Taylor series
      * use sin for even k and cos for odd k
      * which degree to use?
        * keep trying higher and higher degrees until
            * the accuracy of the result worsens
            * OR the accuracy of the result is 8x higher than xE
    * if k mod 4 = 2 then negate result
    * if k mod 4 = 3 then negate result
    * add xE to the error bound of the resulting polynomial
-}

sine_poly :: Degree -> NormLog -> Poly -> Poly
sine_poly maxDeg sweepT x =
    maybeTrace
    (
        "sine_poly:"
        ++ "\n maxDeg = " ++ show maxDeg
        ++ "\n xC = " ++ showAP xC
        ++ "\n xE = " ++ showB xE
        ++ "\n xAccuracy = " ++ show xAccuracy
        ++ "\n r = " ++ showB r
        ++ "\n k = " ++ show k
        ++ "\n txC = " ++ showAP txC
        ++ "\n trM = " ++ showB trM
        ++ "\n taylorSumE = " ++ showB taylorSumE
        ++ "\n resC = " ++ showAP resC
    ) $
--    xPoly (prec 100) -- dummy
    res
    where
    showB = show . getApproximate (bits 30)
    showAP = show . getApproximate (bits 50) . cheb2Power

    -- first separate the centre of the polynomial x from and its radius:
    xC = polyCentre x
    xE = polyRadius x
    xAccuracy = getAccuracy x
    
    -- compute (rC+-rE) = range(xC):
    Interval rL rR =
        sampledRange (-1.0) (1.0) 10 xC
--        approxRange (-1.0) (1.0) (bits 10) xC
--        rangeOnIntervalUnaryFnA (xC, polyFixedDomain)
    r = endpoints2Ball rL rR
    rC = ballCentre r
    
    -- compute k = round(rC/(pi/2)):
    k = toIntegerDown $ 0.5 + (2*rC / piP)
    piP = piBallP p
    p = getPrecision x

    -- shift xC near 0 using multiples of pi/2:
    txC = xC - k * piP / 2
    -- work out an absolute range bound for txC:
    (_, trM) = ball2endpoints $ abs $ r - k * piP / 2  
    
    -- compute sin or cos of txC = xC-k*pi/2 using Taylor series:
    taylorSums 
        | even k = sineTaylorSeries maxDeg sweepT txC
        | otherwise = cosineTaylorSeries maxDeg sweepT txC
    (taylorSum, taylorSumE) = pickByAccuracy [] taylorSums
        where
        pickByAccuracy prevResults (_s@(p, e, n) : rest) =
            maybeTrace 
            ("pickByAccuracy: sE = " ++ showB sE ++ "; sAccuracy = " ++ show sAccuracy ++ "; prec = " ++ show (getPrecision pBest)) $ 
            pbAres
            where
            pbAres
                | tooAccurate || stoppedMakingProgress || sameAccuracyCount > 10 = (polyCentre pBest, sEBest)
                | otherwise =
                    pickByAccuracy ((sAccuracy, p, sE) : prevResults) rest
            tooAccurate = sAccuracy >= xAccuracy + 3
            prevAccuracies = map (\(a,_,_) -> a) prevResults
            sameAccuracyCount = 
                case List.findIndex (/= sAccuracy) prevAccuracies of Just i -> integer i; _ -> 0
            (stoppedMakingProgress, pBest, sEBest) =
                case prevResults of
                    ((a1,p1,sE1):(a2,_,_):(a3,_,_):(a4,_,_):_) 
                        | sAccuracy < a1 && a1 > a2 -> (True, p1, sE1) 
                        | sAccuracy == a1 && a1 == a2 && a2 == a3 && a3 == a4 -> (True, p, sE) 
                    _ -> (False, p, sE)
            sE = e*(trM^n) + (polyRadius p)
            sAccuracy = normLog2Accuracy $ getNormLog sE
        pickByAccuracy _ _ = error "internal error in SineCosine"
    -- if k mod 4 = 2 then negate result,
    -- if k mod 4 = 1 then negate result:
    resC 
        | k `mod` 4 == 2 = -taylorSum
        | k `mod` 4 == 3 = -taylorSum
        | otherwise = taylorSum
    -- add xE to the error bound of the resulting polynomial:
    res = resC `polyAddToRadius` (taylorSumE + xE)

{-
_testSineT i =
    (e,n, getApproximate (bits 30) $ cheb2Power $ p)
    where
    (p,e,n) = (sineTaylorSeries $ x) !!! i
    x = xPoly (prec 100)
_testCosineT i =
    (e,n, getApproximate (bits 30) $ cheb2Power $ p)
    where
    (p,e,n) = (cosineTaylorSeries $ x) !!! i
    x = xPoly (prec 100)
-}

{-|
    For a given polynomial @p@, compute all partial Taylor sums of @sin(p)@ and return
    them together with @e@, an error bound on @[-1,1]@, and a number @n@.
    The number @n@ can be used to obtain a better error bound on a domain @[-a,a]@
    for some @0 <= a < 1@.  The better error bound is @e*a^n@. 
-}
sineTaylorSeries :: Degree -> NormLog -> Poly -> [(Poly, Rational, Integer)]
sineTaylorSeries maxDeg sweepT x =
    let
    termComponents =
        iterate addNextTerm (0,1,1,6,Map.singleton 1 (x::Poly))
        where
        addNextTerm (prevI, prevN, _prevFact, currentFact, prevPowers) =
            (i, n, currentFact, nextFact, newPowers)
            where
            i = prevI + 1
            n = prevN + 2
            nextFact = currentFact*((n+1)*(n+2)) 
            newPowers = Map.insert n (reduce currentPower) prevPowers
            reduce = reduceDegreeAndSweep maxDeg NormZero
            currentPower 
                | odd i = x * (power i) * (power i)
                | otherwise = x * (power (i-1)) * (power (i+1))
                where
                power j = lookupForce j prevPowers 
    sumsAndErrors = 
        makeSums (constPoly (mpBall 0), 1) termComponents
        where
        makeSums (prevSum, sign) ((_i, n, nFact, nextFact, xPowers) : rest) =
            (newSum, 1/nextFact, n+2) : makeSums (newSum, -sign) rest
            where
            newSum = prevSum + sign*xPowN/nFact
            xPowN = lookupForce n xPowers
        makeSums _ _ = error "internal error in SineCosine.sineTaylorSeries"
    in
    sumsAndErrors
    
{-|
    For a given polynomial @p@, compute all partial Taylor sums of @cos(p)@ and return
    them together with @e@, an error bound on @p\in[-1,1]@, and a number @n@.
    The number @n@ can be used to obtain an error bound for @p\in[-a,a]@
    for some @0 <= a@.  The error bound is @e*a^n@. 
-}
cosineTaylorSeries :: Degree -> NormLog -> Poly -> [(Poly, Rational, Integer)]
cosineTaylorSeries maxDeg sweepT x =
    let
    termComponents =
        iterate addNextTerm (1,2,2,24,Map.singleton 2 (x*x :: Poly))
        where
        addNextTerm (prevI, prevN, _prevFact, currentFact, prevPowers) =
            (i, n, currentFact, nextFact, newPowers)
            where
            i = prevI + 1
            n = prevN + 2
            nextFact = currentFact*((n+1)*(n+2)) 
            newPowers = Map.insert n (reduce currentPower) prevPowers
            reduce = reduceDegreeAndSweep maxDeg NormZero
            currentPower 
                | even i = (power i) * (power i)
                | otherwise = (power (i-1)) * (power (i+1))
                where
                power j = lookupForce j prevPowers 
    sumsAndErrors = 
        makeSums (constPoly (mpBall 1), -1) termComponents
        where
        makeSums (prevSum, sign) ((_i, n, nFact, nextFact, xPowers) : rest) =
            (newSum, 1/nextFact, n+2) : makeSums (newSum, -sign) rest
            where
            newSum = prevSum + sign*xPowN/nFact
            xPowN = lookupForce n xPowers
        makeSums _ _ = error "internal error in SineCosine.cosineTaylorSeries"
    in
    sumsAndErrors
    
lookupForce :: Ord k => k -> Map.Map k a -> a
lookupForce j amap = 
    case Map.lookup j amap of 
        Just t -> t
        Nothing -> error "internal error in SineCosine.lookupForce"  

