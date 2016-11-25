{-|
    Module      :  AERN2.Poly.Cheb.SineCosine
    Description :  Sine and cosine for polynomials
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Sine and cosine for polynomials
-}

module AERN2.Poly.Cheb.SineCosine
-- (
-- )
where

import Numeric.MixedTypes
import qualified Prelude as P
-- import Text.Printf

import qualified Data.Map as Map
-- import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

-- import AERN2.Norm
import AERN2.MP.Accuracy
import AERN2.MP.ErrorBound
import AERN2.MP.Float
import AERN2.MP.Ball (MPBall, IsBall(..), IsInterval(..))
import qualified AERN2.MP.Ball as MPBall
-- import AERN2.MP.Dyadic

import AERN2.Real

import AERN2.Interval
import AERN2.RealFun.Operations
-- import AERN2.RealFun.UnaryFun

-- import AERN2.Poly.Basics

import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Ring ()
import AERN2.Poly.Cheb.Eval

import Debug.Trace (trace)


shouldTrace :: Bool
-- shouldTrace = False
shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace
    | shouldTrace = trace
    | otherwise = const id


_test10Xe :: ChPoly MPBall
_test10Xe =
    10*x
    where
    x :: ChPoly MPBall
    x = setPrecision (prec 100) $ varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (0.0,1.0)

_testSine10X :: Accuracy -> ChPoly MPBall
_testSine10X ac =
    sineWithAccuracyGuide ac (10*x)
    where
    x :: ChPoly MPBall
    x = varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (0.0,1.0)


{-

_testSine10X :: ChPoly MPBall
_testSine10X =
    sineWithPrecDegSweep (prec 100) 100 NormZero (10*x)
    where
    x :: ChPoly MPBall
    x = varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (0.0,1.0)

_testSine10Xe :: ChPoly MPBall
_testSine10Xe =
    sineWithPrecDegSweep (prec 100) 100 NormZero (updateRadius (+ (errorBound 0.1)) (10*x))
    where
    x :: ChPoly MPBall
    x = varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (0.0,1.0)

-}

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

sineWithAccuracyGuide ::
  Accuracy -> ChPoly MPBall -> ChPoly MPBall
sineWithAccuracyGuide = sineCosineWithAccuracyGuide True

cosineWithAccuracyGuide ::
  Accuracy -> ChPoly MPBall -> ChPoly MPBall
cosineWithAccuracyGuide = sineCosineWithAccuracyGuide False

sineCosineWithAccuracyGuide ::
  -- (Field c, CanMinMaxSameType c,
  --  CanAbsSameType c,
  --  CanAddSubMulDivBy c CauchyReal,
  --  ConvertibleExactly Dyadic c,
  --  HasNorm c,  CanRound c,
  --  IsBall c, IsInterval c c,
  --  CanApply (ChPoly c) c, ApplyType (ChPoly c) c ~ c)
  -- =>
  Bool -> Accuracy -> ChPoly MPBall -> ChPoly MPBall
sineCosineWithAccuracyGuide isSine acGuide x =
    maybeTrace
    (
        "ChPoly.sineCosine:"
        ++ "\n isSine = " ++ show isSine
        -- ++ "\n xC = " ++ showAP xC
        -- ++ "\n xE = " ++ showB xE
        ++ "\n xAccuracy = " ++ show xAccuracy
        ++ "\n r = " ++ show r
        -- ++ "\n r = " ++ showB r
        ++ "\n k = " ++ show k
        -- ++ "\n txC = " ++ showAP txC
        ++ "\n trM = " ++ show trM
        ++ "\n degree = " ++ show n
        -- ++ "\n getAccuracy taylorSum = " ++ show (getAccuracy taylorSum)
        -- ++ "\n taylorSumE = " ++ show taylorSumE
        ++ "\n getAccuracy result = " ++ show (getAccuracy res)
        -- ++ "\n resC = " ++ showAP resC
    ) $
--    xPoly (prec 100) -- dummy
    res
    where
    -- showB = show . getApproximate (bits 30)
    -- showAP = show . getApproximate (bits 50) . cheb2Power

    isCosine = not isSine

    -- first separate the centre of the polynomial x from its radius:
    xC = centre x
    xE = radius x
    xAccuracy = getAccuracy x

    -- compute (rC+-rE) = range(xC):
    Interval rL rR =
      sampledRange (dyadicInterval (-1.0,1.0)) 5 xC
    r = fromEndpoints rL (rR :: MPBall)
    rC = centreAsBall r :: MPBall

    -- compute k = round(rC/(pi/2)):
    k = fst $ MPBall.integerBounds $ 0.5 + (2*rC / pi)

    -- shift xC near 0 using multiples of pi/2:
    txC = xC - k * pi / 2
    -- work out an absolute range bound for txC:
    (_, trM :: MPBall) = endpoints $ abs $ r - k * pi / 2

    -- compute sin or cos of txC = xC-k*pi/2 using Taylor series:
    (taylorSum, taylorSumE, n)
        | isSine && even k = sineTaylorSum acGuide trM txC
        | isCosine && odd k = sineTaylorSum acGuide trM txC
        | otherwise = cosineTaylorSum acGuide trM txC
    -- if k mod 4 = 2 then negate result,
    -- if k mod 4 = 3 then negate result:
    km4 = k `mod` 4
    resC
        | isSine && 2 <= km4 && km4 <= 3 = -taylorSum
        | isCosine && 1 <= km4 && km4 <= 2 = -taylorSum
        | otherwise = taylorSum
    -- add xE to the error bound of the resulting polynomial:
    res = updateRadius (+ (taylorSumE + xE)) resC


{-|
    For a given polynomial @p@, compute a partial Taylor sum of @cos(p)@ and return
    it together with its error bound @e@ and the degree of the polynomial @n@.
-}
sineTaylorSum ::
  (Ring c, CanDivBy c Integer, IsInterval c c, HasAccuracy c, CanSetPrecision c)
  =>
  Accuracy -> MPBall -> ChPoly c -> (ChPoly c, ErrorBound, Integer)
sineTaylorSum = sineCosineTaylorSum True

{-|
    For a given polynomial @p@, compute a partial Taylor sum of @cos(p)@ and return
    it together with its error bound @e@ and the degree of the polynomial @n@.
-}
cosineTaylorSum ::
  (Ring c, CanDivBy c Integer, IsInterval c c, HasAccuracy c, CanSetPrecision c)
  =>
  Accuracy -> MPBall -> ChPoly c -> (ChPoly c, ErrorBound, Integer)
cosineTaylorSum = sineCosineTaylorSum False

sineCosineTaylorSum ::
  (Ring c, CanDivBy c Integer, IsInterval c c, HasAccuracy c, CanSetPrecision c)
  =>
  Bool -> Accuracy -> MPBall -> ChPoly c -> (ChPoly c, ErrorBound, Integer)
sineCosineTaylorSum isSine acGuide trM x =
    let
    _isCosine = not isSine
    termComponents
      | isSine = iterate addNextTerm (0,1,1,6,Map.singleton 1 xA)
      | otherwise = iterate addNextTerm (1,2,2,24,Map.singleton 2 xxA)
      where
      xA acLimit =
        reduceSetPrec acLimit x
      xxA acLimit =
        reduceDegreeWithLostAccuracyLimit acLimit $ xR*xR
        where
        xR = reduceSetPrec (acLimit + 1) x
      reduceSetPrec acLimit p =
        reduceDegreeWithLostAccuracyLimit acLimit $ setPrecisionAtLeastAccuracy acLimit p
      addNextTerm (prevI, prevN, _prevFact, currentFact, prevPowers) =
        (i, n, currentFact, nextFact, newPowers)
        where
        i = prevI + 1
        n = prevN + 2
        nextFact = currentFact*((n+1)*(n+2))
        newPowers = Map.insert n currentPower prevPowers
        currentPower acLimit
          | isSine && odd i = reduce $ x * (power i) * (power i)
          | isSine = reduce $ x * (power (i-1)) * (power (i+1))
          | even i = reduce $ (power i) * (power i)
          | otherwise = reduce $ (power (i-1)) * (power (i+1))
          where
          power j = lookupForce j prevPowers (acLimit+1)
          reduce = reduceDegreeWithLostAccuracyLimit acLimit
    sumAndError
      | isSine = makeSum (const $ chPoly (x,0), 1) termComponents
      | otherwise = makeSum (const $ chPoly (x,1), -1) termComponents
        where
        makeSum (prevSum, sign) ((_i, n, nFact, nextFact, xPowers) : rest)
          | accurateEnough = (newSum acGuide, e, n+2)
          | otherwise = makeSum (newSum, -sign) rest
          where
          accurateEnough = getAccuracy e >= acGuide
          e = errorBound $ (trM^n)/nextFact
          newSum acLimit = prevSum acLimit + sign*(xPowN acLimit)/nFact
          xPowN = lookupForce n xPowers
        makeSum _ _ = error "internal error in Poly.Cheb.SineCosine.sineCosineTaylorSeries"
    in
    sumAndError
    -- where
    -- fixAccuracyLimit (pA, e, n) = (pA acLimit,e,n)
    --   where
    --   acLimit = 4 + (normLog2Accuracy $ getNormLog e)


lookupForce :: P.Ord k => k -> Map.Map k a -> a
lookupForce j amap =
    case Map.lookup j amap of
        Just t -> t
        Nothing -> error "internal error in SineCosine.lookupForce"
