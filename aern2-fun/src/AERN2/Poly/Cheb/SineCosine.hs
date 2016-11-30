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
import Text.Printf

import qualified Data.Map as Map
-- import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

import AERN2.Normalize

import AERN2.Norm
import AERN2.MP.Accuracy
import AERN2.MP.ErrorBound
import AERN2.MP.Float
import AERN2.MP.Ball (MPBall, mpBall, IsBall(..), IsInterval(..))
import qualified AERN2.MP.Ball as MPBall
-- import AERN2.MP.Dyadic

import AERN2.Real

import AERN2.Interval
import AERN2.RealFun.Operations
-- import AERN2.RealFun.UnaryFun

import AERN2.Poly.Basics

import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Ring ()
import AERN2.Poly.Cheb.Eval ()

import Debug.Trace (trace)

shouldTrace :: Bool
-- shouldTrace = False
shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace
    | shouldTrace = trace
    | otherwise = const id


_chPolySineX :: Accuracy -> ChPoly MPBall
_chPolySineX ac =
    sineWithAccuracyGuide ac x
    where
    x :: ChPoly MPBall
    x = varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (-1.0,1.0)

_chPolySine10X :: Accuracy -> ChPoly MPBall
_chPolySine10X ac =
    sineWithAccuracyGuide ac (10*x)
    where
    x :: ChPoly MPBall
    x = varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (-1.0,1.0)

_chPolyCosine10X :: Accuracy -> ChPoly MPBall
_chPolyCosine10X ac =
    cosineWithAccuracyGuide ac (10*x)
    where
    x :: ChPoly MPBall
    x = varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (-1.0,1.0)

_chPoly10X :: ChPoly MPBall
_chPoly10X =
    10*x
    where
    x :: ChPoly MPBall
    x = varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (-1.0,1.0)

_chPolySine10XSine20XX :: Accuracy -> ChPoly MPBall
_chPolySine10XSine20XX ac =
    sine(10*x + sine(20*x*x))
    where
    sine = sineWithAccuracyGuide ac
    x :: ChPoly MPBall
    x = varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (-1.0,1.0)


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
        "ChPoly.sineCosine: input:"
        ++ "\n isSine = " ++ show isSine
        -- ++ "\n xC = " ++ show xC
        ++ "\n xE = " ++ show xE
        ++ "\n xAccuracy = " ++ show xAccuracy
        ++ "\n r = " ++ show r
        ++ "\n k = " ++ show k
        ++ "\n trM = " ++ show trM
    ) $
    maybeTrace
    (
        "ChPoly.sineCosine: output:"
        ++ "\n Taylor series degree = " ++ show n
        ++ "\n getAccuracy taylorSum = " ++ show (getAccuracy taylorSum)
        ++ "\n taylorSumE = " ++ show taylorSumE
        ++ "\n getAccuracy result = " ++ show (getAccuracy res)
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
    r = mpBall $ applyApprox xC (getDomain xC)
    rC = centreAsBall r :: MPBall

    -- compute k = round(rC/(pi/2)):
    k = fst $ MPBall.integerBounds $ 0.5 + (2*rC / pi)

    -- shift xC near 0 using multiples of pi/2:
    txC ac = (setPrecisionAtLeastAccuracy ac xC) - k * pi / 2
    -- work out an absolute range bound for txC:
    (_, trM :: MPBall) = endpoints $ abs $ r - k * pi / 2

    -- compute sin or cos of txC = xC-k*pi/2 using Taylor series:
    (taylorSum, taylorSumE, n)
        | isSine && even k = sineTaylorSum txC trM acGuide
        | isCosine && odd k = sineTaylorSum txC trM acGuide
        | otherwise = cosineTaylorSum txC trM acGuide
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
  (Field c, CanMulBy c CauchyReal
  , IsBall c, IsInterval c c
  , HasAccuracy c, CanSetPrecision c
  , CanNormalize (ChPoly c), Show (ChPoly c), Show c)
  =>
  (Accuracy -> ChPoly c) -> MPBall -> Accuracy -> (ChPoly c, ErrorBound, Integer)
sineTaylorSum = sineCosineTaylorSum True

{-|
    For a given polynomial @p@, compute a partial Taylor sum of @cos(p)@ and return
    it together with its error bound @e@ and the degree of the polynomial @n@.
-}
cosineTaylorSum ::
  (Field c, CanMulBy c CauchyReal
  , IsBall c, IsInterval c c
  , HasAccuracy c, CanSetPrecision c
  , CanNormalize (ChPoly c), Show (ChPoly c), Show c)
  =>
  (Accuracy -> ChPoly c) -> MPBall -> Accuracy -> (ChPoly c, ErrorBound, Integer)
cosineTaylorSum = sineCosineTaylorSum False

sineCosineTaylorSum ::
  (Field c, CanMulBy c CauchyReal
  , IsBall c, IsInterval c c
  , HasAccuracy c, CanSetPrecision c
  , CanNormalize (ChPoly c), Show (ChPoly c), Show c)
  =>
  Bool ->
  (Accuracy -> ChPoly c) -> MPBall -> Accuracy -> (ChPoly c, ErrorBound, Integer)
sineCosineTaylorSum isSine xAC xM acGuidePre =
    let
    acGuide = acGuidePre + 4
    _isCosine = not isSine

    -- Work out the degree of the highest term we need to get the
    -- Lagrange error bound acGuide-accurate:
    n = Map.size factorialsE - 1 -- the last one is used only for the error term
    (_, (_,_,termSumEB)) = Map.findMax factorialsE -- the Lagrange error bound for T_n
    -- At the same time, compute the factorials and keep the Lagrange error bounds:
    factorialsE =
      maybeTrace ("sineCosineTaylorSum: n = " ++ show (Map.size res - 1)) res
      where
      res = Map.fromAscList $ takeUntilAccurate $ map addE factorials
      factorials = aux 0 1
        where aux i fc_i = (i,fc_i) : aux (i+1) (fc_i*(i+1))
      addE (i, fc_i) = (i, (fc_i, xM_i, e_i))
        where
        e_i = errorBound $ xM_i/fc_i
        xM_i = xM^i
      takeUntilAccurate (t_i@(i,(_fc_i, _xM_i,e_i)):rest)
        | getAccuracy e_i > acGuide && (even i == isSine) = [t_i]
        | otherwise = t_i : takeUntilAccurate rest
      takeUntilAccurate [] = error "sineCosineTaylorSum: internal error"

    -- Work out accuracy needed for each power x^n, given that x^n/n! should have
    -- accuracy around acGuide + 1:
    --    -log_2 (\eps/n!) ~ acGuide + 1
    --    -log_2 (\eps) ~ acGuide + 1 - (-log_2(1/n!))
    powerAccuracies0 =
      -- maybeTrace ("sineCosineTaylorSum: powerAccuracies0 = " ++ show res)
      res
      where
      res = Map.map aux factorialsE
      aux (fc_i,_xM_i,_e_i) =
        -- the accuracy needed of the power to give a sufficiently accurate term:
        acGuide + 1 + (normLog2Accuracy $ getNormLog fc_i)
    -- Ensure the accuracies in powers are sufficient
    -- to compute accurate higher powers by their multiplications:
    powerAccuracies =
      -- maybeTrace ("sineCosineTaylorSum: powerAccuracies = " ++ show res)
      res
      where
      res =
        foldl updateAccuracies powerAccuracies0 $
          drop (int 1) $ reverse $ -- from second-highest down to the lowest
            drop (int 2) $ Map.toAscList powerAccuracies0 -- the 2 lowest are computed directly
      updateAccuracies powerACs (i, ac_i)
        | odd i && odd j =  -- pw_(2j+1) = x * pw_j * pw_j
            updateAC j (ac_i + log_pw_j + log_x) $
            updateAC 1 (ac_i + log_pw_j + log_pw_j) $
            powerACs
            -- pw_(2j+1) + e_pw2j1 =  (x+e_x) * (pw_j + e_pwj) * (pw_j + e_pwj)
            -- = e_x * e_pwj * e_pwj
            -- ...
            -- + x * e_pwj * pw_j -- assume this term puts most constraint on the size of e_pwj
            -- + e_x * pw_j * pw_j -- assume this term puts most constraint on the size of e_x
            -- ...
            -- + x*pw_j*pw_j
        | odd i  = -- pw_(2j+1) = x * pw_(j-1) * pw_(j+1)
            updateAC (j-1) (ac_i + log_pw_jU + log_x) $
            updateAC (j+1) (ac_i + log_pw_jD + log_x) $
            updateAC 1 (ac_i + log_pw_jU + log_pw_jD) $
            powerACs
        | even j = -- pw_(2j) = (power j) * (power j)
            updateAC j (ac_i + log_pw_j) $
            powerACs
        | otherwise = -- pw_(2j) = (power (j-1)) * (power (j+1))
            updateAC (j-1) (ac_i + log_pw_jU) $
            updateAC (j+1) (ac_i + log_pw_jD) $
            powerACs
        where
        updateAC k ac_k = Map.adjust (max ac_k) k
        j = i `div` 2
        log_x = getLogXM 1
        log_pw_j = getLogXM j
        log_pw_jU = getLogXM (j+1)
        log_pw_jD = getLogXM (j-1)
        getLogXM k =
          case (Map.lookup k factorialsE) of
            Just (_fc_k,xM_k,_e_k) -> normLog2Accuracy $ getNormLog xM_k
            _ -> error "sineCosineTaylorSum: internal error"

    x = case Map.lookup 1 powerAccuracies of
      Just ac1 -> xAC ac1
      _ -> error "sineCosineTaylorSum: internal error"

    -- Compute the powers needed for the terms, reducing their size while
    -- respecting the required accuracy:
    powers
      | isSine = powersSine
      | otherwise = powersCosine
      where
      powersSine =
        maybeTrace ("sineCosineTaylorSum: powerSine accuracies:\n"
          ++ (showPowerAccuracies res)) res
        where
        res = foldl addPower initPowers $ zip [1..] [3,5..n]
        initPowers = Map.fromAscList [(1, x)]
        addPower prevPowers (j,i) =
          Map.insert i (reduce i pw_i) prevPowers
          where
          pw_i
            | odd j = x * pwr j * pwr j
            | otherwise = x * pwr (j-1) * pwr (j+1)
          pwr k = case Map.lookup k prevPowers of
            Just r -> r
            _ -> error "sineCosineTaylorSum: internal error (powersSine: pwr k)"
      powersCosine =
        maybeTrace ("sineCosineTaylorSum: powerCosine accuracies:\n"
          ++ (showPowerAccuracies res)) res
        where
        res = foldl addPower initPowers $ zip [2..] [4,6..n]
        initPowers = Map.fromAscList [(2, xxR)]
        xxR = reduce 2 $ x*x
        addPower prevPowers (j,i) =
          Map.insert i (reduce i pw_i) prevPowers
          where
          pw_i
            | even j = pwr j * pwr j
            | otherwise = pwr (j-1) * pwr (j+1)
          pwr k = case Map.lookup k prevPowers of
            Just r -> r
            _ -> error "sineCosineTaylorSum: internal error (powersCosine: pwr k)"
      showPowerAccuracies pwrs =
        unlines $ map showAAA $ Map.toAscList $
          Map.intersectionWith (\p (pa0, pa) -> (pa0,pa, p)) pwrs $
            Map.intersectionWith (,) powerAccuracies0 powerAccuracies
        where
        showAAA (i,(pa0,pa,p)) =
          printf "power %d: accuracy req 0: %s, accuracy req: %s, actual accuracy: %s, degree: %d"
            i (show pa0) (show pa) (show $ getAccuracy p) (terms_degree $  poly_coeffs $ chPoly_poly p)
      reduce i = reduceSizeUsingAccuracyGuide ac_i . setPrecisionAtLeastAccuracy (ac_i + 10)
        where
        ac_i = case Map.lookup i powerAccuracies of
          Just ac -> ac
          _ -> error "sineCosineTaylorSum: internal error"
    termSum =
      maybeTrace ("sineCosineTaylorSum: term accuracies = "
        ++ (show (map (\(i,t) -> (i,getAccuracy t)) terms))) $
      foldl1 (+) (map snd terms) + initNum
      where
      terms =
        Map.toAscList $ Map.intersectionWithKey makeTerm powers factorialsE
      initNum | isSine = 0
              | otherwise = 1
    makeTerm i pwr (fact,_,_e) =
      sign * pwr/fact -- alternate signs
      where
      sign = if (even $ i `div` 2) then 1 else -1
    in
    (termSum, termSumEB, n)

lookupForce :: P.Ord k => k -> Map.Map k a -> a
lookupForce j amap =
    case Map.lookup j amap of
        Just t -> t
        Nothing -> error "internal error in SineCosine.lookupForce"
