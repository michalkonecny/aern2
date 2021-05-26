{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  AERN2.MP.Ball.Elementary
    Description :  Elementary operations on arbitrary precision dyadic balls
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Elementary operations on arbitrary precision dyadic balls
-}
module AERN2.MP.Ball.Elementary
(
  -- * Ball operations (see also instances)
  piBallP
  -- * Helpers for constructing ball functions
  , fromApproxWithLipschitz
)
where

import MixedTypesNumPrelude
import qualified Prelude as P

import AERN2.Normalize

import AERN2.MP.Dyadic (Dyadic)
import qualified AERN2.MP.Float as MPFloat
import AERN2.MP.Float (MPFloat, mpFloat, ceduCentreErr)
-- import AERN2.MP.Float.Operators
import AERN2.MP.Precision
-- import qualified AERN2.MP.ErrorBound as EB
import AERN2.MP.ErrorBound (errorBound)

import AERN2.MP.Ball.Type
import AERN2.MP.Ball.Conversions ()
import AERN2.MP.Ball.Comparisons ()
import AERN2.MP.Ball.Field (mulByEndpoints)


{- trigonometrics -}

piBallP :: Precision -> MPBall
piBallP p = MPBall piC (errorBound piErr)
  where
  (piC, piErr) = MPFloat.ceduCentreErr $ MPFloat.piCEDU p

instance CanSinCos MPBall where
  sin = sinB 1
  cos = cosB 1

sinB :: Integer -> MPBall -> MPBall
sinB i x =
    -- increasingPrecisionUntilNotImproving (fromApproxWithLipschitz MPFloat.sinDown MPFloat.sinUp lip) x
    max (-1) $ min 1 $
    fromApproxWithLipschitz MPFloat.sinCEDU lip x
    where
    lip
        | i == 0 = mpFloat 1
        | otherwise = endpointR $ abs $ cosB (i - 1) x

cosB :: Integer -> MPBall -> MPBall
cosB i x =
    -- increasingPrecisionUntilNotImproving (fromApproxWithLipschitz MPFloat.cosDown MPFloat.cosUp lip) x
    max (-1) $ min 1 $
    fromApproxWithLipschitz MPFloat.cosCEDU lip x
    where
    lip
        | i == 0 = mpFloat 1
        | otherwise = endpointR $ abs $ sinB (i - 1) x

-- increasingPrecisionUntilNotImproving :: (MPBall -> MPBall) -> (MPBall -> MPBall)
-- increasingPrecisionUntilNotImproving f x =
--   waitUntilNotImproving $ map aux (precisions xPrec (xPrec*2))
--   where
--   xPrec = getPrecision x
--   precisions p1 p2 = p1 : (precisions p2 (p1 + p2))
--   aux p = f $ setPrecision p x
--   waitUntilNotImproving xx@(x1:_) = aux2 (getAccuracy x1) xx
--   waitUntilNotImproving _ = error "AERN2.MP.Ball.Elementary: internal error in increasingPrecisionUntilNotImproving"
--   aux2 x1AC (x1:x2:rest)
--     | x1AC < x2AC = aux2 x2AC (x2:rest)
--     | otherwise = x1
--     where
--     x2AC = getAccuracy x2
--   aux2 _ _ = error "AERN2.MP.Ball.Elementary: internal error in increasingPrecisionUntilNotImproving"

{- exp, log, power -}

instance CanExp MPBall where
  exp = intervalFunctionByEndpointsUpDown MPFloat.expDown MPFloat.expUp

instance CanLog MPBall where
  type LogType MPBall = MPBall
  log x
    | x_!>! 1 =
        setPrecision p $ ballFunctionUsingLipschitz log_ logLip x_
    | x_!>! 0 =
        setPrecision p $ intervalFunctionByEndpoints log_ x_
    | x !>! 0 =
        intervalFunctionByEndpoints log_ x
    | otherwise = err
    where
    p = getPrecision x
    x_ = reducePrecionIfInaccurate x
    err = error $ "log: argument must be > 0: " ++ show x
    log_ (MPBall c e) = MPBall lc (e + (errorBound le))
      where
      (lc, le) = ceduCentreErr $ MPFloat.logCEDU c
    logLip y = errorBound $ (1/y)

instance CanPow MPBall MPBall where
  pow = powUsingExpLog (mpBall 1) mulByEndpoints recip

instance CanPow MPBall Dyadic where
  pow b e = pow b (mpBall e)

instance CanPow MPBall Rational where
  pow b e = pow b (mpBallP (getPrecision b) e)

instance CanSqrt MPBall where
  type SqrtType MPBall = MPBall
  sqrt x
    | x !>=! 0 = aux x
    | x ?>=? 0 = aux $ max 0 x
    | otherwise = err
    where
    aux =
      intervalFunctionByEndpointsUpDown
        (\ e -> MPFloat.sqrtDown (P.max (mpFloat 0) e))
        (\ e -> MPFloat.sqrtUp (P.max (mpFloat 0) e))
    err = error $ "sqrt: argument must be >= 0: " ++ show x

{- generic methods for computing real functions from MPFR-approximations -}

{-|
    Computes a real function @f@ from correctly rounded MPFR-approximations and a number @lip@ which is a
    Lipschitz constant for @f@, i.e. @|f(x) - f(y)| <= lip * |x - y|@ for all @x@,@y@.
-}
fromApproxWithLipschitz ::
    (MPFloat -> MPFloat.BoundsCEDU MPFloat) {-^ @fCEDU@: a version of @f@ on MPFloat returning rigorous bounds -} ->
    MPFloat {-^ @lip@ a Lipschitz constant for @f@, @lip > 0@ -} ->
    (MPBall -> MPBall) {-^ @f@ on MPBall rounding *outwards* -}
fromApproxWithLipschitz fCEDU lip _x@(MPBall xc xe) =
    normalize $ MPBall fxCP err
    where
    (fxC, fxErr) = MPFloat.ceduCentreErr $ fCEDU xc
    (MPBall fxCP fxe) =
      setPrecision (getPrecision xc) $ -- beware, some MPFloat functions may increase precision, eg sine and cosine
        (MPBall fxC (errorBound fxErr))
    err = (errorBound lip) * xe  +  fxe

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |]]
  (\ b -> [d|

  instance 
    CanPow $b MPBall 
    where
    type PowType $b MPBall = MPBall
    pow x e = pow (mpBallP (getPrecision e) x) e
  |]))

