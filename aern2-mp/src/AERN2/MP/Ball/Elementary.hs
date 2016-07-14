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
  , monotoneFromApprox
)
where

import Numeric.MixedTypes
import qualified Prelude as P

import AERN2.MP.Dyadic (Dyadic, dyadic)
import qualified AERN2.MP.Float as MPFloat
import AERN2.MP.Float (MPFloat, mpFloat)
-- import AERN2.MP.Float.Operators
import AERN2.MP.Precision
import qualified AERN2.MP.ErrorBound as EB
import AERN2.MP.ErrorBound (errorBound)

import AERN2.MP.Ball.Type
import AERN2.MP.Ball.Conversions ()
import AERN2.MP.Ball.Comparisons ()
import AERN2.MP.Ball.Field ()

{- trigonometrics -}

piBallP :: Precision -> MPBall
piBallP p = MPBall piUp (piUp `EB.subMP` piDown)
  where
  piUp = MPFloat.piUp p
  piDown = MPFloat.piDown p

instance CanSinCos MPBall where
  sin = sinB 1
  cos = cosB 1

sinB :: Integer -> MPBall -> MPBall
sinB i x =
    fromApproxWithLipschitz MPFloat.sinDown MPFloat.sinUp lip x
    where
    lip
        | i == 0 = mpFloat 1
        | otherwise = snd $ endpointsMP $ abs $ cosB (i - 1) x

cosB :: Integer -> MPBall -> MPBall
cosB i x =
    fromApproxWithLipschitz MPFloat.cosDown MPFloat.cosUp lip x
    where
    lip
        | i == 0 = mpFloat 1
        | otherwise = snd $ endpointsMP $ abs $ sinB (i - 1) x

{- exp, log, power -}

instance CanExp MPBall where
  exp = monotoneFromApprox MPFloat.expDown MPFloat.expUp

instance CanLog MPBall where
  log x
    | x !>! 0 = monotoneFromApprox MPFloat.logDown MPFloat.logUp x
    | otherwise = error $ "MPBall log: cannot establish that the argument is positive: " ++ show x

instance CanPow MPBall MPBall where
  pow = powViaExpLog

instance CanPow MPBall Dyadic where
  pow x q = powViaExpLog x (mpBall q)

instance CanPow MPBall Rational where
  pow x q = powViaExpLog x (mpBallP (getPrecision x) q)

instance CanSqrt MPBall where
  sqrt x
    | x !>=! 0 = aux x
    | x ?>=? 0 = aux (max 0 x)
    | otherwise = error $ "MPBall sqrt: cannot establish that the argument is non-negative: " ++ show x
    where
      aux = monotoneFromApprox MPFloat.sqrtDown MPFloat.sqrtUp


{- Instances of Prelude numerical classes provided for convenient use outside AERN2
   and also because Template Haskell translates (-x) to (Prelude.negate x) -}

instance P.Num MPBall where
    fromInteger = convertExactly
    negate = negate
    (+) = (+)
    (*) = (*)
    abs = abs
    signum = error "Prelude.signum not implemented for MPBall"

instance P.Eq MPBall where
    a == b = (a == b) == Just True
    a /= b = (a /= b) == Just True

instance P.Ord MPBall where
    a < b =  (a < b) == Just True
    a <= b =  (a <= b) == Just True
    a > b =  (a > b) == Just True
    a >= b =  (a >= b) == Just True
    compare r1 r2
        | (r1 < r2) == Just True = LT
        | (r1 > r2) == Just True = GT
        | (r1 == r2) == Just True = EQ
        | otherwise = error "AERN2.Num.MPBall: compare: cannot decide"

instance P.Fractional MPBall where
    fromRational = convertExactly . dyadic -- will work only for dyadic rationals
    recip = recip
    (/) = (/)

instance P.Floating MPBall where
    pi = error "MPBall: pi not implemented" -- no global precision to pick
    sqrt = sqrt
    exp = exp
    sin = sin
    cos = cos
    log = log
    atan = error "MPBall: atan not implemented yet"
    atanh = error "MPBall: atanh not implemented yet"
    asin = error "MPBall: asin not implemented yet"
    acos = error "MPBall: acos not implemented yet"
    sinh = error "MPBall: sinh not implemented yet"
    cosh = error "MPBall: cosh not implemented yet"
    asinh = error "MPBall: asinh not implemented yet"
    acosh = error "MPBall: acosh not implemented yet"

{- generic methods for computing real functions from MPFR-approximations -}

{-|
    Computes a real function @f@ from correctly rounded MPFR-approximations and a number @lip@ which is a
    Lipschitz constant for @f@, i.e. @|f(x) - f(y)| <= lip * |x - y|@ for all @x@,@y@.
-}
fromApproxWithLipschitz ::
    (MPFloat -> MPFloat) {-^ @fDown@: a version of @f@ on MPFloat rounding *downwards* -} ->
    (MPFloat -> MPFloat) {-^ @fUp@: a version of @f@ on MPFloat rounding *upwards* -} ->
    MPFloat {-^ @lip@ a Lipschitz constant for @f@, @lip > 0@ -} ->
    (MPBall -> MPBall) {-^ @f@ on MPBall rounding *outwards* -}
fromApproxWithLipschitz fDown fUp lip _x@(MPBall xc xe) =
    MPBall fxc err
    where
    fxl = fDown xc
    fxu = fUp xc
    (MPBall fxc fxe) = fromEndpointsMP fxl fxu
    err = (errorBound lip) * xe  +  fxe

{-|
    Computes a *monotone* ball function @f@ from correctly rounded MPFR-approximations.
-}
monotoneFromApprox ::
    (MPFloat -> MPFloat) {-^ @fDown@: a version of @f@ on MPFloat rounding *downwards* -} ->
    (MPFloat -> MPFloat) {-^ @fUp@: a version of @f@ on MPFloat rounding *upwards* -} ->
    (MPBall -> MPBall) {-^ @f@ on MPBall rounding *outwards* -}
monotoneFromApprox fDown fUp x =
    fromEndpointsMP (fDown l) (fUp u)
    where
    (l,u) = endpointsMP x

{-  random generation -}
