{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module AERN2.MP.Affine.Field
  (
  )
where

import AERN2.MP (IsInterval (endpoints), MPBall (MPBall), errorBound, fromEndpointsAsIntervals, mpBall)
import AERN2.MP.Affine.Conversions
import AERN2.MP.Affine.Ring ()
import AERN2.MP.Affine.Type
import GHC.Records
import MixedTypesNumPrelude


-- instance Field MPAffine

instance (CanTestZero MPAffine) where
  isCertainlyZero = isCertainlyZero . mpBall
  isCertainlyNonZero = isCertainlyNonZero . mpBall

instance CanDiv MPAffine MPAffine where
  type DivType MPAffine MPAffine = MPAffine
  divide aff1 aff2 = aff1 * (recipAff aff2)

instance CanDiv MPBall MPAffine where
  type DivType MPBall MPAffine = MPAffine
  divide b aff2 = b * (recipAff aff2)

instance CanDiv Integer MPAffine where
  type DivType Integer MPAffine = MPAffine
  divide n aff2 = n * (recipAff aff2)

instance CanDiv Int MPAffine where
  type DivType Int MPAffine = MPAffine
  divide n aff2 = n * (recipAff aff2)

instance CanDiv Rational MPAffine where
  type DivType Rational MPAffine = MPAffine
  divide n aff2 = n * (recipAff aff2)

recipAff :: MPAffine -> MPAffine
recipAff aff
  | admitsZero = error "MPAffine: potential division by zero"
  | affIsExact = aRecipAff
  | isNegative = negate (recipAff (negate aff)) -- 1/-x = -(1/x)
  | otherwise -- i.e. aff is positive
    =
      tightEnclosure
  where
    -- find a <= aff <= b
    (aF, bF) = endpoints (mpBall aff)
    a = MPBall aF (errorBound 0)
    b = MPBall bF (errorBound 0)

    -- if a <= 0 <= b return error
    admitsZero = aF <= 0 && 0 <= bF :: Bool

    -- if a == b, return 1/a
    affIsExact = aF == bF
    aRecip = 1 / a :: MPBall
    aRecipAff = mpAffineFromBall aff aRecip aRecip

    -- if b < 0, reduce to positive aff
    isNegative = bF < 0

    -- otherwise (i.e. 0 < a < b) build an aff tightly enclosing the 1/x curve over [a,b]
    ab = a * b
    slope = aff / (-ab) :: MPAffine
    lowerIntercept = 2 / (sqrt ab)
    upperIntercept = (a + b) / ab
    intercept = fromEndpointsAsIntervals lowerIntercept upperIntercept :: MPBall
    tightEnclosure = slope + intercept -- aff/-ab + [ 2/sqrt(ab), (a+b)/ab ]
