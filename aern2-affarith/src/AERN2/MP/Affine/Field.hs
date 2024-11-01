{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module AERN2.MP.Affine.Field
  (
  )
where

import AERN2.MP (IsInterval (endpoints), MPBall (MPBall), errorBound, fromEndpointsAsIntervals, mpBall, HasPrecision (getPrecision), CanSetPrecision (setPrecision))
import AERN2.MP.Affine.Conversions
import AERN2.MP.Affine.Order ()
import AERN2.MP.Affine.Ring ()
import AERN2.MP.Affine.Type
-- import GHC.Records
import MixedTypesNumPrelude
import Data.Hashable (Hashable(hash))


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
  | affIsExact = recipViaMPBall
  | isNegative = negate (recipAff (negate aff)) -- 1/-x = -(1/x)
  -- below, aff is positive, not exact
  | ab !>! 0 && sqrtab !>! 0 = tightEnclosure
  -- internal overflows, revert to recip via MPBall
  | otherwise = recipViaMPBall
  where
    -- find a <= aff <= b
    affBall = mpBall aff
    (aF, bF) = endpoints affBall
    a = MPBall aF (errorBound 0)
    b = MPBall bF (errorBound 0)

    -- if a <= 0 <= b return error
    admitsZero = aF <= 0 && 0 <= bF :: Bool

    -- if a == b, return 1/a
    affIsExact = aF == bF
    affBallRecip = 1 / affBall :: MPBall
    recipViaMPBall = mpAffineFromBall aff affBallRecip affBallRecip

    -- if b < 0, reduce to positive aff
    isNegative = bF < 0

    -- otherwise (i.e. 0 < a < b) build an aff tightly enclosing the 1/x curve over [a,b]:

    -- temporarily double the MPBall precision
    ap = getPrecision a
    aHigherPrec = setPrecision (2*ap + 10) a
    ab = aHigherPrec * b
    slope = setPrecision ap (aff / (-ab)) :: MPAffine
    sqrtab = setPrecision ap $ sqrt ab
    lowerIntercept = 2 / sqrtab
    upperIntercept = setPrecision ap $ (a + b) / ab
    intercept = fromEndpointsAsIntervals lowerIntercept upperIntercept :: MPBall
    tightEnclosure = slope + intercept -- aff/-ab + [ 2/sqrt(ab), (a+b)/ab ]

instance CanPow MPAffine Integer where
  type PowType MPAffine Integer = MPAffine
  pow aff n = powUsingMulRecip (mpAffineWithSample aff 1) (*) recip aff n

instance CanPow MPAffine Int where
  type PowType MPAffine Int = MPAffine
  pow aff n = pow aff (integer n)

instance CanDivIMod MPAffine MPAffine
  where
  type DivIType MPAffine MPAffine = Integer
  divIMod aff m
    | m !>! 0 = (error "Integer division for MPAffine undefined", xmAff)
    | otherwise = error $ "modulus not positive: " ++ show m
    where
      xmAff = mpAffineFromBall aff newTermId ((mpBall aff) `mod` (mpBall m))
      newTermId = ErrorTermId (hash ("mod", aff, m))
