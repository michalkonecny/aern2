{-# LANGUAGE RecordWildCards #-}

module AERN2.MP.Affine.SinCos
  (
  )
where

-- import Prelude qualified as P

import AERN2.MP (MPBall (MPBall), errorBound, mpBall)
import AERN2.MP.Affine.Conversions (mpAffineFromBall)
import AERN2.MP.Affine.Order ()
import AERN2.MP.Affine.Ring (scaleErrTerms)
import AERN2.MP.Affine.Type
import AERN2.MP.Ball.Type (mpBallEndpoints)
import AERN2.MP.Float (mpFloat)
import Data.Hashable (Hashable (hash))
import Data.Map qualified as Map
import GHC.Records
import MixedTypesNumPrelude

{-

assuming e > 0, x in [-1,1]

derivative sin'(x) = cos(x)
derivative cos'(x) = -sin(x)

sin(a + ex) = sin(a) + ex*cos(a) + err(x)
cos(a + ex) = cos(a) - ex*sin(a) + err(x)

\** Case 1: (Potentially) containing a local maximum

[e.g. sin(a + ex) with a - ex < pi/2 < a + ex]

derivative containing 0, not worth utilising the affine parts

calculate via MPBall

\** Case 2: Monotone value

\*** increasing value

[e.g. sin(a + ex) with
-pi/2 <= a + ex <= pi/2
a - ex < 0 < a + ex
]

tL = min(0, err(1)) <= err(x) <= max(0, err(-1)) = tH

sin(a + ex) \in sin(a) + ex*cos(a) + [tL, tH]
cos(a + ex) \in cos(a) - ex*sin(a) + [tL, tH]

new error variable accounting for
- truncation error (tH - tL)/2
- error during scaling of the terms
- error in calculating new centre sin/cos(a) + (tL + tH)/2

\*** decreasing value

reduce to increasing value using
sin(x) = -sin(-x)
cos(x) = cos(-x)

-}

instance CanSinCos MPAffine where
  type SinCosType MPAffine = MPAffine
  sin = mpAffineSin
  cos = mpAffineCos

mpAffineSin :: MPAffine -> MPAffine
mpAffineSin = mpAffineSinCos "sin" sin cos mpAffineSinNegated

mpAffineSinNegated :: MPAffine -> MPAffine
mpAffineSinNegated = mpAffineSinCos "-sin" (negate . sin) (negate . cos) mpAffineSin

mpAffineCos :: MPAffine -> MPAffine
mpAffineCos = mpAffineSinCos "cos" cos (negate . sin) mpAffineCosNegated

mpAffineCosNegated :: MPAffine -> MPAffine
mpAffineCosNegated = mpAffineSinCos "cos" (negate . cos) sin mpAffineCos

mpAffineSinCos :: String -> (MPBall -> MPBall) -> (MPBall -> MPBall) -> (MPAffine -> MPAffine) -> (MPAffine -> MPAffine)
mpAffineSinCos fName f f' fNegated aff
  | fIsDecreasing =
      -- reduce to an increasing case
      negate $ fNegated aff
  | not fIsIncreasing =
      -- possibly not monotone
      viaMPBall -- fall back on MPBall arithmetic
  | otherwise =
      -- increasing f, f' may have one local maximum
      resultIncreasing
  where
    affRange = mpBall aff
    f_affRange = f affRange
    f'_affRange = f' affRange

    fIsIncreasing = f'_affRange !>! 0
    fIsDecreasing = f'_affRange !<! 0

    viaMPBall :: MPAffine = mpAffineFromBall aff f_affRange f_affRange

    -- calculate range of aff
    (MPBall a e) = affRange
    eBall = mpBall e
    aBall = MPBall a (errorBound 0)
    -- a - e, a + e
    (rangeMinBall, rangeMaxBall) = mpBallEndpoints affRange

    -- value and derivative at the centre
    fa = f aBall
    f'a = f' aBall

    -- err(1) = f(a+e) - f(a) + ef'(a)
    errP1 = (f rangeMaxBall) - (f aBall) - eBall * f'a
    -- err(-1) = f(a-e) - f(a) - ef'(a)
    errM1 = (f rangeMinBall) - (f aBall) + eBall * f'a

    tL = min 0 errP1 -- non-positive
    tH = max 0 errM1 -- non-negative
    resultIncreasing = resultWithTangent shift radius
      where
        -- sin/cos in the increasing segment has:
        --   - derivative first increasing then decreasing
        --   - decreasing err(x)
        -- thus tL = min(0, err(1)) <= err(x) <= max(0, err(-1)) = tH
        shift = (tH + tL) / 2 -- could be positive or negative
        radius = (tH - tL) / 2 -- positive radius
    resultWithTangent (shift :: MPBall) radius =
      MPAffine {config = aff.config, ..}
      where
        -- new centre
        centreBall = fa + shift
        (MPBall centre errCentre) = centreBall

        errTerms = Map.insert newTermId (mpFloat newError) scaledTerms
          where
            -- new (scaled) error terms
            ex = aff.errTerms
            (scaledTerms, errScalingTerms) = scaleErrTerms f'a ex -- ex*f'(a)

            -- overall new error
            newTermId = ErrorTermId (hash (fName, aff))
            newError = errTrunc + errScalingTerms + errCentre
            errTrunc = errorBound radius
