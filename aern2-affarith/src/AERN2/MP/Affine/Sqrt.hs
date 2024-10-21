{-# LANGUAGE RecordWildCards #-}
module AERN2.MP.Affine.Sqrt
  (
  )
where

import GHC.Records
import MixedTypesNumPrelude
-- import Prelude qualified as P

import AERN2.MP.Affine.Conversions (mpAffineFromBall)
import AERN2.MP.Affine.Type
import AERN2.MP.Affine.Ring (scaleErrTerms)
import AERN2.MP.Affine.Order ()

import AERN2.MP (MPBall(MPBall), mpBall, errorBound)
import AERN2.MP.Ball.Type (mpBallEndpoints)
import Data.Hashable (Hashable(hash))
import qualified Data.Map as Map
import AERN2.MP.Float (mpFloat)

{-

assuming a > e > 0, x in [-1,1]

derivative sqrt'(x) = 1/(2*sqrt(x))

sqrt(a + ex) = sqrt(a) + ex/(2*sqrt(a)) + err(x)

err(x) = sqrt(a) + ex/(2*sqrt(a)) - sqrt(a + ex)

err(x) >= 0
err(x) decreasing for x < 0
err(0) = 0
err(x) increasing for x > 0

max(err(1),err(-1)) >= err(x) >= 0 for all x in [-1,1]

err(-1) = sqrt(a) - e/(2*sqrt(a)) - sqrt(a - e)
err(1) = sqrt(a) + e/(2*sqrt(a)) - sqrt(a + e)

t = max(err(1),err(-1))

err(1) < err(-1) 
    <=>
    + e/(2*sqrt(a)) - sqrt(a + e) < - e/(2*sqrt(a)) - sqrt(a - e)
    <=>
    + e/(2*sqrt(a)) + e/(2*sqrt(a)) < sqrt(a + e) - sqrt(a - e)
    <=>
    e/sqrt(a) < sqrt(a + e) - sqrt(a - e)
    <=>
    e < sqrt(a)(sqrt(a + e) - sqrt(a - e))

sqrt(a + ex) \in sqrt(a) + ex/(2*sqrt(a)) + [-t, 0] 

new error variable accounting for
- truncation error t/2
- error during scaling of the terms
- error in calculating new centre sqrt(a) + t/2

-}

instance CanSqrt MPAffine where
    type (SqrtType MPAffine) = MPAffine
    sqrt = mpAffineSqrt


mpAffineSqrt :: MPAffine -> MPAffine
mpAffineSqrt aff
    | isCertainlyPositive range_Min_Ball && isCertainlyNonNegative resultAff = 
        -- Accept a proper affine enclosure only if it exludes negative values
        resultAff
    | otherwise = 
        -- Denegerate to interval arithmetic to avoid including negative values due to approximation errors near 0.
        mpAffineFromBall aff newTermId (sqrt range_Ball)
    where
    -- calculate range of aff
    range_Ball = mpBall aff
    (MPBall a e) = range_Ball
    e_Ball = mpBall e
    a_Ball = MPBall a (errorBound 0)
    -- a - e
    (range_Min_Ball, range_Max_Ball) = mpBallEndpoints range_Ball

    -- calculate sqrt(a)
    sqrt_a_Ball = sqrt a_Ball

    -- calculate new (scaled) error terms
    recip_2_sqrt_a_Ball = recip (2 * sqrt_a_Ball) -- 1/(2*sqrt(a))

    ex = aff.errTerms
    (scaled_ex, err_scaling_ex) = scaleErrTerms recip_2_sqrt_a_Ball ex -- ex/(2*sqrt(a))

    -- err(-1) = sqrt(a) - e/(2*sqrt(a)) - sqrt(a - e)
    tL = sqrt_a_Ball - (e_Ball * recip_2_sqrt_a_Ball)  - sqrt range_Min_Ball
    -- err(1) = sqrt(a) + e/(2*sqrt(a)) - sqrt(a + e)
    tR = sqrt_a_Ball + (e_Ball * recip_2_sqrt_a_Ball)  - sqrt range_Max_Ball
    t = max tL tR
    err_trunc = errorBound (t/2)

    -- new centre
    centre_Ball = sqrt_a_Ball - t / 2
    (MPBall centre err_centre) = centre_Ball

    -- overall new error
    newTermId = ErrorTermId (hash ("sqrt", aff))
    errTerms = Map.insert newTermId (mpFloat newError) scaled_ex
        where
        newError = err_trunc + err_scaling_ex + err_centre

    resultAff = MPAffine { config = aff.config, .. }

