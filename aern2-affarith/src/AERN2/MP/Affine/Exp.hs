{-# LANGUAGE RecordWildCards #-}

module AERN2.MP.Affine.Exp
  (
  )
where

-- import Prelude qualified as P

import AERN2.MP (MPBall (MPBall), errorBound, mpBall)
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

derivative exp'(x) = exp(x)

exp(a + ex) = exp(a) + ex*exp(a) + err(x)
            = (1+ex)*exp(a) + err(x)

err(x) = exp(a + ex) - (1+ex)*exp(a)

0 <= err(x) <= err(1)

t = err(1)

exp(a + ex) \in exp(a) + ex*exp(a) + [0, t]

new error variable accounting for
- truncation error t/2
- error during scaling of the terms
- error in calculating new centre exp(a) + t/2

-}

instance CanExp MPAffine where
  type ExpType MPAffine = MPAffine
  exp = mpAffineExp

mpAffineExp :: MPAffine -> MPAffine
mpAffineExp aff = resultAff
  where
    -- calculate range of aff
    range_Ball = mpBall aff
    (MPBall a e) = range_Ball
    e_Ball = mpBall e
    a_Ball = MPBall a (errorBound 0)
    -- a - e
    (_, range_Max_Ball) = mpBallEndpoints range_Ball

    -- calculate exp(a)
    exp_a_Ball = exp a_Ball

    -- calculate new (scaled) error terms
    ex = aff.errTerms
    (scaled_ex, err_scaling_ex) = scaleErrTerms exp_a_Ball ex -- ex*exp(a)

    -- t = err(1) = exp(a + e) - (1+e)*exp(a)
    t = exp range_Max_Ball - (1 + e_Ball) * exp_a_Ball
    err_trunc = errorBound (t / 2)

    -- new centre
    centre_Ball = exp_a_Ball + t / 2
    (MPBall centre err_centre) = centre_Ball

    -- overall new error
    newTermId = ErrorTermId (hash ("exp", aff))
    errTerms = Map.insert newTermId (mpFloat newError) scaled_ex
      where
        newError = err_trunc + err_scaling_ex + err_centre

    resultAff = MPAffine {config = aff.config, ..}
