module AERN2.MP.Affine
  ( module AERN2.MP.Affine.Type,
    module AERN2.MP.Affine.Conversions,
  )
where

import AERN2.MP.Affine.Conversions
import AERN2.MP.Affine.Exp ()
import AERN2.MP.Affine.Field ()
import AERN2.MP.Affine.Order ()
import AERN2.MP.Affine.Ring ()
import AERN2.MP.Affine.Sqrt ()
import AERN2.MP.Affine.Type
import AERN2.MP.Dyadic (dyadic)
import AERN2.MP.Float (mpFloat)
import Data.Map qualified as Map
import MixedTypesNumPrelude

{-
  Ad-hoc tests
-}

_conf :: MPAffineConfig
_conf = MPAffineConfig {maxTerms = int 4, precision = 100}

_mpaff1 :: MPAffine
_mpaff1 =
  MPAffine
    { config = _conf,
      centre = mpFloat 1,
      errTerms =
        Map.fromList
          [ (ErrorTermId (int 1), mpFloat (dyadic (-0.5))),
            (ErrorTermId (int 2), mpFloat (dyadic 0.5))
          ]
    }

_mpaff2 :: MPAffine
_mpaff2 =
  MPAffine
    { config = _conf,
      centre = mpFloat 1,
      errTerms =
        Map.fromList
          [ (ErrorTermId (int 1), mpFloat (dyadic 0.5)),
            (ErrorTermId (int 3), mpFloat (dyadic 0.5))
          ]
    }

_sample :: MPAffine
_sample = _mpaff1

_a100 :: MPAffine
_a100 = mpAffineWithSample _sample 100

_aThird :: MPAffine
_aThird = mpAffineWithSample _sample (1 / 3)
