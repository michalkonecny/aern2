module AERN2.AffArith
  ( module AERN2.AffArith.Conversions,
    module AERN2.AffArith.Type,
  )
where

import AERN2.AffArith.Order ()
import AERN2.AffArith.Conversions
import AERN2.AffArith.Ring ()
import AERN2.AffArith.Field ()
import AERN2.AffArith.Type
import AERN2.MP.Dyadic (dyadic)
import AERN2.MP.Float (mpFloat)
import qualified Data.Map as Map
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

_a100 :: MPAffine
_a100 = mpAffine _conf 100

_aThird :: MPAffine
_aThird = mpAffine _conf (1 / 3)
