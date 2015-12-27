module AERN2.Net.Execution.Direct where

import AERN2.Real
import AERN2.Net.Spec.Arrow


{- Direct evaluation using CauchyReal -}

instance ArrowReal (->) CauchyReal where
    realA r _name = const r
    sqrtA = sqrt
    addA = uncurry (+)
    mulA = uncurry (*)

{- Direct evaluation using MPBall -}

instance ArrowReal (->) MPBall where
--    piA p = cauchyReal2ball (prec2integer p) pi -- TODO: enable when we have (SizeLimits MPBall)
    sqrtA = sqrt
    addA = uncurry (+)
    mulA = uncurry (*)
