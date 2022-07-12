{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  AERN2.MP.Float.PreludeNum
    Description :  Prelude numerical operators for MPFloat
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Instances for Prelude classes such as Num, Fractional, using an unspecified rounding.
-}

module AERN2.MP.Float.PreludeNum
(
)
where

import MixedTypesNumPrelude
import qualified Prelude as P

-- import AERN2.MP.Precision
import AERN2.MP.Float.Auxi

import AERN2.MP.Float.Type
import AERN2.MP.Float.Arithmetic

deriving instance P.Eq MPFloat
deriving instance P.Ord MPFloat

instance P.Num MPFloat where
    fromInteger = MPFloat . P.fromInteger -- default precision (mBound)!
    negate = lift1 P.negate
    abs = lift1 P.abs
    signum = lift1 P.signum
    (+) = c2 addCEDU
    (-) = c2 subCEDU
    (*) = c2 mulCEDU

instance P.Fractional MPFloat where
    fromRational = MPFloat . P.fromRational -- default precision (mBound)!
    (/) = c2 divCEDU

instance P.Floating MPFloat where
    sin = c1 sinCEDU
    cos = c1 cosCEDU
    exp = c1 expCEDU
    log = c1 logCEDU
    pi = error "Prelude.Floating MPFloat: pi not defined"
    asin = error "Prelude.Floating MPFloat: asin not defined yet"
    acos = error "Prelude.Floating MPFloat: acos not defined yet"
    atan = error "Prelude.Floating MPFloat: atan not defined yet"
    sinh = error "Prelude.Floating MPFloat: sinh not defined yet"
    cosh = error "Prelude.Floating MPFloat: cosh not defined yet"
    asinh = error "Prelude.Floating MPFloat: asinh not defined yet"
    acosh = error "Prelude.Floating MPFloat: acosh not defined yet"
    atanh = error "Prelude.Floating MPFloat: atanh not defined yet"

c1 :: 
    (t -> BoundsCEDU MPFloat) -> 
    (t -> MPFloat)
c1 op x = ceduCentre $ op x

c2 :: 
    (t1 -> t2 -> BoundsCEDU MPFloat) -> 
    (t1 -> t2 -> MPFloat)
c2 op x y = ceduCentre $ op x y

instance P.Real MPFloat where
    toRational (MPFloat a) = toRational a
