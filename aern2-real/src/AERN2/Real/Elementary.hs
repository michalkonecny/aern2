{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  AERN2.Real.Elementary
    Description :  selected elementary operations on CReal
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Selected elementary operations on Cauchy Real numbers.
-}
module AERN2.Real.Elementary
(
  pi
)
where

import MixedTypesNumPrelude
import qualified Prelude as P

import Numeric.CollectErrors
  ( cn, CN )

import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.Real.Type
import AERN2.Real.Field ()

{- Examples of use:

*AERN2.Real MixedTypesNumPrelude> sin 1
{?(prec 36): [0.841470984814804978668689727783203125 ± ~1.5621e-10 ~2^(-32)]}
(0.06 secs, 722,440 bytes)
*AERN2.Real MixedTypesNumPrelude> sin 1 ? (bits 1000)
[0.8414709848078965066525023216302989996225630607... ± ~0.0000 ~2^(-1222)]

*AERN2.Real MixedTypesNumPrelude> exp 2 ? (prec 100)
[7.3890560989306502272304274605750078131770241145... ± ~4.7020e-38 ~2^(-124)]
(0.01 secs, 691,448 bytes)
*AERN2.Real MixedTypesNumPrelude> exp 2 ? (prec 1000)
[7.3890560989306502272304274605750078131803155705... ± ~0.0000 ~2^(-1422)]
(0.01 secs, 1,402,832 bytes)
*AERN2.Real MixedTypesNumPrelude> exp 2 ? (prec 10000)
[7.3890560989306502272304274605750078131803155705... ± ~0.0000 ~2^(-16086)]
(0.02 secs, 17,478,648 bytes)
*AERN2.Real MixedTypesNumPrelude> exp 2 ? (prec 100000)
[7.3890560989306502272304274605750078131803155705... ± ~0.0000 ~2^(-179557)]
(0.61 secs, 576,337,656 bytes)
*AERN2.Real MixedTypesNumPrelude> exp 2 ? (prec 1000000)
[7.3890560989306502272304274605750078131803155705... ± ~0.0000 ~2^(-1232830)]
(14.98 secs, 10,234,710,976 bytes)

*AERN2.Real MixedTypesNumPrelude> 2^0.5 ? (prec 100)
[1.4142135623730950488016887242096980779395106418... ± ~1.3299e-36 ~2^(-119)]
(0.01 secs, 1,128,824 bytes)
*AERN2.Real MixedTypesNumPrelude> 2^0.5 ? (prec 1000)
[1.4142135623730950488016887242096980785696718753... ± ~0.0000 ~2^(-1229)]
(0.02 secs, 6,903,360 bytes)
*AERN2.Real MixedTypesNumPrelude> 2^0.5 ? (prec 10000)
[1.4142135623730950488016887242096980785696718753... ± ~0.0000 ~2^(-13539)]
(0.38 secs, 282,175,336 bytes)

*AERN2.Real MixedTypesNumPrelude> 2^(1/3) ? (prec 1000)
[1.2599210498948731647672106072782283505702514647... ± ~0.0000 ~2^(-1229)]
(0.01 secs, 6,946,896 bytes)
*AERN2.Real MixedTypesNumPrelude> 2^(1/3) ? (prec 10000)
[1.2599210498948731647672106072782283505702514647... ± ~0.0000 ~2^(-13539)]
(0.40 secs, 281,994,312 bytes)

-}

{- common elementary operations -}

pi :: CReal
pi = CSequence $ map (cn . piBallP) cseqPrecisions

{- sine, cosine -}

instance CanSinCos CReal where
  cos = lift1 cos
  sin = lift1 sin

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

  instance CanSinCos $t where
    type SinCosType $t = CReal
    cos = cos . creal
    sin = sin . creal

  |]))

{- sqrt -}

instance CanSqrt CReal where
  sqrt = lift1 sqrt

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

  instance CanSqrt $t where
    type SqrtType $t = CReal
    sqrt = sqrt . creal

  |]))

{- exp -}

instance CanExp CReal where
  exp = lift1 exp

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

  instance CanExp $t where
    type ExpType $t = CReal
    exp = exp . creal

  |]))

{- log  -}

instance CanLog CReal where
  log = lift1 log

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

  instance CanLog $t where
    type LogType $t = CReal
    log = log . creal

  |]))

{- power -}

instance CanPow CReal CReal where
  type PowType CReal CReal = CReal
  pow = lift2 pow

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

  instance CanPow $t Dyadic where
    type PowType $t Dyadic = CReal
    pow b e = pow (creal b) (creal e)

  |]))

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

  instance CanPow $t Rational where
    type PowType $t Rational = CReal
    pow b e = pow (creal b) (creal e)

  |]))

-- {- reals mixed with Double -}

-- instance Convertible CReal Double where
--   safeConvert r =
--     safeConvert (centre (r ? (bitsS 53)))

-- binaryWithDouble :: (Double -> Double -> Double) -> CReal -> Double -> Double
-- binaryWithDouble op r d =
--   op (convert r) d

-- instance CanAddAsymmetric CReal Double where
--   type AddType CReal Double = Double
--   add = binaryWithDouble add

-- instance CanAddAsymmetric Double CReal where
--   type AddType Double CReal = Double
--   add = flip add

-- instance CanSub CReal Double where
--   type SubType CReal Double = Double
--   sub = binaryWithDouble sub

-- instance CanSub Double CReal where
--   type SubType Double CReal = Double
--   sub = flip $ binaryWithDouble (flip sub)

-- instance CanMulAsymmetric CReal Double where
--   type MulType CReal Double = Double
--   mul = binaryWithDouble mul

-- instance CanMulAsymmetric Double CReal where
--   type MulType Double CReal = Double
--   mul = flip mul

-- instance CanDiv CReal Double where
--   type DivType CReal Double = Double
--   divide = binaryWithDouble divide

-- instance CanDiv Double CReal where
--   type DivType Double CReal = Double
--   divide = flip $ binaryWithDouble (flip divide)

-- instance CanPow CReal Double where
--   type PowType CReal Double = Double
--   pow = binaryWithDouble pow

-- instance CanPow Double CReal where
--   type PowType Double CReal = Double
--   pow = flip $ binaryWithDouble (flip pow)

instance P.Floating CReal where
    pi = pi
    sqrt = sqrt
    exp = exp
    sin = sin
    cos = cos
    log = log
    -- (**) = (^)
    atan = error "CReal: atan not implemented yet"
    atanh = error "CReal: atanh not implemented yet"
    asin = error "CReal: asin not implemented yet"
    acos = error "CReal: acos not implemented yet"
    sinh = error "CReal: sinh not implemented yet"
    cosh = error "CReal: cosh not implemented yet"
    asinh = error "CReal: asinh not implemented yet"
    acosh = error "CReal: acosh not implemented yet"
