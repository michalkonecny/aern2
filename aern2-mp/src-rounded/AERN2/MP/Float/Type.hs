{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, StandaloneDeriving #-}
{-|
    Module      :  AERN2.MP.Float.Type
    Description :  Arbitrary precision floating point numbers (MPFR)
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision floating-point numbers using MPFR via package rounded.
-}

module AERN2.MP.Float.Type
  (
   -- * MPFloat numbers and their basic operations
   MPFloat
   , showMPFloat
   , setPrecisionCEDU
   , getCEDU
   , p2mpfrPrec
   )
where

import MixedTypesNumPrelude
import qualified Prelude as P

import AERN2.Norm
import AERN2.MP.Precision
import AERN2.MP.Float.Aux

import qualified AERN2.MP.Float.RoundedAdaptor as MPLow
import Data.Typeable

{-| Multiple-precision floating-point type based on MPFR via rounded. -}
type MPFloat = MPLow.Rounded

showMPFloat :: MPFloat -> String
showMPFloat = show

deriving instance (Typeable MPFloat)

p2mpfrPrec :: Precision -> MPLow.Precision
p2mpfrPrec = P.fromInteger . integer

getCEDU :: MPFloat -> MPFloat -> BoundsCEDU MPFloat
getCEDU d u = BoundsCEDU c e d u
  where
  c = u
  e = MPLow.sub MPLow.Up (MPLow.getPrec c) u d

instance HasPrecision MPFloat where
  getPrecision x = prec (P.toInteger $ MPLow.getPrec x)

instance CanSetPrecision MPFloat where
  setPrecision = setPrecisionUp

setPrecisionUp :: Precision -> MPFloat -> MPFloat
setPrecisionUp p = MPLow.set MPLow.Up (p2mpfrPrec p)

setPrecisionDown :: Precision -> MPFloat -> MPFloat
setPrecisionDown p = MPLow.set MPLow.Down (p2mpfrPrec p)

setPrecisionCEDU :: Precision -> MPFloat -> BoundsCEDU MPFloat
setPrecisionCEDU p x =
  getCEDU d u 
  where
  d = setPrecisionDown p x
  u = setPrecisionUp p x

instance HasNorm MPFloat where
  getNormLog x
    | x P.== MPLow.zero = NormZero
    | otherwise = NormBits (P.toInteger $ MPLow.getExp x)

