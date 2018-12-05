{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, StandaloneDeriving #-}
{-|
    Module      :  AERN2.MP.Float.UseCDAR.Type
    Description :  Arbitrary precision floating point numbers (via cdar)
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision floating-point numbers using MPFR via package rounded.
-}

module AERN2.MP.Float.UseCDAR.Type
  (
   -- * MPFloat numbers and their basic operations
   MPFloat
   , showMPFloat
   , setPrecisionCEDU
   , p2mpfrPrec
   , getBoundsCEDU
   )
where

import MixedTypesNumPrelude
import qualified Prelude as P

import Data.Bits (unsafeShiftL)
import Data.Typeable

import AERN2.Norm
import AERN2.MP.Precision
import AERN2.MP.Float.Aux

import qualified Data.CDAR as MPLow

{-| Multiple-precision floating-point type based on CDAR.Approx with 0 radius. -}
type MPFloat = MPLow.Approx

showMPFloat :: MPLow.Approx -> String
showMPFloat x = MPLow.showA x

deriving instance (Typeable MPFloat)

p2mpfrPrec :: Precision -> MPLow.Precision
p2mpfrPrec = P.fromInteger . integer

getBoundsCEDU :: MPFloat -> BoundsCEDU MPFloat
getBoundsCEDU (MPLow.Approx m e s) = 
  BoundsCEDU 
    (MPLow.Approx m 0 s) (MPLow.Approx e 0 s)
    (MPLow.Approx (m-e) 0 s) (MPLow.Approx (m+e) 0 s)
getBoundsCEDU MPLow.Bottom =
  BoundsCEDU
    MPLow.Bottom MPLow.Bottom MPLow.Bottom MPLow.Bottom

instance HasPrecision MPFloat where
  getPrecision (MPLow.Approx _ _ s) = prec (P.toInteger $ -s)
  getPrecision MPLow.Bottom = error "illegal MPFloat (Bottom)"
  

instance CanSetPrecision MPFloat where
  setPrecision p = ceduCentre . setPrecisionCEDU p

setPrecisionCEDU :: Precision -> MPFloat -> BoundsCEDU MPFloat
setPrecisionCEDU pp x@(MPLow.Approx m _ s)
  | s < -p = getBoundsCEDU $ MPLow.limitSize p x
  | s == -p = getBoundsCEDU x
  | otherwise = getBoundsCEDU $ MPLow.Approx (unsafeShiftL m (int $ s+p)) 0 (-p)
  where
  p = p2mpfrPrec pp
setPrecisionCEDU _ MPLow.Bottom = error "setPrecisionCentreErr: Bottom"

instance HasNorm MPFloat where
  getNormLog (MPLow.Approx m _ s) = (getNormLog m) + (integer s)
  getNormLog MPLow.Bottom = error "getNormLog undefined for Bottom"
