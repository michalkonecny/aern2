{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, StandaloneDeriving #-}
{-|
    Module      :  AERN2.MP.Float.Type
    Description :  Arbitrary precision floating point numbers (via cdar)
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision floating-point numbers, re-using CDAR Approx type.
-}

module AERN2.MP.Float.Type
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

-- import Data.Bits (unsafeShiftL)
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
getBoundsCEDU (MPLow.Approx mb m e s) = 
  BoundsCEDU 
    (MPLow.Approx mb m 0 s) (MPLow.approxMB eb_mb e 0 s)
    (MPLow.Approx mb (m-e) 0 s) (MPLow.Approx mb (m+e) 0 s)
getBoundsCEDU MPLow.Bottom =
  BoundsCEDU
    MPLow.Bottom MPLow.Bottom MPLow.Bottom MPLow.Bottom

{-| The bit-size bound for the error bound in CEDU -}
eb_prec :: Precision
eb_prec = prec 63

{-| The bit-size bound for the error bound in CEDU -}
eb_mb :: Int
eb_mb = int $ integer eb_prec

instance HasPrecision MPFloat where
  getPrecision (MPLow.Approx mb _ _ _) = prec (P.toInteger $ mb)
  getPrecision MPLow.Bottom = error "illegal MPFloat (Bottom)"
  

instance CanSetPrecision MPFloat where
  setPrecision p = ceduCentre . setPrecisionCEDU p

setPrecisionCEDU :: Precision -> MPFloat -> BoundsCEDU MPFloat
setPrecisionCEDU pp = getBoundsCEDU . MPLow.enforceMB . MPLow.setMB (p2mpfrPrec pp)

instance HasNorm MPFloat where
  getNormLog (MPLow.Approx _ m _ s) = (getNormLog m) + (integer s)
  getNormLog MPLow.Bottom = error "getNormLog undefined for Bottom"
