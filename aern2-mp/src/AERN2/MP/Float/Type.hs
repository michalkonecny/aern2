{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
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
   MPFloat(..)
   , lift1, lift2, lift2R
   , getErrorStepSizeLog
   , setPrecisionCEDU
   , p2cdarPrec
   , getBoundsCEDU
   )
where

import MixedTypesNumPrelude
import qualified Prelude as P

-- import Data.Bits (unsafeShiftL)
import Data.Typeable

import AERN2.Norm
import AERN2.MP.Precision
import AERN2.MP.Float.Auxi

import qualified Data.CDAR as MPLow
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

{-| Multiple-precision floating-point type based on CDAR.Approx with 0 radius. -}
newtype MPFloat = MPFloat { unMPFloat :: MPLow.Approx }
  deriving (Generic)

lift1 :: (MPLow.Approx -> MPLow.Approx) -> MPFloat -> MPFloat
lift1 f (MPFloat a) = MPFloat (f a)

lift2 :: 
  (MPLow.Approx -> MPLow.Approx -> MPLow.Approx) -> 
  (MPFloat -> MPFloat -> MPFloat)
lift2 f (MPFloat a1) (MPFloat a2) = MPFloat (f a1 a2)

lift2R :: 
  (MPLow.Approx -> MPLow.Approx -> t) -> 
  (MPFloat -> MPFloat -> t)
lift2R f (MPFloat a1) (MPFloat a2) = f a1 a2

instance Show MPFloat where
  show x = MPLow.showA $ unMPFloat x

deriving instance (Typeable MPFloat)
instance NFData MPFloat

p2cdarPrec :: Precision -> MPLow.Precision
p2cdarPrec = P.fromInteger . integer

getBoundsCEDU :: MPFloat -> BoundsCEDU MPFloat
getBoundsCEDU (MPFloat (MPLow.Approx mb m e s)) = 
  BoundsCEDU 
    (MPFloat $ MPLow.Approx mb m 0 s) (MPFloat $ MPLow.approxMB eb_mb e 0 s)
    (MPFloat $ MPLow.Approx mb (m-e) 0 s) (MPFloat $ MPLow.Approx mb (m+e) 0 s)
getBoundsCEDU (MPFloat MPLow.Bottom) =
  BoundsCEDU
    (MPFloat MPLow.Bottom) (MPFloat MPLow.Bottom) 
    (MPFloat MPLow.Bottom) (MPFloat MPLow.Bottom)

{-| The bit-size bound for the error bound in CEDU -}
eb_prec :: Precision
eb_prec = prec 63

{-| The bit-size bound for the error bound in CEDU -}
eb_mb :: Int
eb_mb = int $ integer eb_prec

instance HasPrecision MPFloat where
  getPrecision (MPFloat (MPLow.Approx mb _ _ _)) = prec (P.toInteger $ mb)
  getPrecision (MPFloat MPLow.Bottom) = error "illegal MPFloat (Bottom)"
  
instance CanSetPrecision MPFloat where
  setPrecision p = ceduCentre . setPrecisionCEDU p

setPrecisionCEDU :: Precision -> MPFloat -> BoundsCEDU MPFloat
setPrecisionCEDU pp = 
  getBoundsCEDU . lift1 MPLow.enforceMB . lift1 (MPLow.setMB (p2cdarPrec pp))

instance HasNorm MPFloat where
  getNormLog (MPFloat (MPLow.Approx _ m _ s)) = (getNormLog m) + (integer s)
  getNormLog (MPFloat MPLow.Bottom) = error "getNormLog undefined for Bottom"

{-|
  Returns @s@ such that @2^s@ is the distance to the nearest other number with the same precision.
  Returns Nothing for Bottom.
-}
getErrorStepSizeLog :: MPLow.Approx -> Maybe Int
getErrorStepSizeLog (MPLow.Approx _ _ _ s) = Just $ s
getErrorStepSizeLog _ = Nothing -- represents +Infinity
