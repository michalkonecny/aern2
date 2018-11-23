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
   MPFloat, setPrecisionUp, setPrecisionDown
   , p2mpfrPrec
  --  , downUp2centerError
   )
where

import MixedTypesNumPrelude
import qualified Prelude as P

import AERN2.MP.Precision

import Data.Bits (unsafeShiftL)
import Data.Typeable

import qualified Data.CDAR as MPLow

{-| Multiple-precision floating-point type based on CDAR.Approx with 0 radius. -}
type MPFloat = MPLow.Approx

deriving instance (Typeable MPFloat)

p2mpfrPrec :: Precision -> MPLow.Precision
p2mpfrPrec = P.fromInteger . integer

instance HasPrecision MPFloat where
  getPrecision (MPLow.Approx _ _ p) = prec (P.toInteger $ - p)
  getPrecision MPLow.Bottom = error "illegal MPFloat (Bottom)"
  

instance CanSetPrecision MPFloat where
  setPrecision = setPrecisionUp

setPrecisionUp :: Precision -> MPFloat -> MPFloat
setPrecisionUp = setPrecisionWithRound MPLow.upperA

setPrecisionDown :: Precision -> MPFloat -> MPFloat
setPrecisionDown = setPrecisionWithRound MPLow.lowerA

setPrecisionWithRound :: (MPFloat -> MPFloat) -> Precision -> MPFloat -> MPFloat
setPrecisionWithRound roundFn pp x@(MPLow.Approx m _ s)
  | s < -p = roundFn $ MPLow.limitSize p x
  | s == -p = x
  | otherwise = MPLow.Approx (unsafeShiftL m (int $ s+p)) 0 (-p)
  where
  p = p2mpfrPrec pp
setPrecisionWithRound _ _ MPLow.Bottom = MPLow.Bottom

-- downUp2centerError :: (MPFloat, MPFloat) -> (MPFloat, MPFloat)
-- downUp2centerError (MPLow.Approx l _eL sL, MPLow.Approx u _eR _sR)
--   -- | eL == 0 && eR == 0 && sL == sR = && even (l+r)
--     = (MPLow.Approx ((l+u) `P.div` 2) 0 sL, MPLow.Approx ((u-l) `P.div` 2) 0 sL)
-- downUp2centerError _ = error "downUp2centerError: incompatible operands"
  