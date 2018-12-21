{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, StandaloneDeriving #-}
{-|
    Module      :  AERN2.MP.Float.UseRounded.Type
    Description :  Arbitrary precision floating point numbers (MPFR)
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision floating-point numbers using MPFR via package rounded.
-}

module AERN2.MP.Float.UseRounded.Type
  (
   -- * MPFloat numbers and their basic operations
   MPFloat, setPrecisionUp, setPrecisionDown
   , p2mpfrPrec
   )
where

import MixedTypesNumPrelude
import qualified Prelude as P

import AERN2.MP.Precision

import qualified AERN2.MP.Float.UseRounded.RoundedAdaptor as MPLow
import Data.Typeable

{-| Multiple-precision floating-point type based on MPFR via rounded. -}
type MPFloat = MPLow.Rounded

deriving instance (Typeable MPFloat)

p2mpfrPrec :: Precision -> MPLow.Precision
p2mpfrPrec = P.fromInteger . integer

instance HasPrecision MPFloat where
  getPrecision x = prec (P.toInteger $ MPLow.getPrec x)

instance CanSetPrecision MPFloat where
  setPrecision = setPrecisionUp

setPrecisionUp :: Precision -> MPFloat -> MPFloat
setPrecisionUp p = MPLow.set MPLow.Up (p2mpfrPrec p)

setPrecisionDown :: Precision -> MPFloat -> MPFloat
setPrecisionDown p = MPLow.set MPLow.Down (p2mpfrPrec p)
