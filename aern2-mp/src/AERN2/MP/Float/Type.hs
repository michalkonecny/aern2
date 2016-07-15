{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-|
    Module      :  AERN2.MP.Float.Type
    Description :  Arbitrary precision floating point numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision floating-point numbers

    Currently, we use hmpfr when compiling with ghc 7.10 and higher
    and haskell-mpfr when compiling with ghc 7.8.
-}

module AERN2.MP.Float.Type
  (
   -- * MPFloat numbers and their basic operations
   MPFloat, setPrecisionUp, setPrecisionDown
   , p2mpfrPrec
   )
where

import Numeric.MixedTypes
import qualified Prelude as P

import AERN2.MP.Precision

#ifdef HaskellMPFR
import Data.Typeable
import qualified Data.Approximate.MPFRLowLevel as MPLow

{-| Multiple-precision floating-point type based on MPFR via haskell-mpfr. -}
type MPFloat = MPLow.Rounded

deriving instance (Typeable MPFloat)

#endif
#ifdef HMPFR
import qualified Data.Number.MPFR as MPLow

{-| Multiple-precision floating-point type based on MPFR via hmpfr. -}
type MPFloat = MPLow.MPFR

#endif

instance HasPrecision MPFloat where
  getPrecision x = prec (P.toInteger $ MPLow.getPrec x)

instance CanSetPrecision MPFloat where
  setPrecision = setPrecisionNear

setPrecisionNear :: Precision -> MPFloat -> MPFloat
setPrecisionNear p = MPLow.set MPLow.Near (p2mpfrPrec p)

setPrecisionUp :: Precision -> MPFloat -> MPFloat
setPrecisionUp p = MPLow.set MPLow.Up (p2mpfrPrec p)

setPrecisionDown :: Precision -> MPFloat -> MPFloat
setPrecisionDown p = MPLow.set MPLow.Down (p2mpfrPrec p)

p2mpfrPrec :: Precision -> MPLow.Precision
p2mpfrPrec = P.fromInteger . integer
