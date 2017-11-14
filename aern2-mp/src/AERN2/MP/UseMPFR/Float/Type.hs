{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, StandaloneDeriving #-}
{-|
    Module      :  AERN2.MP.UseMPFR.Float.Type
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

module AERN2.MP.UseMPFR.Float.Type
  (
   -- * MPFloat numbers and their basic operations
   MPFloat, setPrecisionUp, setPrecisionDown
   , p2mpfrPrec
   )
where

import MixedTypesNumPrelude
import qualified Prelude as P

import AERN2.MP.Precision

#ifdef IntegerBackend
import qualified AERN2.MP.UseMPFR.Float.Native as MPLow

type MPFloat = MPLow.MPFloat

p2mpfrPrec :: Precision -> Precision
p2mpfrPrec = id

#endif

#ifdef HaskellMPFR
import qualified Data.Approximate.MPFRLowLevel as MPLow
import Data.Typeable

{-| Multiple-precision floating-point type based on MPFR via haskell-mpfr. -}
type MPFloat = MPLow.Rounded

deriving instance (Typeable MPFloat)

p2mpfrPrec :: Precision -> MPLow.Precision
p2mpfrPrec = P.fromInteger . integer

#endif

#ifdef MPFRRounded
import qualified AERN2.MP.UseMPFR.Float.RoundedAdaptor as MPLow
import Data.Typeable

{-| Multiple-precision floating-point type based on MPFR via rounded. -}
type MPFloat = MPLow.Rounded

deriving instance (Typeable MPFloat)

p2mpfrPrec :: Precision -> MPLow.Precision
p2mpfrPrec = P.fromInteger . integer

#endif

#ifdef HMPFR
import qualified Data.Number.MPFR as MPLow

{-| Multiple-precision floating-point type based on MPFR via hmpfr. -}
type MPFloat = MPLow.MPFR

p2mpfrPrec :: Precision -> MPLow.Precision
p2mpfrPrec = P.fromInteger . integer

#endif

instance HasPrecision MPFloat where
  getPrecision x = prec (P.toInteger $ MPLow.precision x)

instance CanSetPrecision MPFloat where
  setPrecision = setPrecisionUp

setPrecisionUp :: Precision -> MPFloat -> MPFloat
setPrecisionUp p = MPLow.precRound MPLow.TowardInf (p2mpfrPrec p)

setPrecisionDown :: Precision -> MPFloat -> MPFloat
setPrecisionDown p = MPLow.precRound MPLow.TowardNegInf (p2mpfrPrec p)
