{-|
    Module      :  AERN2.Real
    Description :  Exact real numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Exact real numbers represented by Cauchy sequences of MPBalls.
-}
module AERN2.Real
(
   -- * sequences of approximants
   CSequence (..), 
   cseqPrecisions, cseqIndexForPrecision, 
   cseqFromWithCurrentPrec, cseqFromPrecFunction,
   unsafeApproximationExtension,
   -- * real numbers and conversions
   CReal, 
   creal, HasCReals, CanBeCReal,
   pi,
   crealFromWithCurrentPrec, crealFromPrecFunction,
   -- * limits
   HasLimits(..),
   -- * lazy Kleeneans
   CKleenean, CanBeCKleenean, ckleenean, 
   CanSelect(..), CanSelectBool, CanSelectCNBool,
   CanAndOrCountable(..),
   -- * extracting approximations
   CanExtractApproximation(..), (?), bits, prec,
   -- * abstract real numbers
   RealNumber
)
where

import AERN2.Limit
import AERN2.Select
import AERN2.MP
import AERN2.MP.WithCurrentPrec
import AERN2.Real.Type
import AERN2.Real.Comparisons ()
import AERN2.Real.CKleenean
import AERN2.Real.Field ()
import AERN2.Real.Limit ()
import AERN2.Real.Elementary (pi)
-- import AERN2.Real.Tests ()

import MixedTypesNumPrelude
-- import qualified Prelude as P
import GHC.TypeLits ( KnownNat )
import Numeric.CollectErrors (CanTakeCNErrors)

-- import Text.Printf
-- -- import AERN2.MP.Dyadic

class
    (OrderedField r
    , HasLimitsSameType Int r
    , HasLimitsSameType Integer r
    , HasLimitsSameType Rational r
    , CanTakeCNErrors r
    , CanSelectCNBool (OrderCompareType r r)
    , EqCompareType Integer r ~ OrderCompareType r r
    , EqCompareType r Integer ~ OrderCompareType r r
    , EqCompareType Int r ~ OrderCompareType r r
    , EqCompareType r Int ~ OrderCompareType r r
    , EqCompareType Rational r ~ OrderCompareType r r
    , EqCompareType r Rational ~ OrderCompareType r r
    , OrderCompareType Integer r ~ OrderCompareType r r
    , OrderCompareType r Integer ~ OrderCompareType r r
    , OrderCompareType Int r ~ OrderCompareType r r
    , OrderCompareType r Int ~ OrderCompareType r r
    , OrderCompareType Rational r ~ OrderCompareType r r
    , OrderCompareType r Rational ~ OrderCompareType r r
    -- , HasIfThenElseSameType (OrderCompareType r r) r
    )
    => 
    RealNumber r

instance RealNumber CReal
instance
    (KnownNat p) => 
    RealNumber (WithCurrentPrec p (CN MPBall))
