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
   -- * real numbers and conversions
   CReal, 
   CSequence (..), 
   creal, HasCReals, CanBeCReal,
   cseqPrecisions, cseqIndexForPrecision, 
   cseqFromWithCurrentPrec, cseqFromPrecFunction,
   pi,
   crealFromWithCurrentPrec, crealFromPrecFunction,
   -- * limits
   HasLimits(..),
   -- * lazy Kleeneans
   CKleenean, CanBeCKleenean, ckleenean, CanSelect(..),
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
import GHC.TypeLits

-- import Text.Printf
-- -- import AERN2.MP.Dyadic

class
    (OrderedField r
    , HasLimits Int r
    , HasLimits Integer r
    , HasLimits Rational r
    , CanSelect (OrderCompareType r r)
    , (CanTestCertainly (SelectType (OrderCompareType r r))))
    => 
    RealNumber r

instance RealNumber CReal
instance
    (KnownNat p) => 
    RealNumber (WithCurrentPrec p (CN MPBall))
