{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}

module AERN2.Real.Rounded (rational2Rounded) where

import Prelude
import qualified Prelude as P

import Numeric.Rounded

import AERN2.Real.Operations

rational2Rounded :: (Precision p, Rounding r) => Rational -> (Rounded r p)
rational2Rounded = P.fromRational

instance (Precision p, Rounding r) => CanNeg (Rounded r p)  where
    type NegType (Rounded r p) = Rounded r p
    neg = P.negate

instance  (Precision p, Rounding r) => CanNegSameType (Rounded r p)
    
instance  (Precision p, Rounding r) => CanAbs (Rounded r p) where
    type AbsType (Rounded r p) = Rounded r p
    abs = P.abs

instance  (Precision p, Rounding r) => CanAbsSameType (Rounded r p)
    
instance (Precision p, Rounding r) => CanAdd (Rounded r p) (Rounded r p) where
    type AddType (Rounded r p) (Rounded r p) = Rounded r p
    add d1 d2 = d1 P.+ d2

instance (Precision p, Rounding r) => CanAddSameType (Rounded r p)

instance (Precision p, Rounding r) => CanMul (Rounded r p) (Rounded r p) where
    type MulType (Rounded r p) (Rounded r p) = Rounded r p
    mul d1 d2 = d1 P.* d2

instance (Precision p, Rounding r) => CanMulSameType (Rounded r p)

instance (Precision p, Rounding r) => CanDiv (Rounded r p) (Rounded r p) where
    type DivType (Rounded r p) (Rounded r p) = Rounded r p
    div d1 d2 = d1 P./ d2

instance (Precision p, Rounding r) => CanDivSameType (Rounded r p)

    