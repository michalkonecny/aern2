{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module AERN2.Real.Rounded (rational2Rounded, RoundedP) where

import Prelude
import qualified Prelude as P
import GHC.TypeLits (Nat)
import Data.Convertible

import Numeric.Rounded

import AERN2.Real.Operations

type RoundedP p = Rounded TowardInf (p :: Nat)


instance (Precision p) => Convertible Rational (RoundedP p) where
    safeConvert a = Right $ rational2Rounded a

rational2Rounded :: (Precision p) => Rational -> (RoundedP p)
rational2Rounded = P.fromRational

instance (Precision p) => CanNeg (RoundedP p)  where
    type NegType (RoundedP p) = RoundedP p
    neg = P.negate

instance  (Precision p) => CanNegSameType (RoundedP p)
    
instance  (Precision p) => CanAbs (RoundedP p) where
    type AbsType (RoundedP p) = RoundedP p
    abs = P.abs

instance  (Precision p) => CanAbsSameType (RoundedP p)
    
instance (Precision p) => CanAdd (RoundedP p) (RoundedP p) where
    type AddType (RoundedP p) (RoundedP p) = RoundedP p
    add d1 d2 = d1 P.+ d2

instance (Precision p) => CanAddThis (RoundedP p) (RoundedP p)
instance (Precision p) => CanAddSameType (RoundedP p)

instance (Precision p) => CanMul (RoundedP p) (RoundedP p) where
    type MulType (RoundedP p) (RoundedP p) = RoundedP p
    mul d1 d2 = d1 P.* d2

instance (Precision p) => CanMulBy (RoundedP p) (RoundedP p)
instance (Precision p) => CanMulSameType (RoundedP p)

instance (Precision p) => CanDiv (RoundedP p) (RoundedP p) where
    type DivType (RoundedP p) (RoundedP p) = RoundedP p
    div d1 d2 = d1 P./ d2

instance (Precision p) => CanDivBy (RoundedP p) (RoundedP p)
instance (Precision p) => CanDivSameType (RoundedP p)

    