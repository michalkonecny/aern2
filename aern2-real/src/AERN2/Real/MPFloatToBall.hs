{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AERN2.Real.MPFloatToBall 
(rationals2ballMPFloat)
where

import Prelude hiding ((+),(*),(/),(-),fromInteger,fromRational)
--import qualified Prelude as P

import AERN2.Real.MPFloat
import AERN2.Real.OperationsToBall
import AERN2.Real.Operations

type instance ErrorBoundType MPFloat = MPFloat

rationals2ballMPFloat :: Precision -> (Rational, Rational) -> Ball MPFloat
rationals2ballMPFloat p (x,e) =
    Ball (rational2MPFloat p x) (rational2MPFloat p e)

instance CanNegB MPFloat where
    negB x1 =
        Ball (neg x1) zeroMPFloat -- negation is exact

instance CanAddB MPFloat MPFloat where
    addB d1 d2 =
        Ball sumUp errorBound
        where
        errorBound = sumUp - sumDn
        sumUp = d1 + d2
        sumDn = neg $ (neg d1) + (neg d2)

instance CanMulB MPFloat MPFloat where
    mulB d1 d2 =
        Ball prodUp errorBound
        where
        errorBound = prodUp - prodDn 
        prodUp = d1 * d2
        prodDn = neg $ (neg d1) * d2
