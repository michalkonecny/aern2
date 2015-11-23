{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AERN2.Real.DoubleToBall where

import Prelude hiding ((+),(*),(/),(-),fromInteger,fromRational)
import qualified Prelude as P

import AERN2.Real.Double
import AERN2.Real.DoubleBound
import AERN2.Real.OperationsToBall
import AERN2.Real.Operations

newtype DoubleB = DoubleB Double

instance Show DoubleB where
    show (DoubleB d) = show d

type instance ErrorBoundType Double = DoubleBound

instance CanNegB Double where
    negB d1 =
        Ball (neg d1) (rational2DoubleBound 0.0) -- negation is exact

instance CanAddB Double Double where
    addB d1 d2 =
        Ball sumUp (DoubleBound $ sumUp P.- sumDn)
        where
        sumUp = withUpwardsRounding $ d1 + d2
        sumDn = withUpwardsRounding $ neg $ (neg d1) + (neg d2)

instance CanMulB Double Double where
    mulB d1 d2 =
        Ball prodUp (DoubleBound $ prodUp P.- prodDn)
        where
        prodUp = withUpwardsRounding $ d1 * d2
        prodDn = withUpwardsRounding $ neg $ (neg d1) * d2

{-
    TODO: add instances of the other classes in OperationsToBall.
-}
