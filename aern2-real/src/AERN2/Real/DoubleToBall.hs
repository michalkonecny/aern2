{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AERN2.Real.DoubleToBall 
(rationals2ballDouble)
where

import Prelude hiding ((+),(*),(/),(-),fromInteger,fromRational)
import qualified Prelude as P

import Data.Convertible

import AERN2.Real.Double
import AERN2.Real.DoubleBound
import AERN2.Real.OperationsToBall
import AERN2.Real.Operations

type instance ErrorBoundType Double = DoubleBound

rationals2ballDouble :: (Rational, Rational) -> Ball Double
rationals2ballDouble (x,e) =
    Ball (convert x) (convert e)

instance CanNegB Double where
    negB d1 =
        Ball (neg d1) (convert 0.0) -- negation is exact

instance CanAddB Double Double where
    addB d1 d2 =
        Ball sumUp (DoubleBound errorBound)
        where
        errorBound = withUpwardsRounding $ sumUp P.- sumDn
        sumUp = withUpwardsRounding $ d1 + d2
        sumDn = withUpwardsRounding $ neg $ (neg d1) + (neg d2)

instance CanMulB Double Double where
    mulB d1 d2 =
        Ball prodUp (DoubleBound errorBound)
        where
        errorBound = withUpwardsRounding $ prodUp P.- prodDn 
        prodUp = withUpwardsRounding $ d1 * d2
        prodDn = withUpwardsRounding $ neg $ (neg d1) * d2

