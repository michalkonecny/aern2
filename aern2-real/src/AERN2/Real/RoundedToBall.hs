{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module AERN2.Real.RoundedToBall where

import Prelude hiding ((+),(*),(/),(-),fromInteger,fromRational)
import qualified Prelude as P

import Numeric.Rounded
import Data.Coerce (coerce)

import AERN2.Real.Rounded (RoundedP, rational2Rounded)
import AERN2.Real.OperationsToBall
import AERN2.Real.Operations

type instance ErrorBoundType (RoundedP p) = RoundedP p
{- 
    TODO: 
    ErrorBoundType (RoundedP p) = DoubleBound
    
    This would be much more efficient.  Add this when we get mixed Rounded-Double operations. 

-}

instance (Precision p) => CanNegB (RoundedP p) where
    negB d1 =
        Ball (neg d1) (rational2Rounded 0.0) -- negation is exact

instance (Precision p) => CanAddB (RoundedP p) (RoundedP p) where
    addB d1 (d2 :: RoundedP p) =
        Ball (coerce sumUp) errorBound
        where
        errorBound = sumUp P.- (coerce sumDn)
        sumUp = d1Up P.+ d2Up
        sumDn = d1Dn P.+ d2Dn
        d1Up, d2Up :: Rounded TowardInf p
        d1Up = coerce d1
        d2Up = coerce d2
        d1Dn, d2Dn :: Rounded TowardNegInf p
        d1Dn = coerce d1
        d2Dn = coerce d2

instance (Precision p) => CanMulB (RoundedP p) (RoundedP p) where
    mulB d1 (d2 :: RoundedP p) =
        Ball (coerce prodUp) errorBound
        where
        errorBound = prodUp P.- (coerce prodDn)
        prodUp = d1Up P.* d2Up
        prodDn = d1Dn P.* d2Dn
        d1Up, d2Up :: Rounded TowardInf p
        d1Up = coerce d1
        d2Up = coerce d2
        d1Dn, d2Dn :: Rounded TowardNegInf p
        d1Dn = coerce d1
        d2Dn = coerce d2

