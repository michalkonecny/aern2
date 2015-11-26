{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AERN2.Real.MPFloatToBall 
(MPBall, rational2MPBall, rationals2MPBall, piBallUsingPrecision)
where

import Prelude hiding ((+),(*),(/),(-),fromInteger,fromRational)
--import qualified Prelude as P

import AERN2.Real.OperationsToBall
import AERN2.Real.Operations
import AERN2.Real.MPFloat
import AERN2.Real.IntegerRational ()

type instance ErrorBoundType MPFloat = MPFloat

type MPBall = Ball MPFloat

rationals2MPBall :: Precision -> (Rational, Rational) -> MPBall 
rationals2MPBall p (x,e) =
    Ball xUp (xe + eUp)
    where
    (Ball xUp xe) = rational2MPBall p x
    eUp = fromRationalUp p e
    
rational2MPBall :: Precision -> Rational -> MPBall
rational2MPBall p x =
    Ball xUp (xUp - xDn)
    where
    xUp = fromRationalUp p x
    xDn = fromRationalDown p x

piBallUsingPrecision :: Precision -> MPBall
piBallUsingPrecision p =
    Ball piU (piU - piD)
    where
    piU = piUp p
    piD = piDown p

instance CanNegB MPFloat where
    negB x1 =
        Ball (neg x1) zero -- negation is exact

instance CanAddB MPFloat MPFloat where
    addB d1 d2 =
        Ball sumUp errorBound
        where
        errorBound = sumUp - sumDn
        sumUp = d1 + d2
        sumDn = neg $ (neg d1) + (neg d2)

instance CanSubB MPFloat MPFloat where
    subB d1 d2 =
        Ball sumUp errorBound
        where
        errorBound = sumUp - sumDn
        sumUp = d1 - d2
        sumDn = neg $ (neg d1) - (neg d2)

instance CanMulB MPFloat MPFloat where
    mulB d1 d2 =
        Ball prodUp errorBound
        where
        errorBound = prodUp - prodDn 
        prodUp = d1 * d2
        prodDn = neg $ (neg d1) * d2
