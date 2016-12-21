{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.RealFun.UnaryFun.Integration
    Description :  unary function integration
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Unary function integration
-}

module AERN2.RealFun.UnaryFun.Integration
(
  integralOnIntervalSubdivide
, integralOnIntervalIncreasePrecision
)
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (flip const)
#endif

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

-- import Control.Arrow
-- import Control.Applicative

-- import Control.Lens.Operators
-- import Control.Lens (_Just)

import Numeric.CatchingExceptions

-- import AERN2.MP.Dyadic
import AERN2.MP
-- import qualified AERN2.MP.Ball as MPBall

-- import AERN2.QA
import AERN2.Real

import AERN2.Interval (DyadicInterval)
-- import AERN2.Interval (Interval(..), DyadicInterval, RealInterval)
import qualified AERN2.Interval as Interval
import AERN2.RealFun.Operations

import AERN2.RealFun.UnaryFun.Type
import AERN2.RealFun.UnaryFun.Evaluation ()

instance CanIntegrateOverDom UnaryFun DyadicInterval where
  type IntegralOverDomType UnaryFun DyadicInterval = CauchyReal
  integrateOverDom f =
    integralOnIntervalSubdivide (integralOnIntervalIncreasePrecision getArea) standardPrecisions
    -- integralOnIntervalSubdivide (\s di _ac -> (s, getArea di)) standardPrecisions
    where
    getArea di p =
      (apply f diB)*(Interval.width di)
      where
      diB =
        catchingNumExceptions $ setPrecision p $ mpBall di

integralOnIntervalIncreasePrecision ::
  (DyadicInterval -> Precision -> CatchingNumExceptions MPBall) ->
  [Precision] -> DyadicInterval -> Accuracy ->
  ([Precision], CatchingNumExceptions MPBall)
integralOnIntervalIncreasePrecision _getArea [] _di _ac =
  error "AERN2.RealFun.UnaryFun: internal error in integrateOverDom"
integralOnIntervalIncreasePrecision getArea ps@(p1_O:_) di ac =
  aux (getArea di p1_O) ps
  where
  aux diArea1 ps2@(p1:p2:p3rest) =
    maybeTrace
    (
      "integralOnIntervalIncreasePrecision: "
      ++ "\n di = " ++ show di
      ++ "\n ac = " ++ show ac
      ++ "\n p1 = " ++ show p1
      ++ "\n getAccuracy diArea1 = " ++ show (getAccuracy diArea1)
      ++ "\n p2 = " ++ show p2
      ++ "\n getAccuracy diArea2 = " ++ show (getAccuracy diArea2)
    )
    res
    where
    res
      | getAccuracy diArea1 >= ac
          = (ps2, diArea1)
      | getAccuracy diArea1 < getAccuracy diArea2
          = (p2:p3rest, diArea2)
          -- aux diArea2 (p2:p3rest)
      | otherwise
          = (ps2, diArea2)
    diArea2 = getArea di p2
  aux diArea1 ps2 = (ps2, diArea1)

integralOnIntervalSubdivide ::
  (s -> DyadicInterval -> Accuracy -> (s,CatchingNumExceptions MPBall))
  ->
  s -> (DyadicInterval -> CauchyReal)
integralOnIntervalSubdivide integralOnInterval initS diO =
    newCR "integral" [] makeQ
    where
    makeQ ac =
      ifCertainExceptionDie "integrate by subdivide" $
          integr initS diO ac
    integr s di ac
        | getAccuracy value >= ac =
            maybeTrace
            ("integrate by subdivide:"
             ++ "\n di = " ++ show di
             ++ "\n ac = " ++ show ac
             ++ "\n getAccuracy value = " ++ show (getAccuracy value)
             ++ "\n getPrecision value = " ++ show (fmap getPrecision value)
            )
            value
        | otherwise =
            maybeTrace
            ("integrate by subdivide:"
             ++ "\n di = " ++ show di
             ++ "\n ac = " ++ show ac
             ++ "\n getAccuracy value = " ++ show (getAccuracy value)
             ++ "\n getPrecision value = " ++ show (fmap getPrecision value)
            ) $
            (integr s' diL (ac+1))
            +
            (integr s' diR (ac+1))
        where
        (diL, diR) = Interval.split di
        (s', value) = integralOnInterval s di ac
