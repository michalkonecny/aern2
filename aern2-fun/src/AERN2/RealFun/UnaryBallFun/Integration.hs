{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.RealFun.UnaryBallFun.Integration
    Description :  unary function integration
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Unary function integration
-}

module AERN2.RealFun.UnaryBallFun.Integration
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

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

-- import Control.Arrow
-- import Control.Applicative

-- import Control.Lens.Operators
-- import Control.Lens (_Just)



-- import AERN2.MP.Dyadic
import AERN2.MP
-- import qualified AERN2.MP.Ball as MPBall

-- import AERN2.QA
import AERN2.Real

import AERN2.Interval (DyadicInterval)
-- import AERN2.Interval (Interval(..), DyadicInterval, RealInterval)
import qualified AERN2.Interval as Interval
import AERN2.RealFun.Operations

import AERN2.RealFun.UnaryBallFun.Type
import AERN2.RealFun.UnaryBallFun.Evaluation ()

instance CanIntegrateOverDom UnaryBallFun DyadicInterval where
  type IntegralOverDomType UnaryBallFun DyadicInterval = CauchyRealCN
  integrateOverDom f =
    integralOnIntervalSubdivide (integralOnIntervalIncreasePrecision getArea)
      (\ (AccuracySG _ acG) -> standardPrecisions (ac2prec acG))
    -- integralOnIntervalSubdivide (\s di _ac -> (s, getArea di)) standardPrecisions
    where
    getArea di p =
      (apply f diB)*(Interval.width di)
      where
      diB = raisePrecisionIfBelow p $ mpBall di

integralOnIntervalIncreasePrecision ::
  (DyadicInterval -> Precision -> CN MPBall) ->
  [Precision] -> DyadicInterval -> Accuracy ->
  ([Precision], CN MPBall)
integralOnIntervalIncreasePrecision _getArea [] _di _ac =
  error "AERN2.RealFun.UnaryBallFun: internal error in integrateOverDom"
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
  (s -> DyadicInterval -> Accuracy -> (s, CN MPBall))
  ->
  (AccuracySG -> s) -> (DyadicInterval -> CauchyRealCN)
integralOnIntervalSubdivide integralOnInterval initS diO =
    newCRCN "integral" [] makeQ
    where
    makeQ _ ac =
      integr (initS ac) diO (_acStrict ac)
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
