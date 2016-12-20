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

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
-- shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace = if shouldTrace then trace else const id
_dummy :: ()
_dummy = maybeTrace "dummy" ()

instance CanIntegrateOverDom UnaryFun DyadicInterval where
  type IntegralOverDomType UnaryFun DyadicInterval = CauchyReal
  integrateOverDom f =
    integralOnIntervalSubdivide (integralOnIntervalIncreasePrecision getArea) standardPrecisions
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
integralOnIntervalIncreasePrecision getArea ps@(p1:_) di _ac =
  aux (getArea di p1) ps
  where
  aux diArea1 (_p1:p2:p3rest)
    | getAccuracy diArea1 < getAccuracy diArea2 =
        (p2:p3rest, diArea2)
        -- aux diArea2 (p2:p3rest)
    | otherwise =
        (ps, diArea2)
    where
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
            )
            value
        | otherwise =
            (integr s' diL (ac+1))
            +
            (integr s' diR (ac+1))
        where
        (diL, diR) = Interval.split di
        (s', value) = integralOnInterval s di ac
