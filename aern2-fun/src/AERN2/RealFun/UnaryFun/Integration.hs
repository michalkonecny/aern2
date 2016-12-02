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

-- import AERN2.Norm
import AERN2.MP.Accuracy
-- import AERN2.MP.Precision
-- import AERN2.MP.Dyadic
import AERN2.MP.Ball (MPBall, mpBall)
-- import AERN2.MP.Ball (MPBall, mpBall, IsBall(..), IsInterval(..), setPrecisionAtLeastAccuracy)
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
    integralOnIntervalSubdivide integralOnInterval
    where
    integralOnInterval di ac = (apply f diB)*(Interval.width di)
      where
      diB =
        catchingNumExceptions $
          setPrecisionAtLeastAccuracy (ac + 100) $
            mpBall di

integralOnIntervalSubdivide ::
  (DyadicInterval -> Accuracy -> CatchingNumExceptions MPBall)
  ->
  (DyadicInterval -> CauchyReal)
integralOnIntervalSubdivide integralOnInterval diO =
    newCR "integral" [] makeQ
    where
    makeQ ac =
      ifCertainExceptionDie "integrate by subdivide" $
          integr diO ac
    integr di ac
        | getAccuracy value >= ac =
            maybeTrace
            ("integrate by subdivide:"
             ++ "\n di = " ++ show di
             ++ "\n ac = " ++ show ac
             ++ "\n getAccuracy value = " ++ show (getAccuracy value)
            )
            value
        | otherwise =
            (integr diL (ac+1))
            +
            (integr diR (ac+1))
        where
        (diL, diR) = Interval.split di
        value = integralOnInterval di ac
