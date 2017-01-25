{-|
    Module      :  AERN2.RealFun.UnaryDFun
    Description :  Real functions by MPBall evaluators, including derivatives
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Real functions by MPBall evaluators, including derivatives
-}

module AERN2.RealFun.UnaryDFun
(
  UnaryDFun(..)
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

import Control.Applicative

import Numeric.CatchingExceptions

import AERN2.Norm
import AERN2.MP
-- import qualified AERN2.MP.Ball as MPBall
import AERN2.MP.Dyadic

import AERN2.Real
import AERN2.Interval (Interval(..), DyadicInterval, RealInterval)
import qualified AERN2.Interval as Interval

import AERN2.RealFun.Operations

import AERN2.RealFun.UnaryFun.Type
import AERN2.RealFun.UnaryFun.Evaluation
import AERN2.RealFun.UnaryFun.Integration

data UnaryDFun = UnaryDFun { _dfun_derivatives :: [UnaryFun] }


instance CanApply UnaryDFun DyadicInterval where
  type ApplyType UnaryDFun DyadicInterval = RealInterval
  apply f di =
    Interval (minimumOverDom f di) (maximumOverDom f di)

instance CanMaximiseOverDom UnaryDFun DyadicInterval where
  type MaximumOverDomType UnaryDFun DyadicInterval = CauchyReal
  maximumOverDom (UnaryDFun []) _ = error "maximumOverDom UnaryDFun []"
  maximumOverDom (UnaryDFun (UnaryFun _ f_o : derivatives_o)) di_o =
    maximumOnIntervalSubdivide (evalUseD derivatives_o f_o) di_o

instance CanMinimiseOverDom UnaryDFun DyadicInterval where
  type MinimumOverDomType UnaryDFun DyadicInterval = CauchyReal
  minimumOverDom (UnaryDFun []) _ = error "minimumOverDom UnaryDFun []"
  minimumOverDom (UnaryDFun (UnaryFun _ f_o : derivatives_o)) di_o =
    minimumOnIntervalSubdivide (evalUseD derivatives_o f_o) di_o

evalUseD ::
  [UnaryFun] ->
  (CatchingNumExceptions MPBall -> CatchingNumExceptions MPBall) ->
  DyadicInterval ->
  (Maybe (CatchingNumExceptions MPBall, CatchingNumExceptions MPBall), CatchingNumExceptions MPBall)
evalUseD [] f di = (Nothing, evalOnIntervalGuessPrecision f di)
evalUseD (UnaryFun _ f' : rest) f di@(Interval l r)
  | f'di !>=! 0 = (Just (fl,fr), liftA2 fromEndpoints fl fr)
  | f'di !<=! 0 = (Just (fr,fl), liftA2 fromEndpoints fr fl)
  | otherwise = (Nothing, fm + errBall)
  where
  (_, f'di) = evalUseD rest f' di -- recursive call
  fl = f $ catchingNumExceptions $ raisePrecisionIfBelow p $ mpBall l
  fr = f $ catchingNumExceptions $ raisePrecisionIfBelow p $ mpBall r
  fm = f $ catchingNumExceptions $ raisePrecisionIfBelow p $ mpBall m
  m = (l + r)*half
  errBall = f'di*((r-l)*half)*unitBall
  unitBall = catchingNumExceptions $ mpBall (-1,1)
  half = dyadic 0.5

  p =
      case nl of
          NormBits i -> prec $ max 10 (-i)
          NormZero -> getPrecision l
  nl = getNormLog (r - l)


instance CanIntegrateOverDom UnaryDFun DyadicInterval where
  type IntegralOverDomType UnaryDFun DyadicInterval = CauchyReal
  integrateOverDom (UnaryDFun []) = error "integrating UnaryDFun []"
  integrateOverDom (UnaryDFun [f]) = integrateOverDom f
  integrateOverDom (UnaryDFun (f : f' : _)) =
    integralOnIntervalSubdivide (integralOnIntervalIncreasePrecision getArea) standardPrecisions
    where
    getArea di p =
      (apply f diM)*diW+errB
      where
      diW = Interval.width di
      errB = ((deriv - deriv)/2)*((diW*0.5)^2)*0.5
      deriv = apply f' (catchingNumExceptions diB)
      diM = catchingNumExceptions $ centreAsBall diB
      diB = setPrecision p $ mpBall di
