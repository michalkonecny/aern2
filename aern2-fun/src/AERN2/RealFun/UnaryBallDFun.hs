{-|
    Module      :  AERN2.RealFun.UnaryBallDFun
    Description :  Real functions by MPBall evaluators, including derivatives
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Real functions by MPBall evaluators, including derivatives
-}

module AERN2.RealFun.UnaryBallDFun
(
  UnaryBallDFun(..)
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import Control.Applicative



import AERN2.Norm
import AERN2.MP
-- import qualified AERN2.MP.Ball as MPBall
import AERN2.MP.Dyadic

import AERN2.Real
import AERN2.Interval (Interval(..), DyadicInterval, RealInterval)
import qualified AERN2.Interval as Interval

import AERN2.RealFun.Operations

import AERN2.RealFun.UnaryBallFun.Type
import AERN2.RealFun.UnaryBallFun.Evaluation
import AERN2.RealFun.UnaryBallFun.Integration

data UnaryBallDFun = UnaryBallDFun { _dballfun_derivatives :: [UnaryBallFun] }


instance CanApply UnaryBallDFun DyadicInterval where
  type ApplyType UnaryBallDFun DyadicInterval = RealInterval
  apply f di =
    Interval (minimumOverDom f di) (maximumOverDom f di)

instance CanMaximiseOverDom UnaryBallDFun DyadicInterval where
  type MaximumOverDomType UnaryBallDFun DyadicInterval = CauchyReal
  maximumOverDom (UnaryBallDFun []) _ = error "maximumOverDom UnaryBallDFun []"
  maximumOverDom (UnaryBallDFun (UnaryBallFun _ f_o : derivatives_o)) di_o =
    maximumOnIntervalSubdivide (evalUseD derivatives_o f_o) di_o

instance CanMinimiseOverDom UnaryBallDFun DyadicInterval where
  type MinimumOverDomType UnaryBallDFun DyadicInterval = CauchyReal
  minimumOverDom (UnaryBallDFun []) _ = error "minimumOverDom UnaryBallDFun []"
  minimumOverDom (UnaryBallDFun (UnaryBallFun _ f_o : derivatives_o)) di_o =
    minimumOnIntervalSubdivide (evalUseD derivatives_o f_o) di_o

evalUseD ::
  [UnaryBallFun] ->
  (MPBall -> CN MPBall) ->
  DyadicInterval ->
  (Maybe (CN MPBall, CN MPBall), CN MPBall)
evalUseD [] f di = (Nothing, evalOnIntervalGuessPrecision f di)
evalUseD (UnaryBallFun _ f' : rest) f di@(Interval l r)
  | f'di !>=! 0 = (Just (fl,fr), liftA2 fromEndpoints fl fr)
  | f'di !<=! 0 = (Just (fr,fl), liftA2 fromEndpoints fr fl)
  | otherwise = (Nothing, fm + errBall)
  where
  (_, f'di) = evalUseD rest f' di -- recursive call
  fl = f $ raisePrecisionIfBelow p $ mpBall l
  fr = f $ raisePrecisionIfBelow p $ mpBall r
  fm = f $ raisePrecisionIfBelow p $ mpBall m
  m = (l + r)*half
  errBall = f'di*((r-l)*half)*unitBall
  unitBall = mpBall (0,1)
  half = dyadic 0.5

  p =
      case nl of
          NormBits i -> prec $ max 10 (-5*i)
          NormZero -> getPrecision l
  nl = getNormLog (r - l)


instance CanIntegrateOverDom UnaryBallDFun DyadicInterval where
  type IntegralOverDomType UnaryBallDFun DyadicInterval = CauchyRealCN
  integrateOverDom (UnaryBallDFun []) = error "integrating UnaryBallDFun []"
  integrateOverDom (UnaryBallDFun [f]) = integrateOverDom f
  integrateOverDom (UnaryBallDFun (f : f' : _)) =
    integralOnIntervalSubdivide (integralOnIntervalIncreasePrecision getArea)
      (\ (AccuracySG _ acG) -> standardPrecisions (ac2prec acG))
    where
    getArea di p =
      (apply f diM)*diW+errB
      where
      diW = Interval.width di
      errB = ((deriv - deriv)/!2)*((diW*0.5)^!2)*0.5
      deriv = apply f' diB
      diM = centreAsBall diB
      diB = setPrecision p $ mpBall di
