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

-- import AERN2.MP.Dyadic
import AERN2.MP.Ball

-- import AERN2.Real
import AERN2.Interval

import AERN2.RealFun.Operations

import AERN2.RealFun.UnaryFun.Type
import AERN2.RealFun.UnaryFun.Evaluation

data UnaryDFun = UnaryDFun { _dfun_derivatives :: [UnaryFun] }

instance CanApply UnaryDFun DyadicInterval where
  type ApplyType UnaryDFun DyadicInterval = RealInterval
  apply (UnaryDFun []) _ = error "UnaryDFun "
  apply (UnaryDFun (UnaryFun _ f_o : derivatives_o)) di_o =
    rangeOnIntervalSubdivide (evalUseD derivatives_o f_o) di_o
    where
    evalUseD [] f di = evalOnIntervalGuessPrecision f di
    evalUseD (UnaryFun _ f' : rest) f di@(Interval l r)
      | f'di !>=! 0 = liftA2 fromEndpoints fl fr -- f is increasing
      | f'di !<=! 0 = liftA2 fromEndpoints fr fl -- f is decreasing
      | otherwise = fm + errBall
      where
      f'di = evalUseD rest f' di -- recursive call
      fl = f (evalOnIntervalGuessPrecision f (Interval l l))
      fr = f (evalOnIntervalGuessPrecision f (Interval r r))
      fm = f (evalOnIntervalGuessPrecision f (Interval m m))
      m = (l + r)*0.5
      errBall = f'di*((r-l)*0.5)*unitBall
      unitBall = catchingNumExceptions $ mpBall (0,1)
