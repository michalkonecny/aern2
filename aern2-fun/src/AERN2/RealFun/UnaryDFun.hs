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

import AERN2.MP.Dyadic
import AERN2.MP.Ball

import AERN2.Real
import AERN2.Interval

import AERN2.RealFun.Operations

import AERN2.RealFun.UnaryFun.Type
import AERN2.RealFun.UnaryFun.Evaluation

data UnaryDFun = UnaryDFun { _dfun_derivatives :: [UnaryFun] }

instance CanApply UnaryDFun DyadicInterval where
  type ApplyType UnaryDFun DyadicInterval = RealInterval
  apply (UnaryDFun (f : derivatives)) di@(Interval l r) =
    case derivatives of
      [] -> apply f di
      (d : rest) ->
        resViaD
        where
        resViaD = Interval (fm-err) (fm+err)
        fm = apply f (real m)
        m = (l + r)*0.5
        err = (max (abs derL) (abs derR))*(r-l)*0.5
        Interval derL derR = apply (UnaryDFun derivatives) di
