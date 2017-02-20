{-|
    Module      :  AERN2.RealFun.UnaryModFun
    Description :  Real functions by evaluator on dyadic points and a modulus of continuity
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Real functions by evaluator on dyadic points and a modulus of continuity
-}

module AERN2.RealFun.UnaryModFun
(
  UnaryModFun(..)
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

-- import Control.Applicative

import Numeric.CatchingExceptions

-- import AERN2.Norm
import AERN2.MP
-- import qualified AERN2.MP.Ball as MPBall
import AERN2.MP.Dyadic

import AERN2.Real
import AERN2.Interval (DyadicInterval)
-- import qualified AERN2.Interval as Interval

import AERN2.RealFun.Operations

import AERN2.RealFun.UnaryBallFun

{-|

  Here we represent a continuous function \(f\) using its evaluation function on dyadics
  and a "local" modulus of continuity in the following sense:
  .
  For any dyadic \(d_1,d_2\in b\) with \(|d_1-d_2|\leq 2^{ -\omega(b)}\) we have
  \(|f(d_1)-f(d_2)| \leq \mathrm{diameter}(b)\).
-}
data UnaryModFun =
  UnaryModFun
  {
    _modfun_domain :: DyadicInterval
  , _modfun_eval :: CatchingNumExceptions Dyadic -> CatchingNumExceptions CauchyReal {-^ \\(d \mapsto f(d)\\) -}
  , _modfun_modulus :: MPBall -> Integer {-^ \\(\omega\\) -}
  }

instance ConvertibleExactly UnaryModFun UnaryBallFun where
  safeConvertExactly (UnaryModFun dom eval modulus) =
    Right $ UnaryBallFun dom (b2bE =<<)
    where
    b2bE b =
      do -- using the CatchingNumExceptions monad
      minVal <- minValE
      maxVal <- maxValE
      pure $ withExtremesOnPoints minVal maxVal
      where
      bMod = modulus b
      distEval = 0.5^bMod
      l,r :: MPBall
      (l,r) = endpoints b
      (_, n) = integerBounds $ (r-l)/(2*distEval)
        -- we need to split b into at least n segments and evaluate on each
        -- segment to get a reliable estimate of the range of f on b
        -- using the modulus of continuity
      pts -- points to evaluate f on
        | ptsOK pts1 = pts1
        | otherwise = pts2
        where
        pts1 = ptsWithN n
        pts2 = ptsWithN (n + 1)
        ptsOK ps =
          and $
            [(head ps) - l !<=! distEval, r - (last ps) !<=! distEval]
            ++
            zipWith (\p p' -> p' - p !<=! 2*distEval) ps (tail ps)
        ptsWithN n2 =
          map centre [ (l*(i-0.5) + r*(n2-(i-0.5))) / n2 | i <- [1..n2]]
          -- centres of n2 equal-sized segments of b=[l,r]
      values = sequence $ map (eval . catchingNumExceptions) pts
      maxValE = fmap maximum values
      minValE = fmap minimum values
      withExtremesOnPoints :: CauchyReal -> CauchyReal -> MPBall
      withExtremesOnPoints minVal maxVal =
        updateRadius (+ (radius b)) rangeOnPoints
        where
        rangeOnPoints = fromEndpoints (qaMakeQuery minVal ac) (qaMakeQuery maxVal ac)
        ac = 1 + getAccuracy b

-- instance CanApply UnaryBallDFun DyadicInterval where
--   type ApplyType UnaryBallDFun DyadicInterval = RealInterval
--   apply f di =
--     Interval (minimumOverDom f di) (maximumOverDom f di)
--
instance CanMaximiseOverDom UnaryModFun DyadicInterval where
  type MaximumOverDomType UnaryModFun DyadicInterval = CauchyReal
  maximumOverDom = maximumOverDom . unaryBallFun

instance CanMinimiseOverDom UnaryModFun DyadicInterval where
  type MinimumOverDomType UnaryModFun DyadicInterval = CauchyReal
  minimumOverDom = minimumOverDom . unaryBallFun

instance CanIntegrateOverDom UnaryModFun DyadicInterval where
  type IntegralOverDomType UnaryModFun DyadicInterval = CauchyReal
  integrateOverDom = integrateOverDom . unaryBallFun
