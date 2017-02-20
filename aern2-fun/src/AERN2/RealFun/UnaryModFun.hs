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

import AERN2.Norm
import AERN2.MP
-- import qualified AERN2.MP.Ball as MPBall
import AERN2.MP.Dyadic

import AERN2.Real
import AERN2.Interval (Interval(..), DyadicInterval)
import qualified AERN2.Interval as Interval

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
-- instance CanMaximiseOverDom UnaryBallDFun DyadicInterval where
--   type MaximumOverDomType UnaryBallDFun DyadicInterval = CauchyReal
--   maximumOverDom (UnaryBallDFun []) _ = error "maximumOverDom UnaryBallDFun []"
--   maximumOverDom (UnaryBallDFun (UnaryBallFun _ f_o : derivatives_o)) di_o =
--     maximumOnIntervalSubdivide (evalUseD derivatives_o f_o) di_o
--
-- instance CanMinimiseOverDom UnaryBallDFun DyadicInterval where
--   type MinimumOverDomType UnaryBallDFun DyadicInterval = CauchyReal
--   minimumOverDom (UnaryBallDFun []) _ = error "minimumOverDom UnaryBallDFun []"
--   minimumOverDom (UnaryBallDFun (UnaryBallFun _ f_o : derivatives_o)) di_o =
--     minimumOnIntervalSubdivide (evalUseD derivatives_o f_o) di_o
--
-- evalUseD ::
--   [UnaryBallFun] ->
--   (CatchingNumExceptions MPBall -> CatchingNumExceptions MPBall) ->
--   DyadicInterval ->
--   (Maybe (CatchingNumExceptions MPBall, CatchingNumExceptions MPBall), CatchingNumExceptions MPBall)
-- evalUseD [] f di = (Nothing, evalOnIntervalGuessPrecision f di)
-- evalUseD (UnaryBallFun _ f' : rest) f di@(Interval l r)
--   | f'di !>=! 0 = (Just (fl,fr), liftA2 fromEndpoints fl fr)
--   | f'di !<=! 0 = (Just (fr,fl), liftA2 fromEndpoints fr fl)
--   | otherwise = (Nothing, fm + errBall)
--   where
--   (_, f'di) = evalUseD rest f' di -- recursive call
--   fl = f $ catchingNumExceptions $ raisePrecisionIfBelow p $ mpBall l
--   fr = f $ catchingNumExceptions $ raisePrecisionIfBelow p $ mpBall r
--   fm = f $ catchingNumExceptions $ raisePrecisionIfBelow p $ mpBall m
--   m = (l + r)*half
--   errBall = f'di*((r-l)*half)*unitBall
--   unitBall = catchingNumExceptions $ mpBall (-1,1)
--   half = dyadic 0.5
--
--   p =
--       case nl of
--           NormBits i -> prec $ max 10 (-i)
--           NormZero -> getPrecision l
--   nl = getNormLog (r - l)
--
--
-- instance CanIntegrateOverDom UnaryBallDFun DyadicInterval where
--   type IntegralOverDomType UnaryBallDFun DyadicInterval = CauchyReal
--   integrateOverDom (UnaryBallDFun []) = error "integrating UnaryBallDFun []"
--   integrateOverDom (UnaryBallDFun [f]) = integrateOverDom f
--   integrateOverDom (UnaryBallDFun (f : f' : _)) =
--     integralOnIntervalSubdivide (integralOnIntervalIncreasePrecision getArea) standardPrecisions
--     where
--     getArea di p =
--       (apply f diM)*diW+errB
--       where
--       diW = Interval.width di
--       errB = ((deriv - deriv)/2)*((diW*0.5)^2)*0.5
--       deriv = apply f' (catchingNumExceptions diB)
--       diM = catchingNumExceptions $ centreAsBall diB
--       diB = setPrecision p $ mpBall di
