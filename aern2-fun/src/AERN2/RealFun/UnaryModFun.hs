{-# LANGUAGE CPP #-}
#define DEBUG
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
-- (
--   UnaryModFun(..), unaryModFun
-- )
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (\ (_ :: String) t -> t)
#endif

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

-- import Control.Applicative

import Numeric.CatchingExceptions

import AERN2.MP
-- import qualified AERN2.MP.Ball as MPBall
import AERN2.MP.Dyadic

import AERN2.Real
import AERN2.Interval

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
  , _modfun_modulus :: MPBall -> Integer -> Integer {-^ \\(\omega\\) -}
  }

instance HasDomain UnaryModFun where
  type Domain UnaryModFun = DyadicInterval
  getDomain f = _modfun_domain f

instance ConvertibleExactly (DyadicInterval, Integer) UnaryModFun where
  safeConvertExactly (dom,n) =
    Right $ UnaryModFun dom  (const $ pure (real n)) (const $ const constFnModulus)

constFnModulus :: Integer
constFnModulus = -30 -- arbitrary, ideally -infinity...

unaryModFun :: (ConvertibleExactly t UnaryModFun) => t -> UnaryModFun
unaryModFun = convertExactly

instance HasVars UnaryModFun where
  type Var UnaryModFun = ()
  varFn sampleFn () =
    UnaryModFun dom (fmap real) (const id)
    where
    dom = getDomain sampleFn

instance ConvertibleExactly UnaryModFun UnaryBallFun where
  safeConvertExactly (UnaryModFun dom eval modulus) =
    Right $ UnaryBallFun dom (b2bE =<<)
    where
    b2bE b =
      maybeTrace ("UnaryModFun b2bE: "
        ++ "b = "  ++ show b
        ++ ", ac = "  ++ show ac
        ++ ", distEval = "  ++ show distEval
        ++ ", n = "  ++ show n
      ) $
      do -- using the CatchingNumExceptions monad
      minVal <- minValE
      maxVal <- maxValE
      pure $ withExtremesOnPoints minVal maxVal
      where
      ac = getFiniteAccuracy b
      distEval = 0.5^(modulus b (fromAccuracy ac))
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
        ptsOK [] = False
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
        updateRadius (+ (errorBound $ 0.5^(fromAccuracy ac))) rangeOnPoints
        where
        rangeOnPoints = fromEndpoints (qaMakeQuery minVal ac) (qaMakeQuery maxVal ac)

instance CanApply UnaryModFun MPBall where
  type ApplyType UnaryModFun MPBall = MPBall
  apply = apply . unaryBallFun

instance CanApply UnaryModFun DyadicInterval where
  type ApplyType UnaryModFun DyadicInterval = RealInterval
  apply = apply . unaryBallFun

instance CanMaximiseOverDom UnaryModFun DyadicInterval where
  type MaximumOverDomType UnaryModFun DyadicInterval = CauchyReal
  maximumOverDom = maximumOverDom . unaryBallFun

instance CanMinimiseOverDom UnaryModFun DyadicInterval where
  type MinimumOverDomType UnaryModFun DyadicInterval = CauchyReal
  minimumOverDom = minimumOverDom . unaryBallFun

instance CanIntegrateOverDom UnaryModFun DyadicInterval where
  type IntegralOverDomType UnaryModFun DyadicInterval = CauchyReal
  integrateOverDom = integrateOverDom . unaryBallFun

{- selected field ops -}

sinecos_ModFun :: UnaryModFun
sinecos_ModFun =
  -- sin(10*x)+cos(20*x)
  10*x+20*x
  where
  x = varFn (unaryModFun (unaryIntervalDom, 0)) ()

unaryIntervalDom :: DyadicInterval
unaryIntervalDom = dyadicInterval (-1,1)

instance CanAddAsymmetric UnaryModFun UnaryModFun where
  add (UnaryModFun dom1 eval1 modulus1) (UnaryModFun dom2 eval2 modulus2)
    | dom1 == dom2 =
      UnaryModFun dom1 (\d -> eval1 d + eval2 d) modulus'
    | otherwise =
      error "UnaryModFun: add: incompatible domains"
    where
    modulus' b i = max (modulus1 b (i+1)) (modulus2 b (i+1))

instance CanMulAsymmetric Integer UnaryModFun where
  type MulType Integer UnaryModFun = UnaryModFun
  mul n (UnaryModFun dom eval modulus) =
    UnaryModFun dom (\d -> fmap (n *) (eval d)) modulus'
    where
    modulus' b =
      case (getNormLog (abs n)) of
        NormZero -> const constFnModulus
        NormBits nn -> \ i -> modulus b i + nn

instance CanMulAsymmetric UnaryModFun Integer where
  type MulType UnaryModFun Integer = UnaryModFun
  mul f n = mul n f
