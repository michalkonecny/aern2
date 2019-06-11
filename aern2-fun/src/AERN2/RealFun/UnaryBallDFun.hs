{-# LANGUAGE TemplateHaskell #-}
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

import Control.CollectErrors

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

instance HasFnConstructorInfo UnaryBallDFun where
  type FnConstructorInfo UnaryBallDFun = DyadicInterval
  getFnConstructorInfo (UnaryBallDFun (f:_)) = getDomain f
  getFnConstructorInfo (UnaryBallDFun _) = error "getFnConstructorInfo UnaryBallDFun: empty"

instance HasVars UnaryBallDFun where
  type Var UnaryBallDFun = ()
  varFn dom () = UnaryBallDFun [varFn dom (), constFn dom 1]

instance HasAccuracy UnaryBallDFun where
  getAccuracy _f = Exact
  getFiniteAccuracy _f = error "getFiniteAccuracy not defined for UnaryBallDFun"

instance HasAccuracyGuide UnaryBallDFun where
  getAccuracyGuide _f = NoInformation

instance CanSetAccuracyGuide UnaryBallDFun where
  setAccuracyGuide _ f = f

instance (SuitableForCE es) => CanEnsureCE es UnaryBallDFun where
  type EnsureCE es UnaryBallDFun = UnaryBallDFun
  type EnsureNoCE es UnaryBallDFun = UnaryBallDFun
  ensureCE _sample_es = id
  deEnsureCE _sample_es = Right
  ensureNoCE _sample_es v = (Just v, mempty)
  noValueECE _sample_vCE _es = error "UnaryBallDFun noValueCE not implemented yet"
  prependErrorsECE _sample_vCE _es = error "UnaryBallDFun prependErrorsECE not implemented yet"

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
  (CN MPBall -> CN MPBall) ->
  DyadicInterval ->
  (Maybe (CN MPBall, CN MPBall), CN MPBall)
evalUseD [] f di = (Nothing, evalOnIntervalGuessPrecision f di)
evalUseD (UnaryBallFun _ f' : rest) f di@(Interval l r)
  | f'di !>=! 0 = (Just (fl,fr), liftA2 fromEndpoints fl fr)
  | f'di !<=! 0 = (Just (fr,fl), liftA2 fromEndpoints fr fl)
  | otherwise = (Nothing, fm + errBall)
  where
  (_, f'di) = evalUseD rest f' di -- recursive call
  fl = f $ cn $ raisePrecisionIfBelow p $ mpBall l
  fr = f $ cn $ raisePrecisionIfBelow p $ mpBall r
  fm = f $ cn $ raisePrecisionIfBelow p $ mpBall m
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

instance CanMinMaxAsymmetric UnaryBallDFun UnaryBallDFun where
  min (UnaryBallDFun as) (UnaryBallDFun bs) =
    UnaryBallDFun (zipWith min as bs)
  max (UnaryBallDFun as) (UnaryBallDFun bs) =
    UnaryBallDFun (zipWith max as bs)

instance CanNeg UnaryBallDFun where
  negate (UnaryBallDFun as) = UnaryBallDFun (map negate as)

instance CanAddAsymmetric UnaryBallDFun UnaryBallDFun where
  add (UnaryBallDFun as) (UnaryBallDFun bs) =
    UnaryBallDFun (zipWith (+) as bs)

instance CanSub UnaryBallDFun UnaryBallDFun where
  sub (UnaryBallDFun as) (UnaryBallDFun bs) =
    UnaryBallDFun (zipWith (-) as bs)

instance CanMulAsymmetric UnaryBallDFun UnaryBallDFun where
  mul (UnaryBallDFun (a:ad:_)) (UnaryBallDFun (b:bd:_)) =
    UnaryBallDFun [a*b, ad*b+a*bd]
  mul _ _ = error "UnaryBallDFun: mul operand does not contain a derivative"

instance CanDiv UnaryBallDFun UnaryBallDFun where
  type DivTypeNoCN UnaryBallDFun UnaryBallDFun = UnaryBallDFun
  divideNoCN (UnaryBallDFun (a:ad:_)) (UnaryBallDFun (b:bd:_)) =
    UnaryBallDFun [a/b, (ad*b-a*bd)/(b^2)]
  divideNoCN _ _ = error "UnaryBallDFun: divideNoCN operand does not contain a derivative"
  type DivType UnaryBallDFun UnaryBallDFun = UnaryBallDFun
  divide (UnaryBallDFun (a:ad:_)) (UnaryBallDFun (b:bd:_)) =
    UnaryBallDFun [a/b, (ad*b-a*bd)/(b^2)]
  divide _ _ = error "UnaryBallDFun: divide operand does not contain a derivative"

instance CanPow UnaryBallDFun Integer where
  type PowTypeNoCN UnaryBallDFun Integer = UnaryBallDFun
  powNoCN (UnaryBallDFun (b:bd:_)) n =
    UnaryBallDFun [b^n, n*(b^(n-1))*bd]
  powNoCN _ _ = error "UnaryBallDFun: powNoCN operand does not contain a derivative"
  type PowType UnaryBallDFun Integer = UnaryBallDFun
  pow (UnaryBallDFun (b:bd:_)) n =
    UnaryBallDFun [b^n, n*(b^(n-1))*bd]
  pow _ _ = error "UnaryBallDFun: pow operand does not contain a derivative"

instance CanSinCos UnaryBallDFun where
  sin (UnaryBallDFun (a:ad:_)) =
    UnaryBallDFun [sin a, (cos a) * ad]
  sin _ = error "UnaryBallDFun: sin operand does not contain a derivative"
  cos (UnaryBallDFun (a:ad:_)) =
    UnaryBallDFun [cos a, -(sin a) * ad]
  cos _ = error "UnaryBallDFun: sin operand does not contain a derivative"

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| CauchyReal |]]
  (\ t -> [d|

    instance CanAddAsymmetric UnaryBallDFun $t where
      add (UnaryBallDFun (a:rest)) n = UnaryBallDFun (n+a:rest)
      add _ _ = error "UnaryBallDFun: add operand does not contain any value"
    instance CanAddAsymmetric $t UnaryBallDFun where
      type AddType $t UnaryBallDFun = UnaryBallDFun
      add n (UnaryBallDFun (a:rest)) = UnaryBallDFun (n+a:rest)
      add _ _ = error "UnaryBallDFun: add operand does not contain any value"

    instance CanSub UnaryBallDFun $t where
      sub (UnaryBallDFun (a:rest)) n = UnaryBallDFun (a-n:rest)
      sub _ _ = error "UnaryBallDFun: sub operand does not contain any value"
    instance CanSub $t UnaryBallDFun where
      type SubType $t UnaryBallDFun = UnaryBallDFun
      sub n (UnaryBallDFun (a:rest)) = UnaryBallDFun (n-a:rest)
      sub _ _ = error "UnaryBallDFun: sub operand does not contain any value"

    instance CanMulAsymmetric UnaryBallDFun $t where
      mul (UnaryBallDFun as) n = UnaryBallDFun (map (*n) as)
    instance CanMulAsymmetric $t UnaryBallDFun where
      type MulType $t UnaryBallDFun = UnaryBallDFun
      mul n (UnaryBallDFun as) = UnaryBallDFun (map (*n) as)

    instance CanDiv UnaryBallDFun $t where
      type DivTypeNoCN UnaryBallDFun $t = UnaryBallDFun
      divideNoCN (UnaryBallDFun as) n = UnaryBallDFun (map (/n) as)
      type DivType UnaryBallDFun $t = UnaryBallDFun
      divide (UnaryBallDFun as) n = UnaryBallDFun (map (/n) as)
    instance CanDiv $t UnaryBallDFun where
      type DivTypeNoCN $t UnaryBallDFun = UnaryBallDFun
      divideNoCN n (UnaryBallDFun (b:bd:_)) =
        UnaryBallDFun [n/b, (negate n*bd)/(b^!2)]
      divideNoCN _ _ = error "UnaryBallDFun: divideNoCN operand does not contain a derivative"
      type DivType $t UnaryBallDFun = UnaryBallDFun
      divide n (UnaryBallDFun (b:bd:_)) =
        UnaryBallDFun [n/b, (negate n*bd)/(b^!2)]
      divide _ _ = error "UnaryBallDFun: divide operand does not contain a derivative"
  |]))
