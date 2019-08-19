{-# LANGUAGE RebindableSyntax,
    PostfixOperators,
    ScopedTypeVariables,
    DeriveGeneric,
    GeneralizedNewtypeDeriving,
    TypeFamilies,
    TypeOperators,
    ConstraintKinds,
    DefaultSignatures,
    MultiParamTypeClasses,
    FlexibleContexts,
    FlexibleInstances,
    UndecidableInstances
  #-}
-- TODO: remove the above pragma

module AERN2.AnalyticMV.Type
(
  --   Analytic(..)
  -- , UnitInterval (..)
  -- , toUnitInterval
  -- , fromUnitInterval
  -- , lift1UI
  -- , ana_derivative
)
where

import MixedTypesNumPrelude

-- import qualified Data.Map as Map
import Data.Function.Memoize

import AERN2.MP.Ball
import AERN2.MP.Dyadic
import AERN2.RealFun.Operations
import AERN2.Real



data PowS =
  PowS
  {
    pows_x0 :: V CauchyReal
  , pows_k :: Integer
  , pows_A :: Integer
  , pows_terms :: (MultiIndex -> CauchyReal)
  }
  deriving Show

type V a = [a]

type MultiIndex = V Integer

_ode_f1 :: PowS
_ode_f1 = -- (y(x)-1)^2, around 0
  PowS {
    pows_x0 = [real 0],
    pows_k = 1,
    pows_A = 2,
    pows_terms = terms
  } 
  where
  terms [0] = real (1)
  terms [1] = real (-2)
  terms [2] = real 1
  terms _ = real 0

{-

Define sine using a 2-variable linear ODE.

Attempt 1:

y1' = y2
y2' = -y1
y1(0) = 0
y2(0) = 1 -- eek, we currently support only initial value 0

Attempt 2:

y1(t) = sin(t)
y2(t) = cos(t) - 1

y1' = y2+1
y2' = -y1
y1(0) = 0
y2(0) = 0

-}

_ode_sine_1 :: PowS
_ode_sine_1 = -- f(y1,y2) = y2
  PowS {
    pows_x0 = [real 0],
    pows_k = 1,
    pows_A = 1,
    pows_terms = terms
  } 
  where
  terms [0,1] = real 1
  terms [0,0] = real 1
  terms _ = real 0

_ode_sine_2 :: PowS
_ode_sine_2 = -- f(y1,y2) = -y1
  PowS {
    pows_x0 = [real 0],
    pows_k = 1,
    pows_A = 1,
    pows_terms = terms
  } 
  where
  terms [1,0] = real (-1)
  terms _ = real 0

_poly1_0 :: PowS
_poly1_0 = -- x^2-1, around 0
  PowS {
    pows_x0 = [real 0],
    pows_k = 1,
    pows_A = 1,
    pows_terms = terms
  } 
  where
  terms [0] = real (-1)
  terms [2] = real 1
  terms _ = real 0

_poly2_0 :: PowS
_poly2_0 = -- x^2-y^2, around 0
  PowS {
    pows_x0 = [real 0, real 0],
    pows_k = 1,
    pows_A = 1,
    pows_terms = terms
  } 
  where
  terms [2,0] = real 1
  terms [0,2] = real (-1)
  terms _ = real 0

_poly3_0 :: PowS
_poly3_0 = -- x^2-y^2+x*z^3, around 0
  PowS {
    pows_x0 = [real 0, real 0, real 0],
    pows_k = 1,
    pows_A = 1,
    pows_terms = terms
  } 
  where
  terms [2,0,0] = real 1
  terms [0,2,0] = real (-1)
  terms [1,0,3] = real (1)
  terms _ = real 0

-- usage: (apply _exp1_0 [real 0.5]) ? (bitsS 100)
_exp1_0 :: PowS
_exp1_0 =
  PowS {
    pows_x0 = [real 0],
    pows_k = 1,
    pows_A = 1,
    pows_terms = terms
  } 
  where
  terms = memoFix aux
    where
    aux :: (([Integer] -> CauchyReal) -> [Integer] -> CauchyReal)
    aux trms [0] = real 1
    aux trms [m1] = (1/!(real m1))*(trms [m1-1])
    aux _ _ = error "unary _exp1_0 used illegaly"

sum1 :: PowS -> CauchyReal -> CauchyReal
sum1 f x1 =
  realLim
    (\m ->  horn m (a_m m)) -- TODO: evaluate more cleverly
    (\m -> a * q^!(m + 1) /! (1 - q))
  where
  a = pows_A f
  k = pows_k f
  (x0_1:_) = pows_x0 f
  a_m m = pows_terms f [m]
  q = (abs x1) * k
  horn 0 y = y
  horn i y = horn (i - 1) (y * (x1 - x0_1) + (a_m (i - 1)))

sigma :: PowS -> CauchyReal -> PowS
sigma f x1 =
  f {
        pows_x0 = x0_rest, 
        pows_A = a,
        pows_terms = terms
    }
  where
  (x0_1:x0_rest) = pows_x0 f
  a = pows_A f
  k = pows_k f
  terms ms =
    sum1 f1 x1
    where
    f1 = 
      f { 
          pows_x0 = [x0_1], 
          pows_A = a * k ^! (sum ms),
          pows_terms = \[m0] -> pows_terms f (m0 : ms)
        }
    
{-- EVALUATION --}

instance CanApply PowS (V CauchyReal) where
  type ApplyType PowS (V CauchyReal) = CauchyReal
  apply f x = 
    case x of
      [] -> error "CanApply PowS does not support 0-dimensional PowS"
      [x1] -> sum1 f x1
      (x1 : xs) -> apply (sigma f x1) xs


realLim :: (Integer -> CauchyReal) -> (Integer -> CauchyReal) -> CauchyReal
realLim xe_n err_n =
  newCR "lim" [] makeQ
  where
  makeQ _me_src acc@(AccuracySG s _) = h 0
    where
    h k
      | kthOk =
        centreAsBall ((xe_n k) ? (acc + 1)) -- TODO: is this correct?
        + (mpBall (0, (dyadic 0.5)^!(fromAccuracy s)))
      | otherwise =
        h (k + 1)
      where
      kthError :: MPBall
      kthError = (err_n k) ? (acc + 2)
      kthOk :: Bool
      kthOk =
        (kthError <= 0.5^!((fromAccuracy s) + 1)) == Just True

{-- ADDITION --}

-- assuming that both power series use the same centre
instance CanAddAsymmetric PowS PowS where
  type AddType PowS PowS = PowS
  add f1 f2 = 
    PowS {
          pows_x0 = x0_1,
          pows_k = max k1 k2,
          pows_A = a1 + a2,
          pows_terms = memoize terms
      }
    where
    x0_1 = pows_x0 f1
    a1 = pows_A f1
    a2 = pows_A f2
    k1 = pows_k f1
    k2 = pows_k f2
    terms1 = pows_terms f1
    terms2 = pows_terms f2
    terms m = terms1 m + terms2 m

{-- SCALING --}

-- assuming that both power series use the same centre
instance CanMulAsymmetric Rational PowS where
  type MulType Rational PowS = PowS
  mul q1 f2 = 
    PowS {
          pows_x0 = x0,
          pows_k = k,
          pows_A = max 1 $ ceiling $ a*(abs q1),
          pows_terms = memoize terms
      }
    where
    x0 = pows_x0 f2
    a = pows_A f2
    k = pows_k f2
    terms2 = pows_terms f2
    terms m = q1 * terms2 m

instance CanMulAsymmetric Integer PowS where
  type MulType Integer PowS = PowS
  mul n1 f2 = mul (rational n1) f2 

{-- MLUTIPLICATION --}

-- assuming that both power series use the same centre
instance CanMulAsymmetric PowS PowS where
  type MulType PowS PowS = PowS
  mul f1 f2 = 
    PowS {
          pows_x0 = x0_1,
          pows_k = max k1 k2,
          pows_A = a1 * a2,
          pows_terms = memoize terms
      }
    where
    x0_1 = pows_x0 f1
    a1 = pows_A f1
    a2 = pows_A f2
    k1 = pows_k f1
    k2 = pows_k f2
    terms1 = pows_terms f1
    terms2 = pows_terms f2
    terms = aux [] []
      where
      aux m1 m2 [] = terms1 m1 * terms2 m2
      aux m1 m2 (m:ms) =
        sum $ [ aux (m1 ++ [i]) (m2 ++ [m-i]) ms | i <- [0..m] ]
      

{-- DERIVATIVE --}

deriv_powS :: PowS -> Integer -> PowS
deriv_powS f j =
  PowS {
        pows_x0 = x0,
        pows_k = 2*k,
        pows_A = a*k,
        pows_terms = memoize terms
    }
  where
  x0 = pows_x0 f
  a = pows_A f
  k = pows_k f
  terms m = 
    (mj+1) * (pows_terms f m')
    where
    mj = m !! (j-1)
    m' = take (j-1) m ++ [mj+1] ++ drop j m

{-- ODE one step --}

ode_step_powS :: V PowS -> Rational -> V CauchyReal -> V PowS 
ode_step_powS f t0 y0 = map step_i [1..d]
  -- TODO: currently works only with y0 = x0 (where x0 is the centre of f)  
  where
  d = length f
  step_i i =
    PowS {
      pows_x0 = [real t0],
      pows_k = a*k,
      pows_A = 1,
      pows_terms = terms
    } 
    where
    x0 = pows_x0 (head f) -- all components of the field must have the same centre
    a = maximum $ map pows_A f
    k = maximum $ map pows_k f
    terms [m] =
      fim m `apply` y0
    fi0 = 
      PowS {
        pows_x0 = map (const (real 0)) y0,
        pows_k = 1,
        pows_A = 1,
        pows_terms = terms_i
      } 
      where
      terms_i mx 
        | mx == mx_i = real 1
        | otherwise = real 0
      mx_i = take (i-1) (repeat 0) ++ [1] ++ (take (d-i) (repeat 0))

    fimP1 m fim = 
      (1/!(m+1))*(foldl1 (+) [ (deriv_powS fim j) * fj | (j, fj) <- zip [1..d] f])

    fim' _ 0 = fi0
    fim' fim'' m = fimP1 (m-1) (fim'' (m-1))
    fim = memoFix fim'








