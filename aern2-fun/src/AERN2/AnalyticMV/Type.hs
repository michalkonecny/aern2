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
-- (
--     Analytic(..)
--   , UnitInterval (..)
--   , toUnitInterval
--   , fromUnitInterval
--   , lift1UI
--   , ana_derivative
-- )
where

import MixedTypesNumPrelude

import Debug.Trace

import Text.Printf

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Function.Memoize

import AERN2.MP.Ball
-- import AERN2.MP.Dyadic
import AERN2.RealFun.Operations
-- import AERN2.Real


data PowS =
  PowS
  {
    pows_x0 :: V MPBall
  , pows_k :: Integer
  , pows_A :: Integer
  , pows_terms :: (MultiIndex -> MPBall)
  }
  deriving Show

type V a = [a]

type MultiIndex = V Integer

debug_PowS :: String -> PowS -> PowS
debug_PowS name pw =
  pw { pows_terms = terms }
  where
  terms m = (trace msg . pows_terms pw) m
    where
    msg = "term of " ++ name ++ ": " ++ show m

{- POLYNOMIAL to PowS conversion -}

newtype P = P (Map.Map MultiIndex MPBall)

instance Show P where
  show (P p) =
    List.intercalate " + " $ map showTerm $ Map.toAscList p
    where
    showTerm (m, c) = show (c) ++ concat (map showV $ zip [1..] m)
    showV (_i,0) = ""
    showV (i,1) = printf "x%d" i
    showV (i,n) = printf "x%d^%d" i n


{- P arithmetic  -}

x_d_i_n :: Integer -> Integer -> Integer -> P
x_d_i_n d i n = P $ Map.singleton (m_d_i_n d i n) (mpBall 1)

constP :: Integer -> P
constP d = P $ Map.singleton (replicate d 0) (mpBall 1)

vars :: Integer -> [P]
vars d = 
  [ x_d_i_n d i 1 | i<-[1..d]]

_x1,_x2 :: P
[_x1, _x2] = vars 2

m_d_i_n :: Integer -> Integer -> Integer -> MultiIndex
m_d_i_n d i n = 
  take (i-1) (repeat 0) ++ [n] ++ (take (d-i) (repeat 0))

instance CanAddAsymmetric P P where
  add (P p1) (P p2) = P $ Map.unionWith (+) p1 p2
instance CanNeg P where
  negate (P p) = P (Map.map negate p)
instance CanAddAsymmetric P MPBall where
  type AddType P MPBall = P
  add (P p1) r2 = P $ Map.insertWith (+) m0000 r2 p1
    where
    d = length (fst $ Map.findMin p1)
    m0000 = replicate d 0
instance CanMulAsymmetric MPBall P where
  type MulType MPBall P = P
  mul r1 (P p2) = P $ Map.map (* r1) p2
instance CanMulAsymmetric Rational P where
  type MulType Rational P = P
  mul r1 (P p2) = P $ Map.map (* r1) p2
instance CanMulAsymmetric Integer P where
  type MulType Integer P = P
  mul r1 (P p2) = P $ Map.map (* r1) p2
instance CanMulAsymmetric P P where
  mul (P p1) (P p2) = foldl1 (+) pairs 
    where
    pairs = 
      [
        P (Map.fromList [(zipWith (+) m1 m2, c1 * c2)]) 
      | 
        (m1,c1) <- (Map.toList p1) ,  (m2,c2) <- (Map.toList p2)
      ]

{- P translation to centre x0  -}

translate_P :: P -> V MPBall -> P
translate_P (P p) x0 =
  foldl1 (+) $ map evalT $ Map.toList p
  where
  d = length x0
  evalT (m,c) 
    | null varVals = c * (constP d)
    | otherwise = c * (foldl1 (*) varVals)
    where
    varVals = map evalV $ filter (\(_,n,_) -> n > 0) $ zip3 [1..] m x0
  evalV (i,n,x0i) =
    foldl1 (*) $ replicate n $ xi + x0i
    where
    xi = x_d_i_n d i 1

type Analytic = V MPBall -> PowS

poly_Analytic :: P -> Analytic
poly_Analytic pp x0 =
  PowS {
    pows_x0 = x0,
    pows_k = 1,
    pows_A = snd $ integerBounds $ (maximum (map abs $ Map.elems tp)),
    pows_terms = terms
  }
  where
  (P tp) = translate_P pp x0
  terms m =
    case Map.lookup m tp of
      Just c -> c
      _ -> mpBall 0

{- Example Analytic functions -}

{-

Define sine using a 2-variable linear ODE.

y1' = y2
y2' = -y1
y1(0) = 0
y2(0) = 1

(apply (head $ ode_step_Analytic [_ode_sine_1, _ode_sine_2] (rational 0) [mpBall 0, mpBall 1]) [mpBall 0.1]) ? (bits S 10)

-}
_ode_sine_1 :: Analytic
_ode_sine_1 =
  poly_Analytic y2
  -- debug_PowS "_ode_sine_1" . poly_Analytic y2
  where
  [_y1, y2] = vars 2
  
_ode_sine_2 :: Analytic
_ode_sine_2 =
  poly_Analytic (-y1)
  where
  [y1, _y2] = vars 2

_solve_sine_1 :: (CanBeRational t) => Precision -> t -> MPBall
_solve_sine_1 p t = 
  (head $ fst $ ode_Analytic [_ode_sine_1, _ode_sine_2] (rational 0) [mpBallP p 0, mpBallP p 1] (rational t))

-- usage: (apply _exp1_0 [mpBall 0.5]) ? (bitsS 100)
_exp1_0 :: PowS
_exp1_0 =
  PowS {
    pows_x0 = [mpBall 0],
    pows_k = 1,
    pows_A = 1,
    pows_terms = terms
  } 
  where
  terms = memoFix aux
    where
    aux :: (([Integer] -> MPBall) -> [Integer] -> MPBall)
    aux _trms [0] = mpBall 1
    aux trms [m1] = (1/!(mpBall m1))*(trms [m1-1])
    aux _ _ = error "unary _exp1_0 used illegaly"

{- EVALUATION -}

sum1 :: PowS -> MPBall -> MPBall
sum1 f x1 =
  updateRadius (+ (errorBound r)) c
  where
  p = getPrecision x1
  a = pows_A f
  k = pows_k f
  (x0_1:_) = pows_x0 f
  a_m m' = pows_terms f [m']
  q = (abs (x1-x0_1)) * k
  r_prelim = mpBall $ radius x1
  r 
    | r_prelim !>! 0 = r_prelim
    | otherwise = (mpBallP p 0.5)^!(integer $ getPrecision x1)
  m 
    | r ?==? 0 || q ?==? 0 = 0
    | otherwise =
        max 0 $ (snd $ integerBounds $ (~!) $ (log ((1-q)*r/a))/(log q)) - 1
  c = horn m (a_m m)
  horn 0 y = y
  horn i y = horn (i - 1) (y * (x1 - x0_1) + (a_m (i - 1)))

-- realLim :: (Integer -> MPBall) -> (Integer -> MPBall) -> MPBall
-- realLim xe_n err_n =
--   newCR "lim" [] makeQ
--   where
--   makeQ _me_src acc@(AccuracySG s _) = h 0
--     where
--     h k
--       | kthOk =
--         centreAsBall ((xe_n k) ? (acc + 1)) -- TODO: is this correct?
--         + (mpBall (0, (dyadic 0.5)^!(fromAccuracy s)))
--       | otherwise =
--         h (k + 1)
--       where
--       kthError :: MPBall
--       kthError = (err_n k) ? (acc + 2)
--       kthOk :: Bool
--       kthOk =
--         (kthError <= 0.5^!((fromAccuracy s) + 1)) == Just True


sigma :: PowS -> MPBall -> PowS
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

instance CanApply PowS (V MPBall) where
  type ApplyType PowS (V MPBall) = MPBall
  apply f x = 
    case x of
      [] -> error "CanApply PowS does not support 0-dimensional PowS"
      [x1] -> sum1 f x1
      (x1 : xs) -> apply (sigma f x1) xs


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

ode_step_powS :: V PowS -> Rational -> V MPBall -> V PowS 
ode_step_powS f t0 y0 = map step_i [1..d]
  -- works only with y0 = x0 (where x0 is the centre of f)  
  where
  d = length f
  m0000 = replicate d 0
  p = getPrecision (head y0)
  step_i i =
    PowS {
      pows_x0 = [mpBallP p t0],
      pows_k = a*k,
      pows_A = 1,
      pows_terms = terms
    } 
    where
    -- x0 = pows_x0 (head f) -- all components of the field must have the same centre
    a = maximum $ map pows_A f
    k = maximum $ map pows_k f
    terms [m] = fim m `apply` y0
    terms _ = error "bad terms"
    fi0 = 
      PowS {
        pows_x0 = y0,
        pows_k = 1,
        pows_A = 1,
        pows_terms = terms_i
      } 
      where
      terms_i mx 
        | mx == mx_i = mpBall 1
        | mx == m0000 = y0 !! (i-1)
        | otherwise = mpBall 0
      mx_i = m_d_i_n d i 1

    fimP1 m fim'' = 
      (1/!(m+1))*(foldl1 (+) [ (deriv_powS fim'' j) * fj | (j, fj) <- zip [1..d] f])

    fim' _ 0 = fi0
    fim' fim'' m = fimP1 (m-1) (fim'' (m-1))
    fim = memoFix fim'


{- Many steps ODE solving (polynomial only for now) -}

ode_step_Analytic :: V Analytic -> Rational -> V MPBall -> V PowS
ode_step_Analytic fA t0 y0 = ode_step_powS fPowS t0 y0
  where
  fPowS = map ($ y0) fA

ode_Analytic :: V Analytic -> Rational -> V MPBall -> Rational -> (V MPBall, [(Rational, V MPBall, V PowS)])
ode_Analytic fA t00 y00 tE = aux [] t00 y00
  where
  p = getPrecision $ head y00
  aux prevSegs t0 y0 
    | tE <= t1 = (map (flip apply [mpBallP p tE]) seg, reverse $ (t0,y0,seg) : prevSegs)
    | otherwise = aux ((t0,y0,seg):prevSegs) t1 y1
    where
    seg = ode_step_Analytic fA t0 y0
    k = maximum $ map pows_k seg
    h = 1/!(2*k)
    t1 = t0 + h
    y1 = map (flip apply [mpBallP p t1]) seg
  
