{-|
    Module      :  AERN2.Poly.Cheb.Eval
    Description :  Evaluation and range
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Evaluation and range
-}

module AERN2.Poly.Cheb.Eval
(
  evalDirect, evalLip, evalDf, evalDI, reduceToEvalDirectAccuracy, evalDirectWithAccuracy
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

-- import Test.Hspec
-- import Test.QuickCheck

import Data.Maybe

import AERN2.MP.ErrorBound
import AERN2.MP.Ball
import AERN2.MP.Dyadic

-- import AERN2.Real

import AERN2.Interval
import AERN2.RealFun.Operations
import AERN2.RealFun.UnaryFun
-- import AERN2.RealFun.UnaryDFun

import AERN2.Poly.Basics
import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Derivative
-- import AERN2.Poly.Conversion
-- import AERN2.Poly.Power (PowPoly)
import qualified AERN2.Poly.Power as Pow


import AERN2.Poly.Conversion -- TODO remove

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
-- shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace
    | shouldTrace = trace
    | otherwise = const id

{- evaluation -}

instance
  (CanAddSubMulBy MPBall c, Ring c) =>
  CanApply (ChPoly c) MPBall where
  type ApplyType (ChPoly c) MPBall = MPBall
  apply = evalDirect

-- instance
--   (CanAddSubMulBy MPBall c, Ring c) =>
--   CanApply (ChPoly c) Dyadic where
--   type ApplyType (ChPoly c) Dyadic = (CauchyReal, ErrorBound)
--   apply cp x =
--     seqByPrecision2CauchyRealA "apply" $ \ pr ->
--       apply cp $ setPrecision pr $ mpBall x

evalDirect ::
  (Ring t, CanAddSubMulDivBy t Dyadic, CanDivBy t Integer,
   CanAddSubMulBy t c, Ring c)
  =>
  (ChPoly c) -> t -> t
evalDirect (ChPoly dom (Poly terms)) (xInDom :: t) =
    (b0 - b2)/2
    where
    x = fromDomToUnitInterval dom xInDom
    n = terms_degree terms
    (b0:_:b2:_) = bs
    bs :: [t]
    bs = reverse $ aux n z z
    z = convertExactly 0
    aux k bKp2 bKp1
        | k == 0 = [bKp2, bKp1, bK]
        | otherwise = bKp2 : aux (k - 1) bKp1 bK
        where
        bK = (a k) + 2 * x * bKp1 - bKp2
    a k = terms_lookupCoeffDoubleConstTerm terms k

evalDirectWithAccuracy :: Accuracy -> ChPoly MPBall -> MPBall -> MPBall
evalDirectWithAccuracy bts f x =
  if getAccuracy x < Exact then
    error $ "evalDirectWithAccuracy: trying to evaluate on non-exact argument."
             ++ "\n f: "++(show f)++"\nx: "++(show x)
  else
    aux (getPrecision f) (getPrecision x)
  where
  aux p q =
    let
      try = evalDirect (setPrecision p f) (setPrecision p x)
    in
      if getAccuracy try >= min bts (getAccuracy f) then
        try
      else
        aux (p + q) p

evalLip :: ChPoly MPBall -> MPBall -> MPBall -> MPBall
evalLip f l x =
  --trace ("centre: "++ (show $ evalDirect f (centreAsBall x))) $
  --trace ("lipschitz: "++ (show $ l)) $
  --trace ("error: "++ (show $ err)) $
  (evalDirect f (centreAsBall x)) + (fromEndpoints (-err) err :: MPBall)
  where
  err = l* dyadic (ball_error x)*0.5

evalDf :: ChPoly MPBall -> ChPoly MPBall -> MPBall -> MPBall
evalDf f f' x =
  let
    ChPoly _ ts = f'
  in
  {-trace ("evaluating on "++(show x)) $
  trace ("derivative accuracy: "++(show $ getAccuracy $ f')) $
  trace ("derivative precision: "++(show $ getPrecision $ f')) $
  trace ("derivative terms: "++(show $ ts)) $
  trace ("x accuracy: "++(show $ getAccuracy x)) $
  trace ("x precision: "++(show $ getPrecision x)) $
  trace ("terms: "++(show $ chPoly_terms f')) $
  trace ("terms sum: "++(show $ foldl (+) (mpBall 0) (map snd $ (terms_toList . chPoly_terms $ f'))))
  trace ("eval in power basis: "++(show $ Pow.evalDirect (cheb2Power $ chPoly_poly f') x))-}
  evalLip f (abs $ evalDirect f' x) x

reduceToEvalDirectAccuracy :: ChPoly MPBall -> Accuracy -> ChPoly MPBall
reduceToEvalDirectAccuracy f ac =
  aux 5 NoInformation Nothing
  where
  domBall = setPrecision (getPrecision f) $ mpBall $ chPoly_dom f
  aux d oldAccuracy oldTry =
    let
      tryDegree   = reduceDegree d f
      tryAccuracy = getAccuracy (evalDirect tryDegree domBall)
    in
      if tryAccuracy >= ac then
        tryDegree
      else if tryAccuracy <= oldAccuracy then
        fromJust oldTry
      else
        aux (d + 5) tryAccuracy (Just tryDegree)

evalDI :: ChPoly MPBall -> MPBall -> MPBall
evalDI f = evalDf f ((derivative . centre) f)

{-evalDfAccurately :: ChPoly MPBall -> ChPoly MPBall -> MPBall -> MPBall
evalDfAccurately f f' x = (aux 100 50) + (fromEndpoints (-err) err :: MPBall)
  where
  l = abs $ evalDirect f' x
  err = l* dyadic (ball_error x)*0.5
  aux n m =
    let
      fcx = evalDirect (setPrecision (prec n) f) (setPrecision (prec n) $ centreAsBall x)
    in
      if getAccuracy fcx >= getAccuracy f - 1 then
        fcx
      else
        aux (n + m) n-}

{- range -}

instance CanApplyApprox (ChPoly MPBall) DyadicInterval where
  type ApplyApproxType (ChPoly MPBall) DyadicInterval = DyadicInterval
  applyApprox p di =
    dyadicInterval (fromEndpoints lB uB :: MPBall)
    where
    (Interval lB uB) = sampledRange di 5 p :: Interval MPBall MPBall

-- TODO: move sampledRange to a module not specific to ChPoly
sampledRange ::
  (CanApply f t, ApplyType f t ~ t,
   CanMinMaxSameType t, ConvertibleExactly Dyadic t, Show t)
  =>
  DyadicInterval -> Integer -> f -> Interval t t
sampledRange (Interval l r) depth f =
    maybeTrace
    ( "sampledRange:"
    ++ "\n samplePointsT = " ++ (show samplePointsT)
    ++ "\n samples = " ++ show samples
    ) $
    Interval minValue maxValue
    where
    minValue = foldl1 min samples
    maxValue = foldl1 max samples
    samples = map (apply f) samplePointsT
    samplePointsT = map convertExactly samplePoints
    _ = minValue : samplePointsT
    samplePoints :: [Dyadic]
    samplePoints = [(l*i + r*(size - i))*(1/size) | i <- [0..size]]
    size = 2^depth

-- instance CanApply (ChPoly MPBall) DyadicInterval where
--   type ApplyType (ChPoly MPBall) DyadicInterval = (Interval CauchyReal CauchyReal, ErrorBound)
--   apply = rangeViaUnaryFun
--   -- apply = rangeViaRoots

-- rangeViaUnaryFun :: (ChPoly MPBall) -> DyadicInterval -> (Interval CauchyReal CauchyReal, ErrorBound)
-- rangeViaUnaryFun p di = (apply f di, e)
--   where
--   f :: UnaryFun
--   (f, e) = convertExactly p
--
-- rangeViaUnaryDFun :: (ChPoly MPBall) -> DyadicInterval -> (Interval CauchyReal CauchyReal, ErrorBound)
-- rangeViaUnaryDFun p@(ChPoly dom poly) (Interval l r) =
--   (apply ff (Interval lU rU), e)
--   where
--   lU = fromDomToUnitInterval dom (real l)
--   rU = fromDomToUnitInterval dom (real r)
--   ff = UnaryDFun [f,f']
--   pU' = Pow.derivative $ cheb2Power poly
--   f' = UnaryFun dom  (fmap (Pow.evalMBI pU' . fromDomToUnitInterval dom))
--   (f, e) = convertExactly p


-- rangeViaRoots :: (ChPoly MPBall) -> DyadicInterval -> (Interval CauchyReal CauchyReal, ErrorBound)

instance ConvertibleExactly (ChPoly MPBall) (UnaryFun, ErrorBound) where
  safeConvertExactly cp@(ChPoly dom _p) = Right (UnaryFun dom eval, e)
    where
    e = radius cp
    cpExact = centreAsBall cp
    eval = fmap $ evalDirect cpExact
