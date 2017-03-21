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
  evalDirect, evalLip, evalDf, evalLDf, evalDI, reduceToEvalDirectAccuracy, evalDirectWithAccuracy
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

-- import Test.Hspec
-- import Test.QuickCheck

import Data.Maybe

import Numeric.CatchingExceptions

import AERN2.MP.ErrorBound
import AERN2.MP.Ball
import AERN2.MP.Dyadic

-- import AERN2.Real

import AERN2.Interval
import AERN2.RealFun.Operations
import AERN2.RealFun.UnaryBallFun
-- import AERN2.RealFun.UnaryBallDFun

import AERN2.Poly.Basics
import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Derivative

{- evaluation -}

instance
  (CanAddSubMulBy MPBall c, Ring c, c~MPBall) =>
  CanApply (ChPoly c) MPBall where
  type ApplyType (ChPoly c) MPBall = MPBall
  apply p x =
    case getAccuracy x of
      Exact -> evalDirect p x
      _ -> evalDI p x

instance
  CanApply (ChPoly MPBall) (CatchingNumExceptions MPBall) where
  type ApplyType (ChPoly MPBall) (CatchingNumExceptions MPBall) = CatchingNumExceptions MPBall
  apply p x =
    -- TODO: check x is in the domain
    apply p <$> x

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
evalDirect (ChPoly dom (Poly terms) _) (xInDom :: t) =
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
  {-let
    ChPoly _ ts _ = f'
  in
  trace ("evaluating on "++(show x)) $
  trace ("derivative accuracy: "++(show $ getAccuracy $ f')) $
  trace ("derivative precision: "++(show $ getPrecision $ f')) $
  trace ("derivative terms: "++(show $ ts)) $
  trace ("x accuracy: "++(show $ getAccuracy x)) $
  trace ("x precision: "++(show $ getPrecision x)) $
  trace ("terms: "++(show $ chPoly_terms f')) $
  trace ("terms sum: "++(show $ foldl (+) (mpBall 0) (map snd $ (terms_toList . chPoly_terms $ f'))))
  trace ("eval in power basis: "++(show $ Pow.evalDirect (cheb2Power $ chPoly_poly f') x))-}
  evalLip f (abs $ evalDirect f' x) x

evalLDf :: ChPoly MPBall -> ChPoly MPBall -> MPBall -> MPBall
evalLDf f f' x =
  case chPoly_maybeLip f of
    Nothing ->
      evalLip f (abs $ evalDirect f' x) x
    Just lip ->
      evalLip f lip x

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
evalDI f = evalLDf f ((derivative . centre) f)

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

-- instance CanApply (ChPoly MPBall) DyadicInterval where
--   type ApplyType (ChPoly MPBall) DyadicInterval = (Interval CauchyReal CauchyReal, ErrorBound)
--   apply = rangeViaUnaryBallFun
--   -- apply = rangeViaRoots

-- rangeViaUnaryBallFun :: (ChPoly MPBall) -> DyadicInterval -> (Interval CauchyReal CauchyReal, ErrorBound)
-- rangeViaUnaryBallFun p di = (apply f di, e)
--   where
--   f :: UnaryBallFun
--   (f, e) = convertExactly p
--
-- rangeViaUnaryBallDFun :: (ChPoly MPBall) -> DyadicInterval -> (Interval CauchyReal CauchyReal, ErrorBound)
-- rangeViaUnaryBallDFun p@(ChPoly dom poly) (Interval l r) =
--   (apply ff (Interval lU rU), e)
--   where
--   lU = fromDomToUnitInterval dom (real l)
--   rU = fromDomToUnitInterval dom (real r)
--   ff = UnaryBallDFun [f,f']
--   pU' = Pow.derivative $ cheb2Power poly
--   f' = UnaryBallFun dom  (fmap (Pow.evalMBI pU' . fromDomToUnitInterval dom))
--   (f, e) = convertExactly p


-- rangeViaRoots :: (ChPoly MPBall) -> DyadicInterval -> (Interval CauchyReal CauchyReal, ErrorBound)

instance ConvertibleExactly (ChPoly MPBall) (UnaryBallFun, ErrorBound) where
  safeConvertExactly cp@(ChPoly dom _p _) = Right (UnaryBallFun dom eval, e)
    where
    e = radius cp
    cpExact = centreAsBall cp
    eval bE = fmap (evalDirect cpExact) bE
