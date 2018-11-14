{-# LANGUAGE CPP #-}
-- #define DEBUG

module AERN2.Limit where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif

import MixedTypesNumPrelude

import Control.CollectErrors

import AERN2.QA.Protocol
import AERN2.Real

---------
-- limit
---------

class HasLimits ix s where
  type LimitType ix s
  limit :: (ix -> s) -> LimitType ix s

instance HasLimits Rational CauchyReal where
  type LimitType Rational CauchyReal = CauchyReal
  limit s = newCR "limit" [] makeQ
    where
    makeQ (me, _src) ac@(AccuracySG acS _acG) =
      updateRadius (+ (errorBound e)) $ (s e ?<- me) (ac + 1)
      where
      e = 0.5^!(fromAccuracy acS + 1)

instance HasLimits Rational CauchyRealCN where
  type LimitType Rational CauchyRealCN = CauchyRealCN
  limit s = newCRCN "limit" [] makeQ
    where
    makeQ (me, _src) ac@(AccuracySG acS _acG) =
      lift1CE (updateRadius (+ (errorBound e))) $ (s e ?<- me) (ac + 1)
      where
      e = 0.5^!(fromAccuracy acS + 1)

instance HasLimits Rational (CauchyReal -> CauchyRealCN) where
  type LimitType Rational (CauchyReal -> CauchyRealCN) = (CauchyReal -> CauchyRealCN)
  limit fs x = newCRCN "limit" [AnyProtocolQA x] makeQ
    where
    makeQ (me, _src) ac@(AccuracySG acS _acG) =
      maybeTrace ("limit (CauchyReal -> CauchyRealCN): ac = " ++ show ac ) $
      lift1CE (updateRadius (+ (errorBound e))) $ (fx ?<- me) (ac + 1)
      where
      fx = fs e x
      e = 0.5^!(fromAccuracy acS + 1)

{-
  The following strategies are inspired by
  Mueller: The iRRAM: Exact Arithmetic in C++, Section 10.1
  https://link.springer.com/chapter/10.1007/3-540-45335-0_14
-}

instance HasLimits Rational (MPBall -> CN MPBall) where
  type LimitType Rational (MPBall -> CN MPBall) = (MPBall -> CN MPBall)
  limit fs x =
    maybeTrace ("limit (MPBall -> CN MPBall): x = " ++ show x) $
    maybeTrace ("limit (MPBall -> CN MPBall): xPNext = " ++ show xPNext) $
    maybeTrace ("limit (MPBall -> CN MPBall): accuracies = " ++ show accuracies) $
    tryAccuracies accuracies
    where
    acX = getFiniteAccuracy x
    accuracies = aux (fromAccuracy acX)
      where
      aux a
        | a >= 4 = bits a : aux ((100 * a) `div` 105)
        | otherwise = [bits a]
    xPNext = setPrecision (increaseP $ getPrecision x) x
    increaseP p =
      prec $ (integer p) + 10
      -- prec $ ((101 * (integer p)) `div` 100) + 1

    tryAccuracies [] =
      noValueNumErrorPotentialECN (Nothing :: Maybe MPBall) $
        NumError "limit (MPBall -> CN MPBall) failed"
    tryAccuracies (ac : rest) =
      let result = withAccuracy ac in
      case getErrorsCN result of
        [] -> result
        _ -> tryAccuracies rest

    withAccuracy ac =
      maybeTrace ("limit (MPBall -> CN MPBall): withAccuracy: ac = " ++ show ac) $
      lift1CE (updateRadius (+ (errorBound e))) (fs e xPNext)
      where
      e = 0.5^!(fromAccuracy ac)

data WithLipschitz f = WithLipschitz f f

instance HasLimits Rational (WithLipschitz (MPBall -> CN MPBall)) where
  type LimitType Rational (WithLipschitz (MPBall -> CN MPBall)) = (MPBall -> CN MPBall)
  limit ffs xPre =
    maybeTrace ("limit (MPBall -> CN MPBall): x = " ++ show x) $
    maybeTrace ("limit (MPBall -> CN MPBall): xC = " ++ show xC) $
    maybeTrace ("limit (MPBall -> CN MPBall): xE = " ++ show xE) $
    maybeTrace ("limit (MPBall -> CN MPBall): accuracies = " ++ show accuracies) $
    tryAccuracies accuracies
    where
    acX = getFiniteAccuracy x
    accuracies = aux (fromAccuracy acX)
      where
      aux a
        | a >= 4 = bits a : aux ((100 * a) `div` 105)
        | otherwise = [bits a]
    x = increasePrec xPre
      where
      increasePrec z = setPrecision (inc $ getPrecision xPre) z
      inc p =
          prec $ (integer p) + 10
          -- prec $ ((101 * (integer p)) `div` 100) + 1
    xC = centreAsBall x
    xE = radius x

    tryAccuracies [] =
      noValueNumErrorPotentialECN (Nothing :: Maybe MPBall) $
        NumError "limit (MPBall -> CN MPBall) failed"
    tryAccuracies (ac : rest) =
      let result = withAccuracy ac in
      case getErrorsCN result of
        [] -> result
        _ -> tryAccuracies rest

    withAccuracy ac =
      maybeTrace ("limit (MPBall -> CN MPBall): withAccuracy: ac = " ++ show ac) $
      lift1CE (updateRadius (+ (errorBound e))) fx
      where
      e = 0.5^!(fromAccuracy ac)
      WithLipschitz f f' = ffs e
      fxC_CN = f xC
      f'x_CN = f' x
      fx :: CN MPBall
      fx =
          case (ensureNoCN fxC_CN, ensureNoCN f'x_CN) of
            ((Just fxC, []), (Just f'x, [])) ->
              cn $ updateRadius (+ (xE * (errorBound f'x))) fxC
            _ ->
              f x -- fallback

instance HasLimits Rational (WithLipschitz (CauchyReal -> CauchyRealCN)) where
  type LimitType Rational (WithLipschitz (CauchyReal -> CauchyRealCN)) = (CauchyReal -> CauchyRealCN)
  limit ffs x = limit fs x
    where
    fs e = f
      where
      WithLipschitz f _ = ffs e
