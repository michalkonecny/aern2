{-# LANGUAGE CPP #-}
-- #define DEBUG

{-# LANGUAGE PartialTypeSignatures #-}
module AERN2.Real.Limit where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif

import MixedTypesNumPrelude

import Numeric.CollectErrors ( CN )
import qualified Numeric.CollectErrors as CN

-- import Math.NumberTheory.Logarithms (integerLog2)

import AERN2.Real.Type
import AERN2.MP

---------
-- limit
---------

class HasLimits ix s where
  type LimitType ix s
  limit :: (ix -> s) -> LimitType ix s

instance HasLimits Rational CReal where
  type LimitType Rational CReal = CReal
  limit s = crealFromPrecFunction withPrec
    where
    withPrec p = ((s epsilon) ? p) +- epsilon
      where
      epsilon = 0.5 ^ (integer p)
    
instance HasLimits Integer CReal where
  type LimitType Integer CReal = CReal
  limit s = crealFromPrecFunction withPrec
    where
    withPrec p = ((s (integer p)) ? p) +- epsilon
      where
      epsilon = 0.5 ^ (integer p)

instance HasLimits Int CReal where
  type LimitType Int CReal = CReal
  limit s = limit (s . c)
    where
    c :: Integer -> Int
    c = int

instance HasLimits Rational (CReal -> CReal) where
  type LimitType Rational (CReal -> CReal) = (CReal -> CReal)
  limit fs x = crealFromPrecFunction withPrec
    where
    withPrec p = ((fs epsilon x) ? p) +- epsilon
      where
      epsilon = 0.5 ^ (integer p)

{-
  The following strategies are inspired by
  Mueller: The iRRAM: Exact Arithmetic in C++, Section 10.1
  https://link.springer.com/chapter/10.1007/3-540-45335-0_14
-}

instance HasLimits Rational (CN MPBall -> CN MPBall) where
  type LimitType Rational (CN MPBall -> CN MPBall) = (CN MPBall -> CN MPBall)
  limit = limitMPBall (\ac -> 0.5^(fromAccuracy ac))

instance HasLimits Integer (CN MPBall -> CN MPBall) where
  type LimitType Integer (CN MPBall -> CN MPBall) = (CN MPBall -> CN MPBall)
  limit = limitMPBall (\ac -> (fromAccuracy ac))
  -- limit s = limit (s . getN)
  --   where
  --   -- q > 0 => 0 < 0.5^(getN q) <= q
  --   getN :: Rational -> Integer
  --   getN r = integer $ integerLog2 (max 1 $ (ceiling $ 1/r) - 1) + 1

instance HasLimits Int (CN MPBall -> CN MPBall) where
  type LimitType Int (CN MPBall -> CN MPBall) = (CN MPBall -> CN MPBall)
  limit s = limit (s . c)
    where
    c :: Integer -> Int
    c = int

  
limitMPBall :: (Accuracy -> ix) -> (ix -> (CN MPBall -> CN MPBall)) -> (CN MPBall -> CN MPBall)
limitMPBall ac2ix fs x =
  maybeTrace ("limit (CN MPBall -> CN MPBall): x = " ++ show x) $
  maybeTrace ("limit (CN MPBall -> CN MPBall): xPNext = " ++ show xPNext) $
  maybeTrace ("limit (CN MPBall -> CN MPBall): accuracies = " ++ show accuracies) $
  tryAccuracies accuracies
  where
  acX = getFiniteAccuracy x
  accuracies = aux (fromAccuracy acX)
    where
    aux a
      | a >= 4 = bits a : aux ((100 * a) `divI` 105)
      | otherwise = [bits a]
  xPNext = setPrecision (increaseP $ getPrecision x) x
  increaseP p =
    prec $ (integer p) + 10
    -- prec $ ((101 * (integer p)) `div` 100) + 1

  tryAccuracies [] =
    CN.noValueNumErrorPotential $ CN.NumError "limit (CN MPBall -> CN MPBall) failed"
  tryAccuracies (ac : rest) =
    let result = withAccuracy ac in
    case CN.hasError result of
      True -> result
      _ -> tryAccuracies rest

  withAccuracy ac =
    maybeTrace ("limit (CN MPBall -> CN MPBall): withAccuracy: ac = " ++ show ac) $
    (fs ix xPNext) +- e
    where
    ix = ac2ix ac
    e = 0.5^(fromAccuracy ac)

-- data WithLipschitz f = WithLipschitz f f

-- instance HasLimits Rational (WithLipschitz (MPBall -> CN MPBall)) where
--   type LimitType Rational (WithLipschitz (MPBall -> CN MPBall)) = (MPBall -> CN MPBall)
--   limit ffs xPre =
--     maybeTrace ("limit (MPBall -> CN MPBall): x = " ++ show x) $
--     maybeTrace ("limit (MPBall -> CN MPBall): xC = " ++ show xC) $
--     maybeTrace ("limit (MPBall -> CN MPBall): xE = " ++ show xE) $
--     maybeTrace ("limit (MPBall -> CN MPBall): accuracies = " ++ show accuracies) $
--     tryAccuracies accuracies
--     where
--     acX = getFiniteAccuracy x
--     accuracies = aux (fromAccuracy acX)
--       where
--       aux a
--         | a >= 4 = bits a : aux ((100 * a) `divI` 105)
--         | otherwise = [bits a]
--     x = increasePrec xPre
--       where
--       increasePrec z = setPrecision (inc $ getPrecision xPre) z
--       inc p =
--           prec $ (integer p) + 10
--           -- prec $ ((101 * (integer p)) `div` 100) + 1
--     xC = centreAsBall x
--     xE = radius x

--     tryAccuracies [] =
--       noValueNumErrorPotentialECN (Nothing :: Maybe MPBall) $
--         NumError "limit (MPBall -> CN MPBall) failed"
--     tryAccuracies (ac : rest) =
--       let result = withAccuracy ac in
--       case getErrorsCN result of
--         [] -> result
--         _ -> tryAccuracies rest

--     withAccuracy ac =
--       maybeTrace ("limit (MPBall -> CN MPBall): withAccuracy: ac = " ++ show ac) $
--       lift1CE (updateRadius (+ (errorBound e))) fx
--       where
--       e = 0.5^!(fromAccuracy ac)
--       WithLipschitz f f' = ffs e
--       fxC_CN = f xC
--       f'x_CN = f' x
--       fx :: CN MPBall
--       fx =
--           case (ensureNoCN fxC_CN, ensureNoCN f'x_CN) of
--             ((Just fxC, []), (Just f'x, [])) ->
--               cn $ updateRadius (+ (xE * (errorBound f'x))) fxC
--             _ ->
--               f x -- fallback

-- instance HasLimits Rational (WithLipschitz (CReal -> CReal)) where
--   type LimitType Rational (WithLipschitz (CReal -> CReal)) = (CReal -> CReal)
--   limit ffs x = limit fs x
--     where
--     fs e = f
--       where
--       WithLipschitz f _ = ffs e
