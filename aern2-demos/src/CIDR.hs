{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|

Experimenting with programming in CIDR shallow
embedding in Haskell/AERN2.

CIDR is a core language for exact real computation
being developed within the CID EU project.

-}
module CIDR where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif

import MixedTypesNumPrelude
-- import qualified Prelude as P

import Control.CollectErrors

import AERN2.MP.Precision
import AERN2.MP.Dyadic

import AERN2.QA.Protocol
import AERN2.Sequence
import AERN2.Real

------------------------------------------------------
-- example: sqrt via limit using various strategies
------------------------------------------------------

{-|
  @sqrtApprox k x@

  preconditions: @x > 1@,  @k >= 0@

  returns a real number that is within
  $2^{{}-k}$ away from $\sqrt{x}$.
-}
sqrtApprox ::
  _ => Rational -> r -> EnsureCN r
sqrtApprox p x =
  while x isAccurate step
  where
  isAccurate y = mvApproxEq p y (x/!y)
  step y = (y + (x/!y))/!2

data UseLipschitz = UseLipschitz_YES | UseLipschitz_NO

mysqrt ::
  _ => r -> EnsureCN r
mysqrt x = mysqrtX UseLipschitz_YES x

{-|
  @mysqrt useLipschitz useNearbySimpler x@

  preconditions: @x > 1@

  returns $\sqrt{x}$.
-}
mysqrtX ::
  _ => UseLipschitz -> r -> EnsureCN r
mysqrtX useLipschitz (x :: r) =
  case useLipschitz of
    UseLipschitz_NO ->
      (limit (sqrtApprox :: Rational -> r -> EnsureCN r)) x
    UseLipschitz_YES ->
      (limit sqrtApproxL) x
  where
  sqrtApproxL n =
    WithLipschitz
      (sqrtApprox n :: r -> EnsureCN r)
      (\ _ -> (cn (half :: r)))
  half = convertExactly (dyadic 0.5)

-- | A n-times nested sqrt:
mysqrtNx ::
  (_) => UseLipschitz -> Integer -> r -> r
mysqrtNx useLipschitz n x =
  foldl1 (.) (replicate n ((~!) . mysqrtUL)) x
  where
  mysqrtUL = mysqrtX useLipschitz

-- Cauchy reals are slow here:
mysqrtNx_CR_test :: CauchyReal
mysqrtNx_CR_test =
  mysqrtNx UseLipschitz_NO 10 (real 2)
-- [1.000677130693066 ± <2^(-100)]
-- (28.80 secs, 40,191,745,944 bytes)

-- a rapid loss of accuracy, eg:
mysqrtNx_vanilla_test :: MPBall
mysqrtNx_vanilla_test =
  mysqrtNx UseLipschitz_NO 10 (mpBallP (prec 10000) 2)
-- [1.000677130693066 ± <2^(-6435)]
-- (0.10 secs, 166,158,944 bytes)

-- Lipschitz information radically reduces the loss of accuracy, eg:
mysqrtNx_Lip_test :: MPBall
mysqrtNx_Lip_test =
  mysqrtNx UseLipschitz_YES 10 (mpBallP (prec 10000) 2)
-- [1.000677130693066 ± <2^(-9610)]
-- (0.09 secs, 156,292,424 bytes)


----------------
-- redundant comparison
----------------

class CanMVApproxCompare t1 t2 where
  type CanMVApproxCompareType t1 t2
  {-| @mvIsPositiveUpTo p x@

      Return true if @x@ is above $2^{{}-p}$.

      Return false if the number is negative.

      Return true or false if the number is between 0 and $2^{{}-p}$.
  -}
  mvApproxEq :: Rational -> t1 -> t2 -> CanMVApproxCompareType t1 t2

instance CanMVApproxCompare MPBall MPBall where
  type CanMVApproxCompareType MPBall MPBall = Maybe Bool
  mvApproxEq p l r
    | d !<! (p/!2) = Just True
    | d !>! 0 = Just False
    | otherwise = Nothing
    where
    d = abs(l - r)

instance CanMVApproxCompare (CN MPBall) (CN MPBall) where
  type CanMVApproxCompareType (CN MPBall) (CN MPBall) = Maybe Bool
  mvApproxEq p lCN rCN =
    case (ensureNoCN lCN, ensureNoCN rCN) of
      ((Just l, []), (Just r, [])) -> mvApproxEq p l r
      _ -> Nothing

instance CanMVApproxCompare CauchyReal CauchyReal where
  type CanMVApproxCompareType CauchyReal CauchyReal = Bool
  mvApproxEq p l r =
    searchForDecision $ map (uncurry $ mvApproxEq p) $ map (\ac -> (l ? ac, r ? ac)) acs
    where
    acs = map (accuracySG .  bits) $ standardPrecisions (prec 2)
    searchForDecision (Just d : _) = d
    searchForDecision (_ : rest) = searchForDecision rest
    searchForDecision [] = error "mvIsPositiveUpTo CauchyReal: failed to decide"

instance CanMVApproxCompare CauchyRealCN CauchyRealCN where
  type CanMVApproxCompareType CauchyRealCN CauchyRealCN = Bool
  mvApproxEq p l r =
    searchForDecision $ map (uncurry $ mvApproxEq p) $ map (\ac -> (l ? ac, r ? ac)) acs
    where
    acs = map (accuracySG .  bits) $ standardPrecisions (prec 2)
    searchForDecision (Just d : _) = d
    searchForDecision (_ : rest) = searchForDecision rest
    searchForDecision [] = error "mvIsPositiveUpTo CauchyRealCN: failed to decide"

--------------
-- while loop with many-valued condition
--------------

while ::
  (CanChoose b (EnsureCN t), HasBools b, CanEnsureCN t, Show t)
  =>
  t -> (t -> b) -> (t -> t) -> EnsureCN t
while initS cond body =
  maybeTrace ("while: s = " ++ show initS) $
  choose
    [ (cond initS, cn initS)
    , (convertExactly True, while (body initS) cond body)
    ]

----------
-- choose non-deterministically
----------

class (CanEnsureCN t) => CanChoose b t where
  {-| Return any of the values in the associated list whose key is true.
      This is useful when the conditions are only semi-decidable.
      In this case the conditions should be evaluated in a parallel or interleaved
      way so that when some tests do not terminate and others do, the whole
      expression terminates.
  -}
  choose :: [(b,t)] -> EnsureCN t

instance (CanEnsureCN t) => CanChoose Bool t where
  choose ([] :: [(Bool,t)]) =
    noValueNumErrorCertainECN (Nothing :: Maybe t) $ NumError "choose: all options failed"
  choose ((b, t) : rest)
    | b = cn t
    | otherwise = choose rest

instance (CanEnsureCN t) => CanChoose (Maybe Bool) t where
  choose = aux
    where
    aux ([] :: [(Maybe Bool,t)]) =
      noValueNumErrorCertainECN (Nothing :: Maybe t) $ NumError "choose: all options failed"
    aux ((b, t) : rest)
      | b == Just True = cn t
      | b == Just False = aux rest
      | otherwise =
          noValueNumErrorPotentialECN (Nothing :: Maybe t) $ NumError "choose: undecided predicate"

instance
  (CanChoose b t, SuitableForSeq t, SuitableForSeq (EnsureCN t), CanIntersectCNSameType t)
  =>
  CanChoose (Sequence b) (Sequence t) where
  choose (options :: [(Sequence b, Sequence t)]) =
    newSeq (undefined :: (EnsureCN t))  "choose" (map (AnyProtocolQA . snd) options) makeQ
    where
    makeQ (me, _src) ac =
      choose (map (\(b,t) -> ((b ?<- me) ac, (t ?<- me) ac)) options)

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

---------
-- nearby
---------

class CanFindNearbySimpler r where
  nearbySimpler :: Rational -> r -> r

instance CanFindNearbySimpler MPBall where
  nearbySimpler epsilon x
    | radius x <= epsilon = centreAsBall x
    | otherwise = x

instance CanFindNearbySimpler CauchyRealCN where
  nearbySimpler _ x = x -- TODO

instance CanFindNearbySimpler (CN MPBall) where
  nearbySimpler eps x = lift1CE (nearbySimpler eps) x
