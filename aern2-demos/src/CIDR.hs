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
-- sqrt example using limit WITHOUT Lipschitz info
------------------------------------------------------

{-|
  @sqrtApprox k x@

  preconditions: @x > 1@,  @k >= 0@

  returns a real number that is within
  $2^{{}-k}$ away from $\sqrt{x}$.
-}
sqrtApprox ::
  _ => Integer -> r -> EnsureCN r
sqrtApprox p x =
  while x isAccurate step
  where
  isAccurate y = mvIsPositiveUpTo (p+1) (0.5^!(p+1) - abs(y*y-x))
  step y = (y + (x/!y))/!2

{-|
  @mysqrt x@

  preconditions: @x > 1@

  returns $\sqrt{x}$.
-}
mysqrt ::
  _ => r -> EnsureCN r
mysqrt (x :: r) = (limit (sqrtApprox :: Integer -> r -> EnsureCN r)) x

mysqrtNx ::
  _ => Integer -> r -> r
mysqrtNx n x =
  foldl1 (.) (replicate n ((~!) . mysqrt)) x

-- a rapid loss of accuracy, eg:
_test_mysqrtNx :: MPBall
_test_mysqrtNx =
  mysqrtNx 10 (mpBallP (prec 10000) 2)
-- [1.000677130693093 ± <2^(-18)]


------------------------------------------------------
-- sqrt example using limit WITH Lipschitz info
------------------------------------------------------

{-|
  @mysqrt x@

  preconditions: @x > 1@

  returns $\sqrt{x}$.
-}
mysqrtL ::
  _ => r -> EnsureCN r
mysqrtL (x :: r) =
  limit sqrtApproxL x
  where
  sqrtApproxL n =
    WithLipschitz
      (sqrtApprox n :: r -> EnsureCN r)
      (\ _ -> (cn (half :: r)))
  half = convertExactly (dyadic 0.5)

mysqrtLNx ::
  _ => Integer -> r -> r
mysqrtLNx n x =
  foldl1 (.) (replicate n ((~!) . mysqrtL)) x

-- a much less rapid loss of accuracy, eg:
_test_mysqrtLNx :: MPBall
_test_mysqrtLNx =
  mysqrtLNx 10 (mpBallP (prec 10000) 2)
-- [1.000677130693066 ± <2^(-515)]

----------------
-- redundant comparison
----------------

class CanTestMVIsPositiveUpTo t where
  type MVIsPositiveUpToType t
  {-| @mvIsPositiveUpTo p x@

      Return true if @x@ is above $2^{{}-p}$.

      Return false if the number is negative.

      Return true or false if the number is between 0 and $2^{{}-p}$.
  -}
  mvIsPositiveUpTo :: Integer -> t -> MVIsPositiveUpToType t

instance CanTestMVIsPositiveUpTo MPBall where
  type MVIsPositiveUpToType MPBall = Maybe Bool
  mvIsPositiveUpTo p b
    | b !<! 0.5^!p = Just False
    | b !>! 0 = Just True
    | otherwise = Nothing

instance CanTestMVIsPositiveUpTo CauchyReal where
  type MVIsPositiveUpToType CauchyReal = Bool
  mvIsPositiveUpTo p r =
    searchForDecision $ map (mvIsPositiveUpTo p) $ map (r ?) acs
    where
    acs = map (accuracySG .  bits) $ standardPrecisions (prec 2)
    searchForDecision (Just d : _) = d
    searchForDecision (_ : rest) = searchForDecision rest
    searchForDecision [] = error "mvIsPositiveUpTo CauchyReal: failed to decide"

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

instance (HasLimits Accuracy t) => HasLimits Integer t where
  type LimitType Integer t = LimitType Accuracy t
  limit s = limit (s . fromAccuracy)

instance HasLimits Accuracy CauchyReal where
  type LimitType Accuracy CauchyReal = CauchyReal
  limit s = newCR "limit" [] makeQ
    where
    makeQ (me, _src) ac@(AccuracySG acS _acG) =
      updateRadius (+ e) $ (s (acS + 1) ?<- me) (ac + 1)
      where
      e = errorBound $ 0.5^!(fromAccuracy acS + 1)

instance HasLimits Accuracy (CauchyReal -> CauchyRealCN) where
  type LimitType Accuracy (CauchyReal -> CauchyRealCN) = (CauchyReal -> CauchyRealCN)
  limit fs x = newCRCN "limit" [AnyProtocolQA x] makeQ
    where
    makeQ (me, _src) ac@(AccuracySG acS _acG) =
      maybeTrace ("limit (CauchyReal -> CauchyRealCN): ac = " ++ show ac ) $
      lift1CE (updateRadius (+ e)) $ (fx ?<- me) (ac + 1)
      where
      fx = fs (acS + 1) x
      e = errorBound $ 0.5^!(fromAccuracy acS + 1)

{-
  The following strategy is inspired by
  Mueller: The iRRAM: Exact Arithmetic in C++, Section 10.1
  https://link.springer.com/chapter/10.1007/3-540-45335-0_14
-}
instance HasLimits Accuracy (MPBall -> CN MPBall) where
  type LimitType Accuracy (MPBall -> CN MPBall) = (MPBall -> CN MPBall)
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
        | a >= 4 = bits a : aux (a `div` 2)
        | otherwise = [bits a]
    xPNext = setPrecision (increaseP $ getPrecision x) x
    increaseP p = prec $ ((105 * (integer p)) `div` 100) + 1

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
      lift1CE (updateRadius (+ acEB)) (fs ac xPNext)
      where
      acI = fromAccuracy ac
      acEB = errorBound $ 0.5^!acI

data WithLipschitz f = WithLipschitz f f

instance HasLimits Accuracy (WithLipschitz (MPBall -> CN MPBall)) where
  type LimitType Accuracy (WithLipschitz (MPBall -> CN MPBall)) = (MPBall -> CN MPBall)
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
        | a >= 4 = bits a : aux (a `div` 2)
        | otherwise = [bits a]
    x = increasePrec xPre
      where
      increasePrec z = setPrecision (inc $ getPrecision xPre) z
      inc p = prec $ ((105 * (integer p)) `div` 100) + 1
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
      lift1CE (updateRadius (+ acEB)) fx
      where
      acI = fromAccuracy ac
      acEB = errorBound $ 0.5^!acI
      WithLipschitz f f' = ffs ac
      fxC_CN = f xC
      f'x_CN = f' x
      fx :: CN MPBall
      fx =
          case (ensureNoCN fxC_CN, ensureNoCN f'x_CN) of
            ((Just fxC, []), (Just f'x, [])) ->
              cn $ updateRadius (+ (xE * (errorBound f'x))) fxC
            _ ->
              f x -- fallback
