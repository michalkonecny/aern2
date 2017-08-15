{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
#define DEBUG
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

import AERN2.QA.Protocol
import AERN2.Sequence
import AERN2.Real

{-|
  @sqrtApprox k x@

  preconditions: @x > 0@,  @k >= 0@

  returns a real number that is within
  $2^{{}-k}$ away from $\sqrt{x}$.
-}
sqrtApprox ::
  (OrderedField r
  , CanAbsSameType r
  , CanTestMVIsPositiveUpTo r
  , HasBools (MVIsPositiveUpToType r)
  , CanChoose (MVIsPositiveUpToType r) (EnsureCN r)
  -- , CanChoose (OrderCompareType Rational r) (EnsureCN r)
  , Show r)
  =>
  Integer -> r -> EnsureCN r
sqrtApprox p x =
  while x isAccurate step
  where
  isAccurate y = mvIsPositiveUpTo (p+1) (0.5^!(p+1) - abs(y*y-x))
  -- isAccurate y = 0.5^!(p) >= abs(y*y-x)
  step y = (y + (x/!y))/!2

mysqrt ::
  (OrderedField r
  , CanAbsSameType r
  , CanTestMVIsPositiveUpTo r
  , HasBools (MVIsPositiveUpToType r)
  , CanChoose (MVIsPositiveUpToType r) (EnsureCN r)
  -- , CanChoose (OrderCompareType Rational r) (EnsureCN r)
  , HasLimits Accuracy (r -> (EnsureCN r))
  , Show r)
  =>
  r -> EnsureCN r
mysqrt = limit sqrtApprox

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
  limit :: (ix -> s) -> s

instance (HasLimits Accuracy t) => HasLimits Integer t where
  limit s = limit (s . fromAccuracy)

instance HasLimits Accuracy CauchyReal where
  limit s = newCR "limit" [] makeQ
    where
    makeQ (me, _src) ac@(AccuracySG acS _acG) =
      updateRadius (+ e) $ (s (acS + 1) ?<- me) (ac + 1)
      where
      e = errorBound $ 0.5^!(fromAccuracy acS + 1)

instance HasLimits Accuracy (CauchyReal -> CauchyRealCN) where
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
  limit s x =
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
    xPNext = setPrecision (2 * (getPrecision x)) x

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
      lift1CE (updateRadius (+ acEB)) (s ac xPNext)
      where
      acI = fromAccuracy ac
      acEB = errorBound $ 0.5^!acI
