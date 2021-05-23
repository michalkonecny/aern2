{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|

Experimenting with programming in CIDR shallow
embedding in Haskell/AERN2.

CIDR is an experimental core language for exact real computation
briefly developed within the CID EU project in 2017.

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

import AERN2.Limit
import AERN2.QA.Protocol
import AERN2.Sequence
import AERN2.Real

--------------------------------------------------
-- example: sqrt as used in Isabelle formalisation
--------------------------------------------------

sqrtNewton ::
  _ => Rational -> r -> r -> r
sqrtNewton p x y =
  if yIsAccurate
    then y
    else sqrtNewton p x yNext
  where
  yIsAccurate = mvApproxEq p y (x/y)
  yNext = (y+x/y)/2

sqrtNewton_F :: Rational -> Double -> Double -> Double
sqrtNewton_F = sqrtNewton

sqrtNewton_I :: Rational -> CN MPBall -> CN MPBall -> CN MPBall
sqrtNewton_I = sqrtNewton

sqrtNewton_R :: Rational -> CauchyRealCN -> CauchyRealCN -> CauchyRealCN
sqrtNewton_R = sqrtNewton

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
  @mysqrt useLipschitz x@

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
  {-| @mvApproxEq p l r@ where @p>0@

      Return true if @|l-r|@ is $< p$.

      Return false if @|l-r|@ is $> p/2$.

      Return true or false if @|l-r|@ is between $p/2$ and $p$.
  -}
  mvApproxEq :: Rational -> t1 -> t2 -> CanMVApproxCompareType t1 t2

instance CanMVApproxCompare MPBall MPBall where
  type CanMVApproxCompareType MPBall MPBall = Maybe Bool
  mvApproxEq p l r
    | d !<! p = Just True
    | d !>! p/!2 = Just False
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

instance CanMVApproxCompare Double Double where
  type CanMVApproxCompareType Double Double = Bool
  mvApproxEq p l r
    | d < (double (3*p/!4)) = True
    | otherwise = False
    where
    d = abs(l - r)

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
