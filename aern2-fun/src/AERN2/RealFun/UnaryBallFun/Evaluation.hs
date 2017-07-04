{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.RealFun.UnaryBallFun.Evaluation
    Description :  evaluation and range
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Evaluation and range computation
-}

module AERN2.RealFun.UnaryBallFun.Evaluation
(
  evalOnIntervalGuessPrecision
  , minimumOnIntervalSubdivide
  , maximumOnIntervalSubdivide
  , rangeOnIntervalSubdivide
)
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (\ (_ :: String) t -> t)
#endif

import MixedTypesNumPrelude
import qualified Prelude as P
import Text.Printf

import Control.Arrow
import Control.Applicative

import qualified AERN2.PQueue as Q


import AERN2.Norm
import AERN2.MP
import AERN2.MP.Dyadic

import AERN2.QA.Protocol
import qualified AERN2.Sequence as S
import AERN2.Real

import AERN2.Interval (Interval(..), DyadicInterval, RealInterval)
import qualified AERN2.Interval as Interval
import AERN2.RealFun.Operations

import AERN2.RealFun.UnaryBallFun.Type

instance CanApply UnaryBallFun MPBall where
  type ApplyType UnaryBallFun MPBall = CN MPBall
  apply f@(UnaryBallFun _ eval) x =
    eval $ checkInDom f (cn x)

checkInDom ::
  (HasOrderCertainly t Dyadic, CanMinMaxThis t Dyadic, CanEnsureCN t)
  =>
  UnaryBallFun -> t -> EnsureCN t
checkInDom f (x :: t)
  | domL !<=! x && x !<=! domR = cn x
  | x !<! domL || domL !<! x =
    prependErrorsECN (Nothing :: Maybe t) [(ErrorCertain, err)] $ cn xIntersected
  | otherwise =
    prependErrorsECN (Nothing :: Maybe t) [(ErrorPotential, err)] $ cn xIntersected
  where
  err = OutOfRange "apply UnaryBallFun: argument out of function domain"
  Interval domL domR = unaryBallFun_Domain f
  xIntersected = min domR $ max domL x

instance
  -- (CanApply UnaryBallFun t, HasOrderCertainly t Dyadic, CanMinMaxThis t Dyadic)
  -- =>
  CanApply UnaryBallFun (CN MPBall)
  where
  type ApplyType UnaryBallFun (CN MPBall) = CN MPBall
  apply f@(UnaryBallFun _ eval) cx =
    eval $ checkInDom f cx

instance (QAArrow to) => CanApply UnaryBallFun (CauchyRealA to) where
  type ApplyType UnaryBallFun (CauchyRealA to) = (CauchyRealCNA to)
  apply f =
    S.unaryOp "apply" (apply f) (S.getInitQ1FromSimple (arr id))

instance CanApply UnaryBallFun Integer where
  type ApplyType UnaryBallFun Integer = CauchyRealCN
  apply f = apply f . real

instance CanApply UnaryBallFun Int where
  type ApplyType UnaryBallFun Int = CauchyRealCN
  apply f = apply f . real

instance CanApply UnaryBallFun Dyadic where
  type ApplyType UnaryBallFun Dyadic = CauchyRealCN
  apply f = apply f . real

instance CanApply UnaryBallFun DyadicInterval where
  type ApplyType UnaryBallFun DyadicInterval = Interval CauchyReal CauchyReal
  apply f di =
    Interval (minimumOverDom f di) (maximumOverDom f di)

instance CanMaximiseOverDom UnaryBallFun DyadicInterval where
  type MaximumOverDomType UnaryBallFun DyadicInterval = CauchyReal
  maximumOverDom (UnaryBallFun _ f) =
    maximumOnIntervalSubdivide (((,) Nothing) . evalOnIntervalGuessPrecision f)

instance CanMinimiseOverDom UnaryBallFun DyadicInterval where
  type MinimumOverDomType UnaryBallFun DyadicInterval = CauchyReal
  minimumOverDom (UnaryBallFun _ f) =
    minimumOnIntervalSubdivide (((,) Nothing) . evalOnIntervalGuessPrecision f)

evalOnIntervalGuessPrecision ::
  (CN MPBall -> CN MPBall)
  ->
  (DyadicInterval -> CN MPBall)
evalOnIntervalGuessPrecision f (Interval l r) =
    maybeTrace (
        "evalOnIntervalGuessPrecision (1):"
        ++ "\n (l,r) = " ++ show (l,r)
        ++ "\n nl = " ++ show nl
        ++ "\n precisions = " ++ show (take (int 21) precisions)
    ) $ maybeTrace (
        "evalOnIntervalGuessPrecision (2):"
        ++ "\n resultsWithIncreasingPrecision = " ++ show (take (int 21) resultsWithIncreasingPrecision)
    ) $ maybeTrace (
        "evalOnIntervalGuessPrecision (3):"
        ++ "\n result accuracy = " ++ show (getAccuracy result)
        ++ "\n result = " ++ show (result)
    ) $
    result
    where
    result = untilLittleImprovement resultsWithIncreasingPrecision
    resultsWithIncreasingPrecision = map fp precisions
    fp p = f b
        where
        b = cn $ (fromEndpoints lMP rMP :: MPBall)
        lMP = setPrecision p $ mpBall l
        rMP = setPrecision p $ mpBall r
    precisions =
        drop (int 1) $ -- ignore the initial precision
        map prec precisions'
    precisions' = -- Fibonacci series starting with initPrec, initPrec+10, 2*initPrec + 10, ...
        initPrec : (initPrec+10) : zipWith (+) precisions' (drop (int 1) precisions')
    initPrec =
        case nl of
            NormBits i -> max 10 (-i)
            NormZero -> integer $ getPrecision l
    nl = getNormLog (r - l)
    untilLittleImprovement resultsCN =
        case results of
            [] -> head resultsCN
            _ ->
                maybeTrace ("untilLittleImprovement: improvements = " ++ show (take (int 10) improvements)) $
                cn $ pickFirstResultWithLowImprovement $ zip improvements results
        where
        results = filterNoException 20 False resultsCN
        pickFirstResultWithLowImprovement [(_,res)] = res
        pickFirstResultWithLowImprovement ((improvementPrec, res) : rest)
            | improvementPrec == NormZero = res
            | otherwise = pickFirstResultWithLowImprovement rest
        pickFirstResultWithLowImprovement _ = error "internal error in onRationalInterval"
        radii = map (mpBall . dyadic . radius) results
        improvements = zipWith measureImprovement radii (drop (int 1) radii)
        measureImprovement r1 r2 = getNormLog $ max (mpBall 0) $ r1 - r2

filterNoException ::
  (CanEnsureCN a, Show a) =>
  Integer -> Bool -> [a] -> [EnsureNoCN a]
filterNoException maxConsequentExceptions shouldErrorOnMaxReached =
  aux maxConsequentExceptions
  where
  aux _ [] = []
  aux n (xCN:rest) =
    case ensureNoCN xCN of
      Right x -> x : (aux maxConsequentExceptions rest)
      _
        | n < 1 && shouldErrorOnMaxReached ->
          error $ printf "filterNoException: reached maxConsequentExceptions (%d): %s" maxConsequentExceptions (show xCN)
        | n < 1 -> []
        | otherwise -> aux (n - 1) rest

data MonotonicityDirection = Increasing | Decreasing

instance CanNeg MonotonicityDirection where
  negate Increasing = Decreasing
  negate Decreasing = Increasing

rangeOnIntervalSubdivide ::
  (DyadicInterval -> (Maybe (CN MPBall, CN MPBall), CN MPBall))
  ->
  (DyadicInterval -> RealInterval)
rangeOnIntervalSubdivide evalOnInterval di =
  Interval
    (minimumOnIntervalSubdivide evalOnInterval di)
    (maximumOnIntervalSubdivide evalOnInterval di)

minimumOnIntervalSubdivide ::
  (DyadicInterval -> (Maybe (CN MPBall, CN MPBall), CN MPBall))
  ->
  (DyadicInterval -> CauchyReal)
minimumOnIntervalSubdivide evalOnInterval =
  negate . maximumOnIntervalSubdivide negEvalOnInterval
  where
  negEvalOnInterval di =
    case evalOnInterval di of
      (Just (minB, maxB), v) -> (Just (-maxB,-minB), -v)
      (_, v) -> (Nothing, -v)

gunzip :: (Functor f) => (f (a,b)) -> (f a, f b)
gunzip ab =(fmap fst ab, fmap snd ab)

maximumOnIntervalSubdivide ::
  (DyadicInterval -> (Maybe (CN MPBall, CN MPBall), CN MPBall))
  ->
  (DyadicInterval -> CauchyReal)
maximumOnIntervalSubdivide evalOnInterval di =
  res
  where
  res = convergentList2CauchyRealA "range max" $ filterNoException 100 True maxSequence
  maxSequence = search fi fdiL $ Q.singleton $ MaxSearchSegment di fdiL fdiR
    where
    (fdiL, fdiR) = gunzip $ fmap endpoints fdi
    (_,fdi) = fi di
    fi = evalOnInterval
  search fi prevL prevQueue =
    maybeTrace (
        "UnaryBallFun maximumOnIntervalSubdivide search:"
        ++ "\n  seg = " ++ show seg
        ++ "\n  normLog(width(seg)) = " ++ show (getNormLog (Interval.width seg))
    -- ) $ maybeTrace (
    --     "UnaryBallFun maximumOnIntervalSubdivide search (2):"
        ++ "\n  nextL = " ++ show nextL
        ++ "\n  segValR = " ++ show segValR
        ++ "\n  currentBall = " ++ show currentBall
        ++ "\n  accuracy(currentBall) = " ++ show (getAccuracy currentBall)
    ) $
    currentBall :
      search fi nextL nextQueue12
    where
    -- unpack the current segment and a pre-computed enclosure of the function on this segment:
    Just (MaxSearchSegment seg segValL segValR, rest) = Q.minView prevQueue
    -- get an enclosure of the function's maximum based on previous segments and the current segment:
    nextL
      | hasCertainErrorCN prevL = segValL
      | otherwise = liftA2 max segValL prevL
    currentBall :: CN MPBall
    currentBall = liftA2 fromEndpoints nextL segValR

    -- split the current segment and pre-compute
    (seg1, seg2) = Interval.split seg
    ((seg1ValL, seg1ValR), seg1') = fiEE seg1
    ((seg2ValL, seg2ValR), seg2') = fiEE seg2
    seg1NoMax = fmap getMaybeValueCN (seg1ValR <= nextL) == Just (Just True)
    seg2NoMax = fmap getMaybeValueCN (seg2ValR <= nextL) == Just (Just True)
    nextQueue1 =
      if seg1NoMax then rest else Q.insert seg1E rest
    nextQueue12 =
      if seg2NoMax then nextQueue1 else Q.insert seg2E nextQueue1
    seg1E = MaxSearchSegment seg1' seg1ValL seg1ValR
    seg2E = MaxSearchSegment seg2' seg2ValL seg2ValR

    fiEE s =
      maybeTrace (
          "UnaryBallFun maximumOnIntervalSubdivide search: fiEE:"
          ++ "\n  s = " ++ show s
      ) $ maybeTrace (
          "UnaryBallFun maximumOnIntervalSubdivide search: fiEE:"
          ++ "\n  maybeMonotone = " ++ show maybeMonotone
      ) $ maybeTrace (
          "UnaryBallFun maximumOnIntervalSubdivide search: fiEE:"
          ++ "\n  fis = " ++ show fis
      ) $
      case maybeMonotone of
        Nothing -> (gunzip $ fmap endpoints fis, s)
        Just (minB, maxB) -> ((minB, maxB), s)
      where
      (maybeMonotone, fis) = fi s
      -- lI = Interval lE lE
      -- rI = Interval rE rE
      -- (Interval lE rE) = s

data MaxSearchSegment =
    MaxSearchSegment
    {
        _maxSearchSegment_seg :: DyadicInterval,
        _maxSearchSegment_lowerBnd :: CN MPBall, -- should be exact
        _maxSearchSegment_upperBnd :: CN MPBall -- should be exact
    }
    deriving (Show)

instance P.Eq MaxSearchSegment where
    (MaxSearchSegment (Interval l1 r1) _ _) == (MaxSearchSegment (Interval l2 r2) _ _) =
        l1 == l2 && r1 == r2
instance P.Ord MaxSearchSegment where
  compare (MaxSearchSegment _ _ u2CN) (MaxSearchSegment _ _ u1CN) =
    case (ensureNoCN u1CN, ensureNoCN u2CN) of
      (Right u1, Right u2)
        | u1 !<! u2 -> P.LT
        | u1 !>! u2 -> P.GT
      (Right _, _) -> P.LT
      (_, Right _) -> P.GT
      _ -> P.EQ
      --   case (u1 < u2, u1 > u2) of
      --     (Just True, _) -> P.LT
      --     (_, Just True) -> P.GT
      -- _
      --   | hasCertainErrorCN u1 && hasCertainErrorCN u2 -> P.EQ
      --   | hasCertainErrorCN u1 -> P.GT
      --   | hasCertainErrorCN u2 -> P.LT
      --   | otherwise -> P.EQ
