{-|
    Module      :  AERN2.RealFun.UnaryFun.Evaluation
    Description :  evaluation and range
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Evaluation and range computation
-}

module AERN2.RealFun.UnaryFun.Evaluation
(
  evalOnIntervalGuessPrecision
  , rangeOnIntervalSubdivide
)
where

import Numeric.MixedTypes
import qualified Prelude as P
-- import Text.Printf

import Control.Arrow
import Control.Applicative

import Control.Lens.Operators
import Control.Lens (_Just)

import qualified AERN2.PQueue as Q

import Numeric.CatchingExceptions

import AERN2.Norm
import AERN2.MP.Accuracy
import AERN2.MP.Precision
import AERN2.MP.Dyadic
import AERN2.MP.Ball (MPBall, mpBall, IsBall(..), IsInterval(..))
import qualified AERN2.MP.Ball as MPBall

import AERN2.QA
import AERN2.Real

import AERN2.Interval (Interval(..), DyadicInterval, RealInterval)
import qualified AERN2.Interval as Interval
import AERN2.RealFun.Operations

import AERN2.RealFun.UnaryFun.Type

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
-- shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace = if shouldTrace then trace else const id
_dummy :: ()
_dummy = maybeTrace "dummy" ()


instance CanApply UnaryFun MPBall where
  type ApplyType UnaryFun MPBall = MPBall
  apply f@(UnaryFun _ eval) =
    ifCertainExceptionDie "UnafyFn application"
      . eval . checkInDom f . catchingNumExceptions

checkInDom ::
  (HasOrderCertainly t Dyadic, CanMinMaxThis t Dyadic)
  =>
  UnaryFun -> CatchingNumExceptions t -> CatchingNumExceptions t
checkInDom f cx =
  case cx ^. numEXC_maybeValue of
    Just x
      | domL !<=! x && x !<=! domR -> cx
      | x !<! domL || domL !<! x ->
        addCertainException cxIntersected (OutOfRange "apply UnaryFun: argument out of function domain")
      | otherwise ->
        addPotentialException cx (OutOfRange "apply UnaryFun: argument out of function domain")
      where
      cxIntersected = cx & numEXC_maybeValue .~ (Just $ min domR $ max domL x)
    _ -> cx
  where
  Interval domL domR = unaryFun_Domain f

instance
  (CanApply UnaryFun t, HasOrderCertainly t Dyadic, CanMinMaxThis t Dyadic)
  =>
  CanApply UnaryFun (CatchingNumExceptions t)
  where
  type ApplyType UnaryFun (CatchingNumExceptions t) = CatchingNumExceptions (ApplyType UnaryFun t)
  apply f cx =
    (checkInDom f cx) & (numEXC_maybeValue . _Just) %~ apply f

instance (QAArrow to) => CanApply UnaryFun (CauchyRealA to) where
  type ApplyType UnaryFun (CauchyRealA to) = (CauchyRealA to)
  apply f =
    unaryOp "apply" (apply f) (getInitQ1FromSimple (arr id))

instance CanApply UnaryFun Integer where
  type ApplyType UnaryFun Integer = CauchyReal
  apply f = apply f . real

instance CanApply UnaryFun Int where
  type ApplyType UnaryFun Int = CauchyReal
  apply f = apply f . real

instance CanApply UnaryFun Dyadic where
  type ApplyType UnaryFun Dyadic = CauchyReal
  apply f = apply f . real

instance CanApply UnaryFun DyadicInterval where
  type ApplyType UnaryFun DyadicInterval = RealInterval
  apply (UnaryFun _ f) = rangeOnIntervalSubdivide (((,) Nothing) . evalOnIntervalGuessPrecision f)

evalOnIntervalGuessPrecision ::
  (CatchingNumExceptions MPBall -> CatchingNumExceptions MPBall)
  ->
  (DyadicInterval -> CatchingNumExceptions MPBall)
evalOnIntervalGuessPrecision f (Interval l r) =
    maybeTrace
    (
        "evalOnIntervalGuessPrecision:"
        ++ "\n (l,r) = " ++ show (l,r)
        ++ "\n nl = " ++ show nl
        ++ "\n precisions = " ++ show (take (int 10) precisions)
        ++ "\n result accuracy = " ++ show (getAccuracy result)
        ++ "\n result = " ++ show (result)
    ) $
    result
    where
    result = untilLittleImprovement resultsWithIncreasingPrecision
    resultsWithIncreasingPrecision = map fp precisions
    fp p = f b
        where
        b = catchingNumExceptions $ fromEndpoints lMP rMP
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
    untilLittleImprovement resultsE =
        case results of
            [] -> head resultsE
            _ ->
                maybeTrace ("untilLittleImprovement: improvements = " ++ show (take (int 10) improvements)) $
                catchingNumExceptions $ pickFirstResultWithLowImprovement $ zip improvements results
        where
        results = filterNoException 20 False resultsE
        pickFirstResultWithLowImprovement [(_,res)] = res
        pickFirstResultWithLowImprovement ((improvementPrec, res) : rest)
            | improvementPrec == NormZero = res
            | otherwise = pickFirstResultWithLowImprovement rest
        pickFirstResultWithLowImprovement _ = error "internal error in onRationalInterval"
        radii = map (mpBall . dyadic . radius) results
        improvements = zipWith measureImprovement radii (drop (int 1) radii)
        measureImprovement r1 r2 = getNormLog $ max (mpBall 0) $ r1 - r2

-- data MonotonicityDirection = Increasing | Decreasing
--
-- instance CanNeg MonotonicityDirection where
--   negate Increasing = Decreasing
--   negate Decreasing = Increasing

rangeOnIntervalSubdivide ::
  (DyadicInterval -> (Maybe (CatchingNumExceptions MPBall, CatchingNumExceptions MPBall), CatchingNumExceptions MPBall))
  ->
  (DyadicInterval -> RealInterval)
rangeOnIntervalSubdivide evalOnInterval di =
  Interval l r
  where
  l = convergentList2CauchyRealA "range min" $ filterNoException 100 True minSequence
  r = convergentList2CauchyRealA "range max" $ filterNoException 100 True maxSequence
  maxSequence = search fi fdiL $ Q.singleton $ MaxSearchSegment di fdiL fdiR
    where
    (fdiL, fdiR) = gunzip $ fmap MPBall.endpoints fdi
    (_,fdi) = fi di
    fi = evalOnInterval
  minSequence = map negate $ search fi fdiL $ Q.singleton $ MaxSearchSegment di fdiL fdiR
    where
    (fdiL, fdiR) = gunzip $ fmap endpoints fdi
    (_, fdi) = fi di
    fi = negateRes . evalOnInterval
    negateRes (Nothing, c) = (Nothing, -c)
    negateRes (Just (a,b), c) = (Just (-b,-a), -c)
  search fi prevL prevQueue =
    maybeTrace
    (
        "UnaryFun rangeOnInterval search:"
        ++ "\n  seg = " ++ show seg
        ++ "\n  normLog(width(seg)) = " ++ show (getNormLog (Interval.width seg))
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
      | hasCertainException prevL = segValL
      | otherwise = liftA2 max segValL prevL
    currentBall :: CatchingNumExceptions MPBall
    currentBall = liftA2 fromEndpoints nextL segValR

    -- split the current segment and pre-compute
    (seg1, seg2) = Interval.split seg
    ((seg1ValL, seg1ValR), seg1') = fiEE seg1
    ((seg2ValL, seg2ValR), seg2') = fiEE seg2
    seg1NoMax = (seg1ValR <= nextL) == Just (Just True)
    seg2NoMax = (seg2ValR <= nextL) == Just (Just True)
    nextQueue1 =
      if seg1NoMax then rest else Q.insert seg1E rest
    nextQueue12 =
      if seg2NoMax then nextQueue1 else Q.insert seg2E nextQueue1
    seg1E = MaxSearchSegment seg1' seg1ValL seg1ValR
    seg2E = MaxSearchSegment seg2' seg2ValL seg2ValR

    fiEE s =
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
        _maxSearchSegment_lowerBnd :: CatchingNumExceptions MPBall, -- should be exact
        _maxSearchSegment_upperBnd :: CatchingNumExceptions MPBall -- should be exact
    }

instance P.Eq MaxSearchSegment where
    (MaxSearchSegment (Interval l1 r1) _ _) == (MaxSearchSegment (Interval l2 r2) _ _) =
        l1 == l2 && r1 == r2
instance P.Ord MaxSearchSegment where
  compare (MaxSearchSegment _ _ u2) (MaxSearchSegment _ _ u1) =
    case (u1 < u2, u1 > u2) of
      (Just (Just True), _) -> P.LT
      (_, Just (Just True)) -> P.GT
      _
        | hasCertainException u1 && hasCertainException u2 -> P.EQ
        | hasCertainException u1 -> P.GT
        | hasCertainException u2 -> P.LT
        | otherwise -> P.EQ
