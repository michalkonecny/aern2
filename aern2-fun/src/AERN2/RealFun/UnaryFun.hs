{-|
    Module      :  AERN2.RealFun.UnaryFun
    Description :  Real functions represented by Haskell evaluators
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Real functions represented by Haskell evaluators
-}

module AERN2.RealFun.UnaryFun
(
  UnaryFun(..), unaryFun
)
where

import Numeric.MixedTypes
import qualified Prelude as P
-- import Text.Printf

import Data.Typeable

import Control.Arrow
import Control.Applicative

import Control.Lens.Operators

import qualified AERN2.PQueue as Q -- used in a range algorithm

-- import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

import Numeric.CatchingExceptions

import AERN2.MP.Dyadic
import AERN2.MP.Ball

import AERN2.QA
import AERN2.Real

import AERN2.Interval
import AERN2.RealFun.Operations

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
-- shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace
    | shouldTrace = trace
    | otherwise = const id

_dummy :: ()
_dummy = maybeTrace "dummy" ()


data UnaryFun =
  UnaryFun
  {
    unaryFun_Domain :: DyadicInterval
    ,
    {-| For convergent sequence of *open* balls the resulting sequence should also converge. -}
    unaryFun_Eval :: CatchingNumExceptions MPBall -> CatchingNumExceptions MPBall
  }

instance HasDomain UnaryFun where
  type Domain UnaryFun = DyadicInterval
  getDomain = unaryFun_Domain

type CanBeUnaryFun t = ConvertibleExactly t UnaryFun
unaryFun :: (CanBeUnaryFun t) => t -> UnaryFun
unaryFun = convertExactly

instance (CanBeMPBall t, Show t, Typeable t)
  =>
  ConvertibleExactly (DyadicInterval, t) UnaryFun
  where
  safeConvertExactly (dom, x) =
    case safeConvertExactly x of
      Right b -> Right $ UnaryFun dom (fmap $ const b)
      _err -> convError "unable to convert to constant function: " (dom,x)

instance HasVars UnaryFun where
  type Var UnaryFun = ()
  varFn sampleF () =
    UnaryFun dom id
    where
    dom = getDomain sampleF

{- evaluation -}

instance CanApply UnaryFun (CatchingNumExceptions MPBall) where
  type ApplyType UnaryFun (CatchingNumExceptions MPBall) = CatchingNumExceptions MPBall
  apply f = unaryFun_Eval f . checkInDom f

checkInDom :: UnaryFun -> CatchingNumExceptions MPBall -> CatchingNumExceptions MPBall
checkInDom f cb =
  case cb ^. numEXC_maybeValue of
    Just b | domB `contains` b -> cb
    Just b | domB ?<=? b && b ?<=? domB ->
      fmap (intersect domB) cb
    Just _ -> addCertainException cb (OutOfRange "apply UnaryFun: argument out of function domain")
    _ -> cb
  where
  domB = mpBall (unaryFun_Domain f)

instance (QAArrow to) => CanApply UnaryFun (CauchyRealA to) where
  type ApplyType UnaryFun (CauchyRealA to) = (CauchyRealA to)
  apply f =
    unaryOp "apply" (apply f) (getInitQ1FromSimple (arr id))

instance CanApply UnaryFun Integer where
  type ApplyType UnaryFun Integer = CatchingNumExceptions MPBall
  apply f = apply f . catchingNumExceptions . mpBall

instance CanApply UnaryFun Int where
  type ApplyType UnaryFun Int = CatchingNumExceptions MPBall
  apply f = apply f . catchingNumExceptions . mpBall

instance CanApply UnaryFun Dyadic where
  type ApplyType UnaryFun Dyadic = CatchingNumExceptions MPBall
  apply f = apply f . catchingNumExceptions . mpBall

instance CanApply UnaryFun DyadicInterval where
  type ApplyType UnaryFun DyadicInterval = Interval CauchyReal CauchyReal
  apply = rangeOnInterval

rangeOnInterval ::
  (QAArrow to)
  =>
  UnaryFun ->
  DyadicInterval ->
  Interval (CauchyRealA to) (CauchyRealA to)
rangeOnInterval (UnaryFun _dom f) di =
    Interval l r
    where
    l = convergentList2CauchyRealA "range min" $ filterNoException 100 True minSequence
    r = convergentList2CauchyRealA "range max" $ filterNoException 100 True maxSequence
    maxSequence = search fi fdiL $ Q.singleton $ MaxSearchSegment di fdiL fdiR
        where
        (fdiL, fdiR) = gunzip $ fmap endpoints fdi
        fdi = fi di
        fi = onDyadicInterval f
    minSequence = map negate $ search fi fdiL $ Q.singleton $ MaxSearchSegment di fdiL fdiR
        where
        (fdiL, fdiR) = gunzip $ fmap endpoints fdi
        fdi = fi di
        fi = negate . onDyadicInterval  f
    search fi prevL prevQueue =
        maybeTrace
        (
            "UnaryFun rangeOnInterval search:"
            ++ "\n  seg = " ++ show seg
            ++ "\n  normLog(width(seg)) = " ++ show (getNormLog (intervalWidth seg))
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
        currentBall = liftA2 fromEndpoints nextL segValR

        -- split the current segment and pre-compute
        (seg1, seg2) = splitInterval seg
        (seg1ValL, seg1ValR) = fiEE seg1
        (seg2ValL, seg2ValR) = fiEE seg2
        seg1NoMax = (seg1ValR <= nextL) == Just (Just True)
        seg2NoMax = (seg2ValR <= nextL) == Just (Just True)
        nextQueue1 =
            if seg1NoMax then rest else Q.insert seg1E rest
        nextQueue12 =
            if seg2NoMax then nextQueue1 else Q.insert seg2E nextQueue1
        seg1E = MaxSearchSegment seg1 seg1ValL seg1ValR
        seg2E = MaxSearchSegment seg2 seg2ValL seg2ValR

        fiEE s =
            gunzip $ fmap endpoints $ fi s

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


onDyadicInterval ::
  (CatchingNumExceptions MPBall -> CatchingNumExceptions MPBall)
  ->
  (DyadicInterval -> CatchingNumExceptions MPBall)
onDyadicInterval f (Interval l r) =
    maybeTrace
    (
        "onDyadicInterval:"
        ++ "\n nl = " ++ show nl
        ++ "\n precisions = " ++ show (take (int 10) precisions)
        ++ "\n result accuracy = " ++ show (getAccuracy result)
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
            NormBits i -> (max 10 (-i))
            NormZero -> error "onDyadicInterval does not work for a singleton interval"
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
