{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.Sequence.Helpers
    Description :  helper functions for sequence operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Helper functions for sequence operations.
-}
module AERN2.Sequence.Helpers
(
  -- Operations returning Seq
  unaryOp, binaryOp, binaryOpWithPureArg
  -- Construction of initial queries
  , getInitQ1FromSimple, getInitQ1TFromSimple, getInitQ1Q2FromSimple
  -- Operations returning an enclosure (eg MPBall)
  , binaryWithEncl, binaryWithEnclTranslateAC
  , seqElementSimilarToEncl
  -- misc
  ,getSeqFnNormLog
  ,ensureAccuracyA
)
where

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

import Control.Arrow

import AERN2.MP

import AERN2.QA.Protocol
import AERN2.AccuracySG
import AERN2.Sequence.Type

{- generic implementations of operations of different arity -}

unaryOp ::
  (QAArrow to, SuitableForSeq a, SuitableForSeq b)
  =>
  String ->
  (a -> b) ->
  (Maybe (QAId to) {-^ my id -} -> SequenceA to a -> (AccuracySG `to` (AccuracySG, Maybe a))) ->
  SequenceA to a -> SequenceA to b
unaryOp name op getInitQ1 r1 =
  newSeq (op sampleA1) name [AnyProtocolQA r1] makeQ
  where
  SequenceP sampleA1 = qaProtocol r1
  makeQ (me, _src) =
    proc ac ->
      do
      (q1Init, mb1) <- getInitQ1 me r1 -< ac
      ensureAccuracyA (proc [q1] -> (r1 ?<- me) -< q1) op -< (ac, ([q1Init], mb1))

binaryOpWithPureArg ::
  (QAArrow to, SuitableForSeq a, SuitableForSeq b)
  =>
  String ->
  (a -> t -> b) ->
  (Maybe (QAId to) {-^ my id -} -> SequenceA to a -> t -> (AccuracySG `to` (AccuracySG, Maybe a))) ->
  SequenceA to a -> t -> SequenceA to b
binaryOpWithPureArg name op getInitQ1T r1 t =
  newSeq (op sampleA t) name [AnyProtocolQA r1] makeQ
  where
  SequenceP sampleA = qaProtocol r1
  makeQ (me, _src) =
    proc ac ->
      do
      (q1Init, mb1) <- getInitQ1T me r1 t -< ac
      ensureAccuracyA (proc [q1] -> (r1 ?<- me) -< q1) (flip op t) -< (ac, ([q1Init], mb1))

binaryOp ::
  (QAArrow to, SuitableForSeq a, SuitableForSeq b, SuitableForSeq c)
  =>
  String ->
  (a -> b -> c) ->
  (Maybe (QAId to) {-^ my id -} -> SequenceA to a -> SequenceA to b ->
  (AccuracySG `to` ((AccuracySG, Maybe a), (AccuracySG, Maybe b)))) ->
  SequenceA to a -> SequenceA to b -> SequenceA to c
binaryOp name op getInitQ1Q2 r1 r2 =
  newSeq (op sampleA sampleB) name [AnyProtocolQA r1, AnyProtocolQA r2] makeQ
  where
  SequenceP sampleA = qaProtocol r1
  SequenceP sampleB = qaProtocol r2
  makeQ (me,_src) =
    proc ac ->
      do
      ((q1Init, mb1), (q2Init, mb2)) <- getInitQ1Q2 me r1 r2 -< ac
      ensureAccuracyA
        (proc [q1,q2] -> ((r1,r2) ??<- me) -< (q1,q2))
        (uncurry op)
          -< (ac, ([q1Init, q2Init], do {b1<-mb1;b2<-mb2;Just (b1,b2)}))

{- functions to help determine initial queries -}

getInitQ1FromSimple ::
  (Arrow to)
  =>
  AccuracySG `to` q ->
  Maybe (QAId to) {-^ my id -} -> r1 -> AccuracySG `to` (q, Maybe a)
getInitQ1FromSimple simpleA _ _ =
  proc q ->
    do
    initQ1 <- simpleA -< q
    returnA -< (initQ1, Nothing)

getInitQ1TFromSimple ::
  (Arrow to)
  =>
  AccuracySG `to` q ->
  Maybe (QAId to) {-^ my id -} -> r1 -> t -> AccuracySG `to` (q, Maybe a)
getInitQ1TFromSimple simpleA _ _ _ =
  proc q ->
    do
    initQ1 <- simpleA -< q
    returnA -< (initQ1, Nothing)

getInitQ1Q2FromSimple ::
  (Arrow to)
  =>
  AccuracySG `to` (q,q) ->
  Maybe (QAId to) {-^ my id -} -> r1 -> r2 -> AccuracySG `to` ((q, Maybe a), (q, Maybe b))
getInitQ1Q2FromSimple simpleA _ _ _ =
  proc q ->
    do
    (initQ1, initQ2) <- simpleA -< q
    returnA -< ((initQ1, Nothing), (initQ2, Nothing))

{-
  functions for iterative querying of operands
  until the result is of a sufficient accuracy
-}

ensureAccuracyA ::
  (ArrowChoice to, Show a, Show b
  , HasAccuracy b
  , CanEnsureCN b, HasAccuracy (EnsureNoCN b), Show (EnsureNoCN b))
  =>
  ([AccuracySG] `to` a) ->
  (a -> b) ->
  ((AccuracySG, ([AccuracySG], Maybe a)) `to` b)
ensureAccuracyA getA op =
    proc (q,(js, aPrelim)) ->
        case fmap op aPrelim of
          Just resultPrelim | getAccuracy resultPrelim >= q ->
            returnA -<
                maybeTrace (
                    "ensureAccuracyA: Pre-computed result sufficient. (q = " ++ show q ++
                    "; js = " ++ show js ++
                    "; result accuracy = " ++ (show $ getAccuracy resultPrelim) ++ ")"
                ) $
                resultPrelim
          _ ->
            aux -< (q,js)
    where
    aux =
        proc (q,js) ->
            do
            a <- getA -< js
            let result = op a
            case ensureNoCN result of
              (Just resultNoCN, es) | not (hasCertainError es) ->
                if getAccuracy resultNoCN >= _acStrict q
                  then
                  returnA -<
                      maybeTrace (
                          "ensureAccuracyA: Succeeded. (q = " ++ show q ++
                          "; js = " ++ show js ++
                          "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                      ) $
                      result
                  else
                  aux -<
                      maybeTrace (
                          "ensureAccuracyA: Not enough ... (q = " ++ show q ++
                          "; js = " ++ show js ++
                          "; a = " ++ show a ++
                          "; resultNoCN = " ++ show resultNoCN ++
                          "; resultNoCN accuracy = " ++ (show $ getAccuracy resultNoCN) ++ ")" ++
                          "; result = " ++ show result ++
                          "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                      ) $
                      (q, map (+1) js)
              _ -> returnA -< result -- certain error, give up improving


{- MPBall + CauchyReal = MPBall, only allowed in the (->) arrow  -}

binaryWithEncl ::
  (HasAccuracy b, HasPrecision b, CanSetPrecision t)
  =>
  (a -> b -> t) -> Sequence a -> b -> t
binaryWithEncl = binaryWithEnclTranslateAC (\ _ _ -> id)

binaryWithEnclTranslateAC ::
  (HasAccuracy b, HasPrecision b, CanSetPrecision t)
  =>
  (a -> b -> AccuracySG -> AccuracySG) ->
  (a -> b -> t) -> Sequence a -> b -> t
binaryWithEnclTranslateAC accuracyTranslationForB op sa b =
  lowerPrecisionIfAbove (getPrecision b) $
    op (seqElementSimilarToEncl (flip accuracyTranslationForB b) b sa) b

seqElementSimilarToEncl ::
  (HasAccuracy b, HasPrecision b) =>
  (a -> AccuracySG -> AccuracySG) ->
  b -> Sequence a -> a
seqElementSimilarToEncl accuracyTranslation b sa =
  sa ? (accuracyTranslation a $ accuracySG $ getFiniteAccuracy b)
  where
  a = sa ? acSG0

{- miscellaneous -}

getSeqFnNormLog ::
  (QAArrow to, CanEnsureCN v, HasNorm (EnsureNoCN v))
  =>
  Maybe (QAId to) -> SequenceA to a -> (a -> v) -> AccuracySG `to` (Maybe Integer, a)
getSeqFnNormLog src a f =
  proc q ->
    do
    aq <- seqWithAccuracy a src -< q
    returnA -< (aux aq, aq)
  where
  aux aq =
    case ensureNoCN (f aq) of
      (Just faqNoCN, es) | not (hasCertainError es) ->
        case getNormLog faqNoCN of
          NormBits faqNL -> Just faqNL
          NormZero -> Nothing
      _ -> Nothing
