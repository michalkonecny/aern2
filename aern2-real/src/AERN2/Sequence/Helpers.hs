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
      q1Init <- getInitQ1 me r1 -< ac
      ensureAccuracyA1 (r1 ?<- me) op -< (ac, q1Init)

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
      q1Init <- getInitQ1T me r1 t -< ac
      ensureAccuracyA1 (r1 ?<- me) (flip op t) -< (ac, q1Init)

binaryOp ::
  (QAArrow to, SuitableForSeq a, SuitableForSeq b, SuitableForSeq c)
  =>
  String ->
  (a -> b -> c) ->
  (Maybe (QAId to) {-^ my id -} -> SequenceA to a -> SequenceA to b -> (AccuracySG `to` ((AccuracySG, Maybe a), (AccuracySG, Maybe b)))) ->
  SequenceA to a -> SequenceA to b -> SequenceA to c
binaryOp name op getInitQ1Q2 r1 r2 =
  newSeq (op sampleA sampleB) name [AnyProtocolQA r1, AnyProtocolQA r2] makeQ
  where
  SequenceP sampleA = qaProtocol r1
  SequenceP sampleB = qaProtocol r2
  makeQ (me,_src) =
    proc ac ->
      do
      (q1Init, q2Init) <- getInitQ1Q2 me r1 r2 -< ac
      ensureAccuracyA2 ((r1,r2) ??<- me) op -< (ac, q1Init, q2Init)

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

ensureAccuracyA1 ::
  (ArrowChoice to, Show a1, Show b
  , HasAccuracy b
  , CanEnsureCN b, HasAccuracy (EnsureNoCN b))
  =>
  (AccuracySG `to` a1) ->
  (a1 -> b) ->
  ((AccuracySG, (AccuracySG, Maybe a1)) `to` b)
ensureAccuracyA1 getA1 op =
    proc (q,(j1, a1Prelim)) ->
        case fmap op a1Prelim of
          Just resultPrelim | getAccuracy resultPrelim >= q ->
            returnA -<
                maybeTrace (
                    "ensureAccuracy1: Pre-computed result sufficient. (q = " ++ show q ++
                    "; j1 = " ++ show j1 ++
                    "; result accuracy = " ++ (show $ getAccuracy resultPrelim) ++ ")"
                ) $
                resultPrelim
          _ ->
            aux -< (q,j1)
    where
    aux =
        proc (q,j1) ->
            do
            a1 <- getA1 -< j1
            let result = op a1
            case ensureNoCN result of
              Left _ -> returnA -< result -- errors, give up improving
              Right resultNoCN ->
                if getAccuracy resultNoCN >= _acStrict q
                  then
                  returnA -<
                      maybeTrace (
                          "ensureAccuracy1: Succeeded. (q = " ++ show q ++
                          "; j1 = " ++ show j1 ++
                          "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                      ) $
                      result
                  else
                  aux -<
                      maybeTrace (
                          "ensureAccuracy1: Not enough ... (q = " ++ show q ++
                          "; j1 = " ++ show j1 ++
                          "; a1 = " ++ show a1 ++
                          "; result = " ++ show result ++
                          "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                      ) $
                      (q, j1+1)

ensureAccuracyA2 ::
  (ArrowChoice to, Show a1, Show a2, Show b
  , HasAccuracy b
  , CanEnsureCN b, HasAccuracy (EnsureNoCN b))
  =>
  ((AccuracySG, AccuracySG) `to` (a1,a2)) ->
  (a1 -> a2 -> b) ->
  ((AccuracySG, (AccuracySG, Maybe a1), (AccuracySG, Maybe a2)) `to` b)
ensureAccuracyA2 getA12 op =
    proc (q,(j1, a1Prelim),(j2, a2Prelim)) ->
        let resultP = do a1P <- a1Prelim; a2P <- a2Prelim; Just (op a1P a2P) in
        case resultP of
          Just resultPrelim | getAccuracy resultPrelim >= q ->
            returnA -<
                maybeTrace (
                    "ensureAccuracy1: Pre-computed result sufficient. (q = " ++ show q ++
                    "; j1 = " ++ show j1 ++
                    "; result accuracy = " ++ (show $ getAccuracy resultPrelim) ++ ")"
                ) $
                resultPrelim
          _ ->
            aux -< (q,j1,j2)
    where
    aux =
        proc (q,j1,j2) ->
            do
            (a1, a2) <- getA12 -< (j1, j2)
            let result = op a1 a2
            case ensureNoCN result of
              Left _ -> returnA -< result -- errors, give up improving
              Right resultNoCN ->
                if getAccuracy resultNoCN >= _acStrict q
                  then
                    returnA -<
                        maybeTrace (
                            "ensureAccuracy2: Succeeded. (q = " ++ show q ++
                            "; j1 = " ++ show j1 ++
                            "; j2 = " ++ show j2 ++
                            "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                        ) $
                        result
                  else
                    aux -<
                        maybeTrace (
                            "ensureAccuracy2: Not enough ... (q = " ++ show q ++
                            "; j1 = " ++ show j1 ++
                            "; a1 = " ++ show a1 ++
                            "; j2 = " ++ show j2 ++
                            "; a2 = " ++ show a2 ++
                            "; result = " ++ show result ++
                            "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                        ) $
                        (q, j1+1, j2+1)

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
      Left _ -> Nothing
      Right faqNoCN ->
        case getNormLog faqNoCN of
          NormBits faqNL -> Just faqNL
          NormZero -> Nothing
