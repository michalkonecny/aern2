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

    Helper functions for sequence operations
-}
module AERN2.Sequence.Helpers
(
  getSeqFnNormLog
  , seqElementSimilarToEncl
  , binaryWithEncl, binaryWithEnclTranslateAC
  , unaryOp, binaryOp, binaryOpWithPureArg
  , getInitQ1FromSimple, getInitQ1TFromSimple, getInitQ1Q2FromSimple
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

import Numeric.MixedTypes
-- import qualified Prelude as P

-- import Data.Convertible

import Control.Arrow

-- import Control.Lens hiding (op, (??))

-- import qualified Control.CollectErrors as CE

import AERN2.MP.Ball
-- import AERN2.MP.Precision
-- import AERN2.MP.Accuracy

import AERN2.QA.Protocol
import AERN2.AccuracySG
import AERN2.Sequence.Type

getSeqFnNormLog ::
  (QAArrow to, HasNorm (WithoutCN b), CanEnsureCN b)
  =>
  SequenceA to a ->
  (a -> b) ->
  AccuracySG `to` (CollectNumErrors NormLog, a)
getSeqFnNormLog r fn =
  proc q ->
    do
    b <- seqWithAccuracy r -< q
    returnA -< (fmap getNormLog (ensureCN $ fn b), b)

{- MPBall + CauchyReal = MPBall, only allowed in the (->) arrow  -}

seqElementSimilarToEncl ::
  (HasAccuracy b, HasPrecision b) =>
  (AccuracySG -> AccuracySG) ->
  b -> Sequence a -> a
seqElementSimilarToEncl accuracyTranslation b sa =
  sa ? (accuracyTranslation $ accuracySG $ getFiniteAccuracy b)

binaryWithEncl ::
  (HasAccuracy b, HasPrecision b, CanSetPrecision t)
  =>
  (a -> b -> t) -> Sequence a -> b -> t
binaryWithEncl = binaryWithEnclTranslateAC (const id)

binaryWithEnclTranslateAC ::
  (HasAccuracy b, HasPrecision b, CanSetPrecision t)
  =>
  (b -> AccuracySG -> AccuracySG) ->
  (a -> b -> t) -> Sequence a -> b -> t
binaryWithEnclTranslateAC accuracyTranslationForB op sa b =
  lowerPrecisionIfAbove (getPrecision b) $
    op (seqElementSimilarToEncl (accuracyTranslationForB b) b sa) b

{- generic implementations of operations of different arity -}

unaryOp ::
  (QAArrow to, SuitableForSeq a, SuitableForSeq b)
  =>
  String ->
  (a -> b) ->
  (SequenceA to a -> (AccuracySG `to` (AccuracySG, Maybe a))) ->
  SequenceA to a -> SequenceA to b
unaryOp name op getInitQ1 r1 =
  newSeq (op sampleA1) name [AnyProtocolQA r1] makeQ
  where
  SequenceP sampleA1 = qaProtocol r1
  makeQ =
    proc ac ->
      do
      q1Init <- getInitQ1 r1 -< ac
      ensureAccuracyA1 (r1 ?) op -< (ac, q1Init)

binaryOpWithPureArg ::
  (QAArrow to, SuitableForSeq a, SuitableForSeq b)
  =>
  String ->
  (a -> t -> b) ->
  (SequenceA to a -> t -> (AccuracySG `to` (AccuracySG, Maybe a))) ->
  SequenceA to a -> t -> SequenceA to b
binaryOpWithPureArg name op getInitQ1T r1 t =
  newSeq (op sampleA t) name [AnyProtocolQA r1] makeQ
  where
  SequenceP sampleA = qaProtocol r1
  makeQ =
    proc ac ->
      do
      q1Init <- getInitQ1T r1 t -< ac
      ensureAccuracyA1 (r1 ?) (flip op t) -< (ac, q1Init)

binaryOp ::
  (QAArrow to, SuitableForSeq a, SuitableForSeq b, SuitableForSeq c)
  =>
  String ->
  (a -> b -> c) ->
  (SequenceA to a -> SequenceA to b -> (AccuracySG `to` ((AccuracySG, Maybe a), (AccuracySG, Maybe b)))) ->
  SequenceA to a -> SequenceA to b -> SequenceA to c
binaryOp name op getInitQ1Q2 r1 r2 =
  newSeq (op sampleA sampleB) name [AnyProtocolQA r1, AnyProtocolQA r2] makeQ
  where
  SequenceP sampleA = qaProtocol r1
  SequenceP sampleB = qaProtocol r2
  makeQ =
    proc ac ->
      do
      (q1Init, q2Init) <- getInitQ1Q2 r1 r2 -< ac
      ensureAccuracyA2 ((r1,r2) ??) op -< (ac, q1Init, q2Init)

{- functions to help determine initial queries -}

getInitQ1FromSimple ::
  (Arrow to)
  =>
  AccuracySG `to` q ->
  r1 -> AccuracySG `to` (q, Maybe a)
getInitQ1FromSimple simpleA _ =
  proc q ->
    do
    initQ1 <- simpleA -< q
    returnA -< (initQ1, Nothing)

getInitQ1TFromSimple ::
  (Arrow to)
  =>
  AccuracySG `to` q ->
  r1 -> t -> AccuracySG `to` (q, Maybe a)
getInitQ1TFromSimple simpleA _ _ =
  proc q ->
    do
    initQ1 <- simpleA -< q
    returnA -< (initQ1, Nothing)

getInitQ1Q2FromSimple ::
  (Arrow to)
  =>
  AccuracySG `to` (q,q) ->
  r1 -> r2 -> AccuracySG `to` ((q, Maybe a), (q, Maybe b))
getInitQ1Q2FromSimple simpleA _ _ =
  proc q ->
    do
    (initQ1, initQ2) <- simpleA -< q
    returnA -< ((initQ1, Nothing), (initQ2, Nothing))

{-
  functions for iterative querying of operands
  until the result is of a sufficient accuracy
-}

ensureAccuracyA1 ::
  (ArrowChoice to, HasAccuracy b, Show a1, Show b)
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
            if getAccuracy result >= _acStrict q
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
  (ArrowChoice to, HasAccuracy b, Show a1, Show a2, Show b)
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
            if getAccuracy result >= _acStrict q
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
