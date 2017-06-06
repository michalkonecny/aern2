{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.CauchySeq.Helpers
    Description :  helper functions for Cauchy sequence operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Auxiliary functions for Cauchy sequence operations
-}
module AERN2.CauchySeq.Helpers
-- (
--   getCRFnNormLog
--   , binaryWithBall
--   , binaryWithDouble
--   , unaryOp, binaryOp, binaryOpWithPureArg
--   , getInitQ1FromSimple, getInitQ1TFromSimple, getInitQ1Q2FromSimple
-- )
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

import Data.Convertible

import Control.Arrow

import Control.Lens hiding (op, (??))

import qualified Control.CollectErrors as CE

import AERN2.MP.Ball
-- import AERN2.MP.Precision
-- import AERN2.MP.Accuracy

import AERN2.QA.Protocol
import AERN2.AccuracySG
import AERN2.CauchySeq.Type

getCRFnNormLog ::
  (QAArrow to, HasNorm (WithoutCN a), CanEnsureCN a)
  =>
  CauchySeqA to a ->
  (a -> a) ->
  AccuracySG `to` (CollectNumErrors NormLog, a)
getCRFnNormLog r fn =
  proc q ->
    do
    b <- seqWithAccuracy r -< q
    returnA -< (fmap getNormLog (ensureCN $ fn b), b)

{- MPBall + CauchyReal = MPBall, only allowed in the (->) arrow  -}

-- mpBallSimilarTo :: MPBall -> CauchyReal -> CollectNumErrors MPBall
-- mpBallSimilarTo b r =
--   r ? (accuracySG $ getFiniteAccuracy b)
--
-- binaryWithBall ::
--   (CanEnsureCN t, CanSetPrecision (EnsureCN t))
--   =>
--   (MPBall -> MPBall -> t) -> CauchyReal -> MPBall -> EnsureCN t
-- binaryWithBall op r b =
--   lowerPrecisionIfAbove (getPrecision b) res
--   where
--   res =
--     CE.lift2ensureCE op (mpBallSimilarTo b r) (cn b)
--
-- instance Convertible CauchyReal Double where
--   safeConvert r =
--     do
--     b <- CE.getConvertResult (r ? (bitsS 53))
--     safeConvert (centre b)
--
-- binaryWithDouble :: (Double -> Double -> Double) -> CauchyReal -> Double -> Double
-- binaryWithDouble op r d =
--   op (convert r) d
--
-- {- generic implementations of operations of different arity -}
--
-- unaryOp ::
--   (QAArrow to)
--   =>
--   String ->
--   (CollectNumErrors MPBall -> CollectNumErrors MPBall) ->
--   (CauchySeqA to a -> (AccuracySG `to` (AccuracySG, Maybe MPBall))) ->
--   CauchySeqA to a -> CauchySeqA to a
-- unaryOp name op getInitQ1 r1 =
--   newCR name [AnyProtocolQA r1] makeQ
--   where
--   makeQ =
--     proc ac ->
--       do
--       q1InitMB <- getInitQ1 r1 -< ac
--       ensureAccuracyA1 (r1 ?) op -< (ac, q1InitMB)
--
-- binaryOpWithPureArg ::
--   (QAArrow to)
--   =>
--   String ->
--   (CollectNumErrors MPBall -> t -> CollectNumErrors MPBall) ->
--   (CauchySeqA to a -> t -> (AccuracySG `to` (AccuracySG, Maybe MPBall))) ->
--   CauchySeqA to a -> t -> CauchySeqA to a
-- binaryOpWithPureArg name op getInitQ1T r1 t =
--   newCR name [AnyProtocolQA r1] makeQ
--   where
--   makeQ =
--     proc ac ->
--       do
--       q1InitMB <- getInitQ1T r1 t -< ac
--       ensureAccuracyA1 (r1 ?) (flip op t) -< (ac, q1InitMB)
--
-- binaryOp ::
--   (QAArrow to)
--   =>
--   String ->
--   (CollectNumErrors MPBall -> CollectNumErrors MPBall -> CollectNumErrors MPBall) ->
--   (CauchySeqA to a -> CauchySeqA to a -> (AccuracySG `to` ((AccuracySG, Maybe MPBall), (AccuracySG, Maybe MPBall)))) ->
--   CauchySeqA to a -> CauchySeqA to a -> CauchySeqA to a
-- binaryOp name op getInitQ1Q2 r1 r2 =
--   newCR name [AnyProtocolQA r1, AnyProtocolQA r2] makeQ
--   where
--   makeQ =
--     proc ac ->
--       do
--       (q1InitMB, q2InitMB) <- getInitQ1Q2 r1 r2 -< ac
--       ensureAccuracyA2 ((r1,r2) ??) op -< (ac, q1InitMB, q2InitMB)
--
-- {- functions to help determine initial queries -}
--
-- getInitQ1FromSimple ::
--   (Arrow to)
--   =>
--   AccuracySG `to` q ->
--   r1 -> AccuracySG `to` (q, Maybe MPBall)
-- getInitQ1FromSimple simpleA _ =
--   proc q ->
--     do
--     initQ1 <- simpleA -< q
--     returnA -< (initQ1, Nothing)
--
-- getInitQ1TFromSimple ::
--   (Arrow to)
--   =>
--   AccuracySG `to` q ->
--   r1 -> t -> AccuracySG `to` (q, Maybe MPBall)
-- getInitQ1TFromSimple simpleA _ _ =
--   proc q ->
--     do
--     initQ1 <- simpleA -< q
--     returnA -< (initQ1, Nothing)
--
-- getInitQ1Q2FromSimple ::
--   (Arrow to)
--   =>
--   AccuracySG `to` (q,q) ->
--   r1 -> r2 -> AccuracySG `to` ((q, Maybe MPBall), (q, Maybe MPBall))
-- getInitQ1Q2FromSimple simpleA _ _ =
--   proc q ->
--     do
--     (initQ1, initQ2) <- simpleA -< q
--     returnA -< ((initQ1, Nothing), (initQ2, Nothing))
--
-- {-
--   functions for iterative querying of operands
--   until the result is of a sufficient accuracy
-- -}
--
-- ensureAccuracyA1 ::
--   (ArrowChoice to) =>
--   (AccuracySG `to` MPBall) ->
--   (CollectNumErrors MPBall -> CollectNumErrors MPBall) ->
--   ((AccuracySG, (AccuracySG, Maybe MPBall)) `to` MPBall)
-- ensureAccuracyA1 getA1 op =
--     proc (q,(j1, mB)) ->
--         do
--         let mResult = fmap op (fmap catchingNumExceptions mB)
--         case mResult ^? _Just . numEXC_maybeValue of
--             Just (Just result) | getAccuracy result >= q ->
--                 returnA -<
--                     maybeTrace (
--                         "ensureAccuracy1: Pre-computed result sufficient. (q = " ++ show q ++
--                         "; j1 = " ++ show j1 ++
--                         "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
--                     ) $
--                     result
--             _ -> aux -< (q,j1)
--     where
--     aux =
--         proc (q,j1) ->
--             do
--             a1 <- getA1 -< j1
--             let resultCE = op (catchingNumExceptions a1)
--             case resultCE ^. numEXC_maybeValue of
--               Just result | getAccuracy result >= _acStrict q ->
--                 returnA -<
--                     maybeTrace (
--                         "ensureAccuracy1: Succeeded. (q = " ++ show q ++
--                         "; j1 = " ++ show j1 ++
--                         "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
--                     ) $
--                     result
--               _ ->
--                 aux -<
--                     maybeTrace (
--                         "ensureAccuracy1: Not enough ... (q = " ++ show q ++
--                         "; j1 = " ++ show j1 ++
--                         "; a1 = " ++ show a1 ++
--                         "; result = " ++ show resultCE ++
--                         "; result accuracy = " ++ (show $ getAccuracy resultCE) ++ ")"
--                     ) $
--                     (q, j1+1)
--
-- ensureAccuracyA2 ::
--   (ArrowChoice to) =>
--   ((AccuracySG, AccuracySG) `to` (MPBall, MPBall)) ->
--   (CollectNumErrors MPBall -> CollectNumErrors MPBall -> CollectNumErrors MPBall) ->
--   ((AccuracySG, (AccuracySG, Maybe MPBall), (AccuracySG, Maybe MPBall)) `to` MPBall)
-- ensureAccuracyA2 getA12 op =
--     proc (q,(j1, mB1),(j2, mB2)) ->
--         do
--         let mResult =
--               do b1 <- mB1; b2 <- mB2;
--                   Just $ op (catchingNumExceptions b1) (catchingNumExceptions b2)
--         case (mResult ^? _Just . numEXC_maybeValue) of
--             (Just (Just result)) | getAccuracy result >= q ->
--                 returnA -<
--                     maybeTrace (
--                         "ensureAccuracy2: Pre-computed result sufficient. (q = " ++ show q ++
--                         "; j1 = " ++ show j1 ++
--                         "; j2 = " ++ show j2 ++
--                         "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
--                     ) $
--                     result
--             _ -> aux -< (q,j1,j2)
--     where
--     aux =
--         proc (q, j1, j2) ->
--             do
--             (a1,a2) <- getA12 -< (j1, j2)
--             let resultCE = op (catchingNumExceptions a1) (catchingNumExceptions a2)
--             case resultCE ^. numEXC_maybeValue of
--               Just result | getAccuracy result >= _acStrict q ->
--                   returnA -<
--                     maybeTrace (
--                         "ensureAccuracy2: Succeeded. (q = " ++ show q ++
--                         "; j1 = " ++ show j1 ++
--                         "; j2 = " ++ show j2 ++
--                         "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
--                     ) $
--                     result
--               _ -> aux -<
--                     maybeTrace (
--                         "ensureAccuracy2: Not enough ... (q = " ++ show q ++
--                         "; a1 = " ++ show a1 ++
--                         "; getPrecision a1 = " ++ show (getPrecision a1) ++
--                         "; j1 = " ++ show j1 ++
--                         "; a2 = " ++ show a2 ++
--                         "; getPrecision a2 = " ++ show (getPrecision a2) ++
--                         "; j2 = " ++ show j2 ++
--                         "; result = " ++ (show $ resultCE) ++
--                         "; result accuracy = " ++ (show $ getAccuracy resultCE) ++ ")"
--                     ) $
--                     (q,j1+1,j2+1)
