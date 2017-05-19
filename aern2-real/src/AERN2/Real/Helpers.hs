{-|
    Module      :  AERN2.Real.Helpers
    Description :  helper functions for CR operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Auxiliary functions for CR operations
-}
module AERN2.Real.Helpers
(
  getCRFnNormLog
  , binaryWithBall
  , binaryWithDouble
  , unaryOp, binaryOp, binaryOpWithPureArg
  , getInitQ1FromSimple, getInitQ1TFromSimple, getInitQ1Q2FromSimple
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

import Data.Convertible

import Control.Arrow

import Control.Lens hiding (op, (??))

import Numeric.CatchingExceptions

import AERN2.MP.Ball
-- import AERN2.MP.Precision
-- import AERN2.MP.Accuracy

import AERN2.QA.Protocol
import AERN2.AccuracySG
import AERN2.Real.Type

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

getCRFnNormLog ::
  (QAArrow to)
  =>
  CauchyRealA to ->
  (MPBall -> MPBall) ->
  AccuracySG `to` (NormLog, MPBall)
getCRFnNormLog r fn =
  proc q ->
    do
    b <- realWithAccuracy r -< q
    returnA -< (getNormLog (fn b), b)

{- MPBall + CauchyReal = MPBall, only allowed in the (->) arrow  -}

mpBallSimilarTo :: MPBall -> CauchyReal -> MPBall
mpBallSimilarTo b r =
  r ? (accuracySG $ getFiniteAccuracy b)

binaryWithBall :: (MPBall -> MPBall -> MPBall) -> CauchyReal -> MPBall -> MPBall
binaryWithBall op r b =
  lowerPrecisionIfAbove (getPrecision b) $
    op (mpBallSimilarTo b r) b

instance Convertible CauchyReal Double where
  safeConvert = safeConvert . centre . (? (bitsS 53))

binaryWithDouble :: (Double -> Double -> Double) -> CauchyReal -> Double -> Double
binaryWithDouble op r d =
  op (convert r) d

{- generic implementations of operations of different arity -}

unaryOp ::
  (QAArrow to)
  =>
  String ->
  (CatchingNumExceptions MPBall -> CatchingNumExceptions MPBall) ->
  (CauchyRealA to -> (AccuracySG `to` (AccuracySG, Maybe MPBall))) ->
  CauchyRealA to -> CauchyRealA to
unaryOp name op getInitQ1 r1 =
  newCR name [AnyProtocolQA r1] makeQ
  where
  makeQ =
    proc ac ->
      do
      q1InitMB <- getInitQ1 r1 -< ac
      ensureAccuracyA1 (r1 ?) op -< (ac, q1InitMB)

binaryOpWithPureArg ::
  (QAArrow to)
  =>
  String ->
  (CatchingNumExceptions MPBall -> t -> CatchingNumExceptions MPBall) ->
  (CauchyRealA to -> t -> (AccuracySG `to` (AccuracySG, Maybe MPBall))) ->
  CauchyRealA to -> t -> CauchyRealA to
binaryOpWithPureArg name op getInitQ1T r1 t =
  newCR name [AnyProtocolQA r1] makeQ
  where
  makeQ =
    proc ac ->
      do
      q1InitMB <- getInitQ1T r1 t -< ac
      ensureAccuracyA1 (r1 ?) (flip op t) -< (ac, q1InitMB)

binaryOp ::
  (QAArrow to)
  =>
  String ->
  (CatchingNumExceptions MPBall -> CatchingNumExceptions MPBall -> CatchingNumExceptions MPBall) ->
  (CauchyRealA to -> CauchyRealA to -> (AccuracySG `to` ((AccuracySG, Maybe MPBall), (AccuracySG, Maybe MPBall)))) ->
  CauchyRealA to -> CauchyRealA to -> CauchyRealA to
binaryOp name op getInitQ1Q2 r1 r2 =
  newCR name [AnyProtocolQA r1, AnyProtocolQA r2] makeQ
  where
  makeQ =
    proc ac ->
      do
      (q1InitMB, q2InitMB) <- getInitQ1Q2 r1 r2 -< ac
      ensureAccuracyA2 ((r1,r2) ??) op -< (ac, q1InitMB, q2InitMB)

{- functions to help determine initial queries -}

getInitQ1FromSimple ::
  (Arrow to)
  =>
  AccuracySG `to` q ->
  r1 -> AccuracySG `to` (q, Maybe MPBall)
getInitQ1FromSimple simpleA _ =
  proc q ->
    do
    initQ1 <- simpleA -< q
    returnA -< (initQ1, Nothing)

getInitQ1TFromSimple ::
  (Arrow to)
  =>
  AccuracySG `to` q ->
  r1 -> t -> AccuracySG `to` (q, Maybe MPBall)
getInitQ1TFromSimple simpleA _ _ =
  proc q ->
    do
    initQ1 <- simpleA -< q
    returnA -< (initQ1, Nothing)

getInitQ1Q2FromSimple ::
  (Arrow to)
  =>
  AccuracySG `to` (q,q) ->
  r1 -> r2 -> AccuracySG `to` ((q, Maybe MPBall), (q, Maybe MPBall))
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
  (ArrowChoice to) =>
  (AccuracySG `to` MPBall) ->
  (CatchingNumExceptions MPBall -> CatchingNumExceptions MPBall) ->
  ((AccuracySG, (AccuracySG, Maybe MPBall)) `to` MPBall)
ensureAccuracyA1 getA1 op =
    proc (q,(j1, mB)) ->
        do
        let mResult = fmap op (fmap catchingNumExceptions mB)
        case mResult ^? _Just . numEXC_maybeValue of
            Just (Just result) | getAccuracy result >= q ->
                returnA -<
                    maybeTrace (
                        "ensureAccuracy1: Pre-computed result sufficient. (q = " ++ show q ++
                        "; j1 = " ++ show j1 ++
                        "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                    ) $
                    result
            _ -> aux -< (q,j1)
    where
    aux =
        proc (q,j1) ->
            do
            a1 <- getA1 -< j1
            let resultCE = op (catchingNumExceptions a1)
            case resultCE ^. numEXC_maybeValue of
              Just result | getAccuracy result >= _acStrict q ->
                returnA -<
                    maybeTrace (
                        "ensureAccuracy1: Succeeded. (q = " ++ show q ++
                        "; j1 = " ++ show j1 ++
                        "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                    ) $
                    result
              _ ->
                aux -<
                    maybeTrace (
                        "ensureAccuracy1: Not enough ... (q = " ++ show q ++
                        "; j1 = " ++ show j1 ++
                        "; a1 = " ++ show a1 ++
                        "; result = " ++ show resultCE ++
                        "; result accuracy = " ++ (show $ getAccuracy resultCE) ++ ")"
                    ) $
                    (q, j1+1)

ensureAccuracyA2 ::
  (ArrowChoice to) =>
  ((AccuracySG, AccuracySG) `to` (MPBall, MPBall)) ->
  (CatchingNumExceptions MPBall -> CatchingNumExceptions MPBall -> CatchingNumExceptions MPBall) ->
  ((AccuracySG, (AccuracySG, Maybe MPBall), (AccuracySG, Maybe MPBall)) `to` MPBall)
ensureAccuracyA2 getA12 op =
    proc (q,(j1, mB1),(j2, mB2)) ->
        do
        let mResult =
              do b1 <- mB1; b2 <- mB2;
                  Just $ op (catchingNumExceptions b1) (catchingNumExceptions b2)
        case (mResult ^? _Just . numEXC_maybeValue) of
            (Just (Just result)) | getAccuracy result >= q ->
                returnA -<
                    maybeTrace (
                        "ensureAccuracy2: Pre-computed result sufficient. (q = " ++ show q ++
                        "; j1 = " ++ show j1 ++
                        "; j2 = " ++ show j2 ++
                        "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                    ) $
                    result
            _ -> aux -< (q,j1,j2)
    where
    aux =
        proc (q, j1, j2) ->
            do
            (a1,a2) <- getA12 -< (j1, j2)
            let resultCE = op (catchingNumExceptions a1) (catchingNumExceptions a2)
            case resultCE ^. numEXC_maybeValue of
              Just result | getAccuracy result >= _acStrict q ->
                  returnA -<
                    maybeTrace (
                        "ensureAccuracy2: Succeeded. (q = " ++ show q ++
                        "; j1 = " ++ show j1 ++
                        "; j2 = " ++ show j2 ++
                        "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                    ) $
                    result
              _ -> aux -<
                    maybeTrace (
                        "ensureAccuracy2: Not enough ... (q = " ++ show q ++
                        "; a1 = " ++ show a1 ++
                        "; getPrecision a1 = " ++ show (getPrecision a1) ++
                        "; j1 = " ++ show j1 ++
                        "; a2 = " ++ show a2 ++
                        "; getPrecision a2 = " ++ show (getPrecision a2) ++
                        "; j2 = " ++ show j2 ++
                        "; result = " ++ (show $ resultCE) ++
                        "; result accuracy = " ++ (show $ getAccuracy resultCE) ++ ")"
                    ) $
                    (q,j1+1,j2+1)
