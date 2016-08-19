{-|
    Module      :  AERN2.Real.Type
    Description :  Cauchy real numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Cauchy real numbers
-}
module AERN2.Real.Type
(
  QAProtocol(..), QA(..)
  , QAArrow(..)
  , CauchyRealP, pCR
  , CauchyRealA, CauchyReal, cr
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

import Control.Arrow

import AERN2.MP.Ball
-- import AERN2.MP.Precision
-- import AERN2.MP.Accuracy

import AERN2.QA

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace
    | shouldTrace = trace
    | otherwise = const id

_dummy :: ()
_dummy = maybeTrace "dummy" ()

data CauchyRealP = CauchyRealP deriving (Show)

instance QAProtocol CauchyRealP where
  type Q CauchyRealP = Accuracy
  type A CauchyRealP = MPBall
  -- sampleQ _ = NoInformation

pCR :: CauchyRealP
pCR = CauchyRealP

instance QAProtocolCacheable CauchyRealP where
  type QACache CauchyRealP = Maybe MPBall
  newQACache _ = Nothing
  lookupQACache _ cache ac =
    case cache of
      Just b | getAccuracy b >= ac -> Just b
      _ -> Nothing
  updateQACache _ Nothing _ b = Just b
  updateQACache _ (Just b1) _ b2 = Just (b1 `intersect` b2)

type CauchyRealA to = QA to CauchyRealP

type CauchyReal = CauchyRealA (->)

cr :: String -> Accuracy `to` MPBall -> CauchyRealA to
cr name makeQ = qa name pCR NoInformation makeQ

{- TODO: expand and move the following to separate modules -}

binaryOp ::
  (ArrowChoice to)
  =>
  String ->
  (MPBall -> MPBall -> MPBall) ->
  (CauchyRealA to -> CauchyRealA to -> (Accuracy `to` ((Accuracy, Maybe MPBall), (Accuracy, Maybe MPBall)))) ->
  CauchyRealA to -> CauchyRealA to -> CauchyRealA to
binaryOp name op getInitQ1Q2 r1 r2 = cr name makeQ
  where
  makeQ =
    proc ac ->
      do
      (q1InitMB, q2InitMB) <- getInitQ1Q2 r1 r2 -< ac
      ensureAccuracyA2 (qaMakeQuery r1) (qaMakeQuery r2) op -< (ac, q1InitMB, q2InitMB)

ensureAccuracyA2 ::
  (ArrowChoice to) =>
  (Accuracy `to` MPBall) ->
  (Accuracy `to` MPBall) ->
  (MPBall -> MPBall -> MPBall) ->
  ((Accuracy, (Accuracy, Maybe MPBall), (Accuracy, Maybe MPBall)) `to` MPBall)
ensureAccuracyA2 getA1 getA2 op =
    proc (q,(j1, mB1),(j2, mB2)) ->
        do
        let mResult = do b1 <- mB1; b2 <- mB2; Just $ op b1 b2
        case mResult of
            Just result | getAccuracy result >= q ->
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
            a1 <- getA1 -< j1
            a2 <- getA2 -< j2
            let result = op a1 a2
            if getAccuracy result >= q
                then returnA -<
                    maybeTrace (
                        "ensureAccuracy2: Succeeded. (q = " ++ show q ++
                        "; j1 = " ++ show j1 ++
                        "; j2 = " ++ show j2 ++
                        "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                    ) $
                    result
                else aux -<
                    maybeTrace (
                        "ensureAccuracy2: Not enough ... (q = " ++ show q ++
                        "; a1 = " ++ show a1 ++
                        "; getPrecision a1 = " ++ show (getPrecision a1) ++
                        "; j1 = " ++ show j1 ++
                        "; a2 = " ++ show a2 ++
                        "; getPrecision a2 = " ++ show (getPrecision a2) ++
                        "; j2 = " ++ show j2 ++
                        "; result = " ++ (show $ result) ++
                        "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                    ) $
                    (q,j1+1,j2+1)

getInitQ1Q2FromSimple ::
  (Arrow to)
  =>
  Accuracy `to` (q,q) ->
  r1 -> r2 -> Accuracy `to` ((q, Maybe MPBall), (q, Maybe MPBall))
getInitQ1Q2FromSimple simpleA _ _ =
  proc q ->
    do
    (initQ1, initQ2) <- simpleA -< q
    returnA -< ((initQ1, Nothing), (initQ2, Nothing))

instance (QAArrow to) => CanAddAsymmetric (CauchyRealA to) (CauchyRealA to) where
  type AddType (CauchyRealA to) (CauchyRealA to) = (CauchyRealA to)
  add = binaryOp "+" add (getInitQ1Q2FromSimple $ proc q -> returnA -< (q,q))

{-|
  Example arrow-generic real number computation
-}
_addA :: (QAArrow to) => (CauchyRealA to, CauchyRealA to) `to` CauchyRealA to
_addA =
  -- using -XArrows syntax:
  proc (x,y) -> do
    s <-(-:-)-< x + y //.. [x,y] -- listing source values for better tracing messages
      -- -:- and //.. are shorcuts for qaRegister and (,) respectively
    returnA -< s

_CRonePure :: CauchyReal
_CRonePure = cr "one" (\ _ac -> mpBall 1)

_addApure :: CauchyReal
_addApure = _addA (_CRonePure, _CRonePure)

_CRoneCached :: CauchyRealA QACachedA
_CRoneCached = cr "one" (Kleisli $ \ _ac -> return $ mpBall 1)

_addAcached :: QACachedA () (CauchyRealA QACachedA)
_addAcached =
  proc () ->
    do
    xReg <-(-:-)-< _CRoneCached //.. []
    _addA -< (xReg,xReg)
    -- returnA -< xR + xR

_addAcachedPrint :: IO ()
_addAcachedPrint =
  printQANetLogThenResult $ executeQACachedA $
    proc () ->
      do
      x <-_addAcached -< ()
      qaMakeQueryA -< (x, bits 10)
