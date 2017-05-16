{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
#define DEBUG
{-|
    Module      :  AERN2.QA.Strategy.Parallel
    Description :  QA net parallel evaluation
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    QA net parallel evaluation
-}
module AERN2.QA.Strategy.Parallel
(
  module AERN2.QA.NetLog
  , module AERN2.QA.NetState
  , QAParA
  , executeQAParA, executeQAParUncachedA
)
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO  (\ (_ :: String)-> return ())
#endif

import Numeric.MixedTypes
-- import qualified Prelude as P
import Text.Printf

import Control.Arrow

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class

import AERN2.QA.Protocol
import AERN2.QA.NetLog
import AERN2.QA.NetState

type QAParA = Kleisli QAParM

data QAParM a = QAParM { unQAParM :: QANetStateTV -> IO a }

type QANetStateTV = TVar (QANetState QAParM)

instance Functor QAParM where
  fmap f (QAParM tv2ma) = QAParM (fmap f . tv2ma)
instance Applicative QAParM where
  pure a = QAParM (const $ pure a)
  (QAParM tv2f) <*> (QAParM tv2a) = QAParM (\ tv -> tv2f tv <*> tv2a tv)
instance Monad QAParM where
  (QAParM tv2ma) >>= f =
    QAParM $ \tv -> tv2ma tv >>= ($ tv) . unQAParM . f
instance MonadIO QAParM where
  liftIO ma = QAParM (const ma)

instance QAArrow QAParA where
  type QAId QAParA = ValueId
  qaRegister = Kleisli qaRegisterM
    where
    qaRegisterM (QA__ name Nothing sourceIds p sampleQ (Kleisli q2a)) =
      do
      valueId <- newId
      return $ QA__ name (Just valueId) [] p sampleQ (Kleisli $ makeQPar valueId)
      where
      newId =
        maybeTrace ("newId: " ++ show name) $
        QAParM $ \ nsTV -> atomically $
          do
          ns <- readTVar nsTV
          let (i, ns') = insertNode p name sourceIds q2a ns
          writeTVar nsTV ns'
          return i
      makeQPar valueId q =
        QAParM $ \nsTV ->
          do
          maybeTraceIO $ printf "[%s]: q = %s" (show valueId) (show q)
          ns' <- atomically $
            do
            ns <- readTVar nsTV
            let ns' = logQuery ns valueId (show q)
            writeTVar nsTV ns'
            return ns'
          (a, usedCache, cache') <- ($ nsTV) $ unQAParM $ getAnswer ns' p valueId q
          atomically $
            do
            ns2 <- readTVar nsTV
            let ns2' = logAnswerUpdateCache ns2 p valueId (show a, usedCache, cache')
            writeTVar nsTV ns2'
          maybeTraceIO $ printf "[%s]: a = %s" (show valueId) (show a)
          return a
    qaRegisterM _ =
      error "internal error in AERN2.QA.Strategy.Par: qaRegister called with an existing id"

  type QAPromise QAParA = TMVar
  qaMakeQueryGetPromiseA = Kleisli qaMakeQueryGetPromiseM
    where
    qaMakeQueryGetPromiseM (qa, q) =
      QAParM $ \ nsTV ->
        do
        aTMV <- atomically newEmptyTMVar
        _ <- forkIO $ computeAnswer nsTV aTMV
        return aTMV
        where
        computeAnswer nsTV aTMV =
          do
          a <- unQAParM (runKleisli (qaMakeQuery qa) q) nsTV
          atomically $ putTMVar aTMV a
  qaFulfilPromiseA = Kleisli qaFulfilPromiseM
    where
    qaFulfilPromiseM aTMV =
      QAParM $ \ _nsTV ->
        atomically $ takeTMVar aTMV
--
executeQAParA :: (QAParA () a) -> IO (QANetLog, a)
executeQAParA code =
  do
  nsTV <- atomically $ newTVar (initQANetState True)
  result <- ($ nsTV) $ unQAParM $ runKleisli code ()
  ns <- atomically $ readTVar nsTV
  pure $ (net_log ns, result)
--
executeQAParUncachedA :: (QAParA () a) -> IO (QANetLog, a)
executeQAParUncachedA code =
  do
  nsTV <- atomically $ newTVar (initQANetState False)
  result <- ($ nsTV) $ unQAParM $ runKleisli code ()
  ns <- atomically $ readTVar nsTV
  pure $ (net_log ns, result)
