{-# LANGUAGE CPP #-}
-- #define DEBUG
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
  QAParA
  , executeQAParA, executeQAParAwithLog
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

import MixedTypesNumPrelude
-- import qualified Prelude as P
import Text.Printf

import Control.Arrow

import qualified Data.IntMap as IntMap

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class

import AERN2.QA.Protocol
import AERN2.QA.NetLog

data QANetState =
  QANetState
  {
    net_nextId :: ValueId
  , net_log :: QANetLog
  }

initQANetState :: QANetState
initQANetState =
    QANetState
    {
      net_nextId = ValueId 1
    , net_log = []
    }

getValueId :: QANetState -> [ValueId] -> String -> (QANetState, ValueId)
getValueId ns sources name =
  (ns2, vId)
  where
  ns2 =
    ns
    {
      net_nextId = succ vId
    , net_log = (net_log ns) ++ [logItem]
    }
  vId = net_nextId ns
  logItem = QANetLogCreate vId sources name

logQuery ::
  QANetState -> Maybe ValueId -> ValueId -> String -> QANetState
logQuery ns src valueId qS =
  ns { net_log = (net_log ns) ++ [logItem] }
  where
  logItem = QANetLogQuery src valueId qS

logAnswer ::
  QANetState -> Maybe ValueId -> ValueId -> (String, String) -> QANetState
logAnswer ns src valueId (aS, usedCacheS) =
  ns
  {
      net_log = (net_log ns) ++ [logItem]
  }
  where
  logItem = QANetLogAnswer src valueId usedCacheS aS

type QAParA = Kleisli QAParM

data QAParM a = QAParM { unQAParM :: Maybe (TVar QANetState) -> IO a }

instance Functor QAParM where
  -- fmap f (QAParM tv2ma) = QAParM (\nsTV -> fmap f (tv2ma nsTV))
  fmap f (QAParM tv2ma) = QAParM (fmap (fmap f) tv2ma)
instance Applicative QAParM where
  pure a = QAParM (pure . pure a)
  (QAParM tv2f) <*> (QAParM tv2a) =
    QAParM (\nsTV -> (tv2f nsTV <*> tv2a nsTV))
instance Monad QAParM where
  (QAParM tv2ma) >>= f =
    QAParM $ \nsTV -> tv2ma nsTV >>= ($ nsTV) . unQAParM . f
instance MonadIO QAParM where
  liftIO = QAParM . const

instance QAArrow QAParA where
  type QAId QAParA = ValueId
  qaRegister options = Kleisli qaRegisterM
    where
    isParallel = not (QARegPreferSerial `elem` options)
    qaRegisterM qa@(QA__ name Nothing sourceIds (p :: p) sampleQ _) =
      QAParM $ \m_nsTV ->
        do
        vId <- case m_nsTV of
          Nothing -> pure (ValueId 0)
          Just nsTV ->
            atomically $
              do
              ns <- readTVar nsTV
              let (ns2,i) = getValueId ns sourceIds name
              writeTVar nsTV ns2
              pure i
        activeQsTV <- atomically $ newTVar initActiveQs
        cacheTV <- atomically $ newTVar $ newQACache p
        return $ QA__ name (Just vId) [] p sampleQ (\me_src -> Kleisli $ makeQPar vId activeQsTV cacheTV me_src)
      where
      initActiveQs = IntMap.empty :: IntMap.IntMap (Q p)
      nextActiveQId activeQs
        | IntMap.null activeQs = int 1
        | otherwise =
            int $ 1 + (fst $ IntMap.findMax activeQs)
      makeQPar vId activeQsTV cacheTV (_, src) q =
        QAParM $ \m_nsTV ->
          do
          maybeTraceIO $ printf "[%s]: q = %s" name (show q)
          case m_nsTV of
            Nothing -> pure ()
            Just nsTV ->
              atomically $ do -- log query
                ns <- readTVar nsTV
                writeTVar nsTV $ logQuery ns src vId (show q)

          -- consult the cache and index of active queries in an atomic transaction:
          (maybeAnswer, mLogMsg, maybeComputeId) <- atomically $
            do
            cache <- readTVar cacheTV
            case lookupQACache p cache q of
              (Just a, mLogMsg) -> return (Just a, mLogMsg, Nothing)
              (_, mLogMsg) ->
                do
                activeQs <- readTVar activeQsTV
                let alreadyActive = or $ map (!>=! q) $ IntMap.elems activeQs
                if alreadyActive then return (Nothing, mLogMsg, Nothing) else
                  do
                  let computeId = nextActiveQId activeQs
                  writeTVar activeQsTV $ IntMap.insert computeId q activeQs
                  return (Nothing, mLogMsg, Just computeId)
          -- act based on the cache and actity consultation:
          case (maybeAnswer, maybeComputeId) of
            (Just a, _) -> -- got cached answer, just return it:
              pure $ promise mLogMsg (pure a)
            (_, Just computeId) ->
              -- no cached answer, no pending computation:
              do
              _ <- forkComputation m_nsTV computeId -- start a new computation
              pure $ promise mLogMsg waitForAnwer -- and wait for the answer
            _ -> -- no cached answer but there is a pending computation:
              pure $ promise mLogMsg waitForAnwer -- wait for a pending computation
        where
        promise mLogMsg answerIO =
          Kleisli $ const $ QAParM $ \m_nsTV ->
            case m_nsTV of
              Nothing -> answerIO
              Just nsTV -> do
                a <- answerIO
                atomically $ do
                  ns <- readTVar nsTV
                  writeTVar nsTV $ logAnswer ns src vId (show a,  logMsg)
                pure a
          where
          logMsg = case mLogMsg of Just m -> m; _ -> ""
        waitForAnwer = atomically $
          do
          cache <- readTVar cacheTV
          case lookupQACache p cache q of
            (Just a, _mLogMsg) -> return a
            (_, _mLogMsg) -> retry
        forkComputation nsTV computeId
          | isParallel = do { _ <- forkIO computation; return () }
          | otherwise = do { computation; return () }
          where
          computation =
            do
            -- compute an answer:
            a <- (unQAParM $ runKleisli (qaMakeQuery qa src) q) nsTV
            -- update the cache with this answer:
            atomically $ modifyTVar cacheTV (updateQACache p q a)
            -- remove computeId from active queries:
            atomically $ modifyTVar activeQsTV (IntMap.delete computeId)
    qaRegisterM _ =
      error "internal error in AERN2.QA.Strategy.Par: qaRegister called with an existing id"

  qaFulfilPromiseA = Kleisli qaFulfilPromiseM
    where
    qaFulfilPromiseM promiseA =
      runKleisli promiseA ()
  qaMakeQueryGetPromiseA src = Kleisli qaMakeQueryGetPromiseM
    where
    qaMakeQueryGetPromiseM (qa, q) =
      runKleisli (qaMakeQueryGetPromise qa (me, src)) q
      where
      me = case qaId qa of Nothing -> src; me2 -> me2

executeQAParA :: (QAParA () a) -> IO a
executeQAParA code =
  do
  (unQAParM $ runKleisli code ()) Nothing

executeQAParAwithLog :: (QAParA () a) -> IO (QANetLog, a)
executeQAParAwithLog code =
  do
  nsTV <- atomically $ newTVar initQANetState
  result <- (unQAParM $ runKleisli code ()) (Just nsTV)
  ns <- atomically $ readTVar nsTV
  return (net_log ns, result)
