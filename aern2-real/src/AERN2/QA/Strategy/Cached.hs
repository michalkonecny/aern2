{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.QA.Strategy.Cached
    Description :  QA net evaluation with answer caching
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    QA net evaluation with answer caching
-}
module AERN2.QA.Strategy.Cached
(
  QACachedAId, executeQACachedAId
  , QACachedA, QANetInfo(..), initQANetInfo
  , QAComputation(..), AnyQAComputation(..)
  , executeQACachedA, printQANetLogThenResult
  , formatQALog, printQALog
)
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (\ (_ :: String) t -> t)
#endif

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

import Unsafe.Coerce

import Control.Arrow

import Data.Functor.Identity

-- import Data.Maybe
-- import Data.List
import qualified Data.Map as Map

import Control.Monad.Trans.State

import AERN2.QA.Protocol
import AERN2.QA.NetLog

type QACachedAId = QACachedA Identity

type QACachedA m = Kleisli (QACachedM m)

instance (Monad m) => QARegArrow (QACachedA m) where
  type QAId (QACachedA m) = ValueId
  qaRegister = Kleisli qaRegisterM
    where
    qaRegisterM (x@(QA__ name _ sourceIds p sampleQ _)) =
      do
      xId <- newId x sourceIds
      return $ QA__ name (Just xId) [] p sampleQ (Kleisli $ makeQCached xId)
      where
      makeQCached = getAnswer p
      -- sourceIds = catMaybes $ map anyPqaId sources

instance QAArrow QACachedAId where
  type QAPromise QACachedAId = Identity
  qaMakeQueryGetPromiseA = qaMakeQueryA >>> arr Identity
  qaFulfilPromiseA = arr runIdentity
  qaMakeQueryA = Kleisli qaMakeQueryM
    where
    qaMakeQueryM (qa, q) = runKleisli (qaMakeQuery qa) q

executeQACachedA :: (Monad m) => (QACachedA m () a) -> m (QANetLog, a)
executeQACachedA code =
  do
  (result, ni) <- (runStateT $ runKleisli code ()) initQANetInfo
  let lg = net_log ni
  return (lg, result)

executeQACachedAId :: (QACachedAId () a) -> (QANetLog, a)
executeQACachedAId code = res
  where
  Identity res = executeQACachedA code

type QACachedM m = StateT (QANetInfo m) m

data QANetInfo m =
  QANetInfo
  {
    net_id2value :: Map.Map ValueId (AnyQAComputation m),
    net_log :: QANetLog
  }

data AnyQAComputation m =
    forall p . (QAProtocolCacheable p) => -- existentially quantified type
        AnyQAComputation (QAComputation m p)

data QAComputation m p =
    QAComputation
        p
        (QACache p)
        (Q p -> QACachedM m (A p)) -- ^ used only if a suitable answer is not in the above cache

initQANetInfo :: QANetInfo m
initQANetInfo =
    QANetInfo
    {
        net_id2value = Map.empty,
        net_log = []
    }

newId :: (QAProtocolCacheable p, Monad m) => (QA (QACachedA m) p) -> [QAId (QACachedA m)] -> QACachedM m ValueId
newId (QA__ name Nothing _ p _sampleQ (Kleisli q2a)) sourceIds =
  maybeTrace ("newId: " ++ show name) $
  do
  ni <- get
  let (i, ni') = aux ni
  put ni'
  return i
  where
  aux ni =
    (i, ni { net_id2value = id2value', net_log = net_log' } )
    where
    id2value = net_id2value ni
    lg = net_log ni
    i | Map.null id2value = (ValueId 1)
      | otherwise = succ $ fst (Map.findMax id2value)
    id2value' = Map.insert i (AnyQAComputation (QAComputation p (newQACache p) q2a)) id2value
    net_log' = lg ++ [logItem]
    logItem =
      QANetLogCreate i sourceIds name
    -- sourceIds = catMaybes $ map qaId sources
newId _ _ =
  error "internal error in AERN2.QA: newId called with an existing id"

getAnswer :: (QAProtocolCacheable p, Monad m) => p -> ValueId -> Q p -> QACachedM m (A p)
getAnswer (p :: p) valueId q =
    maybeTrace ("getAnswer: q = " ++ show q) $
    do
    ni <- get
    let ni' = logQuery ni
    put ni'
    aux ni'
    where
    logQuery ni =
        ni { net_log = (net_log ni) ++ [logItem] }
        where
        logItem = QANetLogQuery valueId (show q)
    aux ni1 =
      do
      (a, usedCache, cache') <-case lookupQACache p cache q of
        (Just a, mLogMsg) ->
          return (a, logMsg, cache)
            where logMsg = "used cache" ++ case mLogMsg of Nothing -> ""; (Just m) -> " (" ++ m ++ ")"
        (_, mLogMsg) ->
          do
          a <- q2a q
          return (a, logMsg, updateQACache p cache q a)
            where logMsg = "not used cache" ++ case mLogMsg of Nothing -> ""; (Just m) -> " (" ++ m ++ ")"
      ni2 <- get
      put $ ni2
          {
              net_id2value = id2value' ni2 cache',
              net_log = lg2 ni2 a usedCache
          }
      return $
          maybeTrace ("getAnswer: a = " ++ show a) $
              a
      where
      id2value = net_id2value ni1
      qaComputation :: (QAComputation m p)
      qaComputation = case Map.lookup valueId id2value of
          Just (AnyQAComputation comp) -> unsafeCoerce comp
          Nothing -> error $ "unknown valueId " ++ show valueId
      QAComputation _ cache q2a = qaComputation
      id2value' ni2 cache' =
          Map.insert valueId
              (AnyQAComputation (QAComputation p cache' q2a))
              (net_id2value ni2)
      lg2 ni2 a usedCache = (net_log ni2) ++ [logItem a usedCache]
      logItem a usedCache =
          QANetLogAnswer valueId usedCache (show a)
