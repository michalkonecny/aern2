{-# LANGUAGE ExistentialQuantification #-}
{-|
    Module      :  AERN2.QA.Strategy.Cached.NetState
    Description :  state of a QA net
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    A monad-generic state of a QA net.
-}
module AERN2.QA.Strategy.Cached.NetState
(
  QANetState(..), initQANetState
  , AnyQAComputation(..), QAComputation(..)
  , insertNode, logQuery, logAnswerUpdateCache, getAnswerPromise
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import Control.Arrow

import Unsafe.Coerce

-- import Data.Maybe
-- import Data.List
import qualified Data.Map as Map

import AERN2.QA.Protocol
import AERN2.QA.NetLog

data QANetState m =
  QANetState
  {
    net_id2value :: Map.Map ValueId (AnyQAComputation m)
    , net_log :: QANetLog
    , net_should_cache :: Bool
  }

data AnyQAComputation m =
    forall p . (QAProtocolCacheable p) => -- existentially quantified type
        AnyQAComputation (QAComputation m p)

data QAComputation m p =
    QAComputation
        p
        (QACache p)
        (Q p -> m (QAPromiseA (Kleisli m) (A p))) -- ^ used only if a suitable answer is not in the above cache

initQANetState :: Bool -> QANetState m
initQANetState should_cache =
    QANetState
    {
        net_id2value = Map.empty
        , net_log = []
        , net_should_cache = should_cache
    }

insertNode ::
  (QAProtocolCacheable p) =>
  p ->
  String ->
  [ValueId] ->
  (Q p -> m (QAPromiseA (Kleisli m) (A p))) ->
  QANetState m ->
  (ValueId, QANetState m)
insertNode p name sourceIds q2pa ns =
  (i, ns { net_id2value = id2value', net_log = net_log' } )
  where
  id2value = net_id2value ns
  lg = net_log ns
  i | Map.null id2value = (ValueId 1)
    | otherwise = succ $ fst (Map.findMax id2value)
  id2value' = Map.insert i (AnyQAComputation (QAComputation p (newQACache p) q2pa)) id2value
  net_log' = lg ++ [logItem]
  logItem =
    QANetLogCreate i sourceIds name


logQuery ::
  QANetState m -> ValueId -> String -> QANetState m
logQuery ns valueId qS =
  ns { net_log = (net_log ns) ++ [logItem] }
  where
  logItem = QANetLogQuery valueId qS

logAnswerUpdateCache ::
  (QAProtocolCacheable p)
  =>
  QANetState m -> p -> ValueId -> (String, String, QACache p) -> QANetState m
logAnswerUpdateCache ns (p :: p) valueId (aS, usedCacheS, cache') =
  ns
  {
      net_id2value = id2value',
      net_log = (net_log ns) ++ [logItem]
  }
  where
  logItem = QANetLogAnswer valueId usedCacheS aS
  id2value' =
      Map.insert valueId
          (AnyQAComputation (QAComputation p cache' q2a))
          (net_id2value ns)
  id2value = net_id2value ns
  qaComputation :: (QAComputation m p)
  qaComputation = case Map.lookup valueId id2value of
      Just (AnyQAComputation comp) -> unsafeCoerce comp
      Nothing -> error $ "unknown valueId " ++ show valueId
  QAComputation _ _ q2a = qaComputation

getAnswerPromise ::
  (QAProtocolCacheable p, Monad m)
  =>
  QANetState m -> p -> ValueId -> Q p -> m (() -> m (A p, [Char], QACache p))
getAnswerPromise ns (p :: p) valueId q =
  do
  case lookupQACache p cache q of
    (Just a, mLogMsg) ->
      return $ \() -> return (a, logMsg, cache)
      where logMsg = "used cache" ++ case mLogMsg of Nothing -> ""; (Just m) -> " (" ++ m ++ ")"
    (_, mLogMsg) ->
      do
      pa <- q2pa q
      a <- runKleisli pa ()
      let cache' = updateQACache p q a cache
      let a' = case lookupQACache p cache' q of (Just aa, _) -> aa; _ -> a
      if should_cache
        then return $ \() -> return (a', logMsg, cache')
        else return $ \() -> return (a, logMsg, cache)
      where logMsg = "not used cache" ++ case mLogMsg of Nothing -> ""; (Just m) -> " (" ++ m ++ ")"
  where
  id2value = net_id2value ns
  should_cache = net_should_cache ns
  qaComputation :: (QAComputation m p)
  qaComputation = case Map.lookup valueId id2value of
      Just (AnyQAComputation comp) -> unsafeCoerce comp
      Nothing -> error $ "unknown valueId " ++ show valueId
  QAComputation _ cache q2pa = qaComputation
