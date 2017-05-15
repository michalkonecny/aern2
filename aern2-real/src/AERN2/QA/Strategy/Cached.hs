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
  QACachedA, QANetInfo(..), initQANetInfo
  , ValueId(..)
  , QANetLog, QANetLogItem(..)
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
import qualified Prelude as P
-- import Text.Printf

import Unsafe.Coerce

import Data.Functor.Identity

import Control.Arrow

-- import Data.Maybe
-- import Data.List
import qualified Data.Map as Map

import Control.Monad.Trans.State

import AERN2.QA.Protocol

instance QAArrow QACachedA where
  type QAId QACachedA = ValueId
  type QAPromise QACachedA = Identity
  qaRegister = Kleisli qaRegisterM
    where
    qaRegisterM (x@(QA__ name _ sourceIds p sampleQ _)) =
      do
      xId <- newId x sourceIds
      return $ QA__ name (Just xId) [] p sampleQ (Kleisli $ makeQCached xId)
      where
      makeQCached = getAnswer p
      -- sourceIds = catMaybes $ map anyPqaId sources
  qaMakeQueryGetPromiseA = qaMakeQueryA >>> arr Identity
  qaFulfilPromiseA = arr runIdentity
  qaMakeQueryA = Kleisli qaMakeQueryM
    where
    qaMakeQueryM (qa, q) = runKleisli (qaMakeQuery qa) q

executeQACachedA :: (() `QACachedA` a) -> (QANetLog, a)
executeQACachedA code =
    (lg, result)
    where
    (result, ni) = (runState $ runKleisli code ()) initQANetInfo
    lg = net_log ni

printQANetLogThenResult :: (Show a) =>(QANetLog, a) -> IO ()
printQANetLogThenResult (lg, result) =
    do
    printQALog lg
    putStrLn $ show result

printQALog :: QANetLog -> IO ()
printQALog = putStrLn . formatQALog 0

formatQALog :: Integer -> QANetLog -> String
formatQALog = aux
    where
    aux _ [] = ""
    aux level (item : rest) =
        (indent ++ show item ++ "\n") ++
        (aux level' rest)
        where
        indent = replicate (int levelNow) ' '
        (levelNow, level') =
            case item of
                QANetLogQuery _ _ -> (level + 1, level + 1)
                QANetLogAnswer _ _ _ -> (level, level - 1)
                _ -> (level, level)


type QACachedA = Kleisli QACachedM
type QACachedM = State QANetInfo

data QANetInfo =
  QANetInfo
  {
    net_id2value :: Map.Map ValueId AnyQAComputation,
    net_log :: QANetLog
  }

newtype ValueId = ValueId Integer
    deriving (Show, P.Eq, P.Ord, Enum)

data AnyQAComputation =
    forall p . (QAProtocolCacheable p) => -- existentially quantified type
        AnyQAComputation (QAComputation p)

type QANetLog = [QANetLogItem]

data QANetLogItem
    = QANetLogCreate
        ValueId -- new value
        [ValueId] -- dependent values
        String -- name
    | QANetLogQuery
        ValueId -- the value being queried
        String -- description of query
    | QANetLogAnswer
        ValueId -- the value being described
        String -- information about the use of cache
        String -- description of answer

instance Show QANetLogItem where
  show (QANetLogCreate valId sources name) =
    "new (" ++ (show valId) ++ ") " ++ name ++ " <- " ++ show sources
  show (QANetLogQuery valId queryS) =
    "(" ++ (show valId) ++ "): ? " ++ queryS
  show (QANetLogAnswer valId cacheInfoS answerS) =
    "(" ++ (show valId) ++ "): ! " ++ answerS ++ " (" ++ cacheInfoS ++ ")"

data QAComputation p =
    QAComputation
        p
        (QACache p)
        (Q p -> QACachedM (A p)) -- ^ used only if a suitable answer is not in the above cache

initQANetInfo :: QANetInfo
initQANetInfo =
    QANetInfo
    {
        net_id2value = Map.empty,
        net_log = []
    }

newId :: (QAProtocolCacheable p) => (QA QACachedA p) -> [QAId QACachedA] -> QACachedM ValueId
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

getAnswer :: (QAProtocolCacheable p) => p -> ValueId -> Q p -> QACachedM (A p)
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
      qaComputation :: (QAComputation p)
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
