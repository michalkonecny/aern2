{-# LANGUAGE ExistentialQuantification #-}
{-|
    Module      :  AERN2.QA
    Description :  Cacheable question-answer protocols
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Cacheable question-answer protocols
-}
module AERN2.QA
(
  QAProtocol(..), QAProtocolCacheable(..)
  , QA(..), newQA
  , QAArrow(..), (-:-), (//..)
  , QACachedA, QANetInfo(..)
  , executeQACachedA, printQANetLogThenResult
)
where

import Numeric.MixedTypes
import qualified Prelude as P

import Unsafe.Coerce

import Control.Arrow
import Data.Maybe
import qualified Data.Map as Map

import Control.Monad.Trans.State

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

class (Show p, Show (Q p), Show (A p)) => QAProtocol p where
  type Q p
  type A p

class (QAProtocol p) => QAProtocolCacheable p where
  type QACache p
  newQACache :: p -> QACache p
  lookupQACache :: p -> QACache p -> Q p -> Maybe (A p)
  updateQACache :: p -> QACache p -> Q p -> A p -> QACache p

{-| An object we can ask queries about.  Queries can be asked in some Arrow @to@. -}
data QA to p = QA
  {
    qaName :: String,
    qaId :: Maybe (QAId to),
    qaSources :: [QAId to],
    qaProtocol :: p,
    qaSampleQ :: Q p,
    qaMakeQuery :: (Q p) `to` (A p)
  }

newQA :: String -> p -> Q p -> (Q p) `to` (A p) -> QA to p
newQA name = QA name Nothing []

{-|
  A class of Arrows suitable for use in QA objects.
-}
class (ArrowChoice to) => QAArrow to where
  type QAId to
  {-|
    Register a QA object, which leads to a change in its
    query processing mechanism so that, eg, answers can be cached
    or computations assigned to different threads/processes.

    The second parameter is a list of objects that the given
    object directly depends on.  This is used only for logging
    and visualisation.
  -}
  qaRegister :: (QAProtocolCacheable p) =>
    (QA to p, [QA to p]) `to` (QA to p)
  qaMakeQueryA :: (QA to p, Q p) `to` (A p)

(-:-) :: (QAArrow to, QAProtocolCacheable p) => (QA to p, [QA to p]) `to` (QA to p)
(-:-) = qaRegister

infix 0 //.., -:-

(//..) :: a -> b -> (a,b)
a //..b = (a,b)

{- Simple QAArrow instances -}

{-|
  Normal Haskell functions are a trivial QAArrow instance
  where registration has no effect.
-}
instance QAArrow (->) where
  type QAId (->) = ()
  qaRegister = fst
  qaMakeQueryA (qa, q) = qaMakeQuery qa q

instance QAArrow QACachedA where
  type QAId QACachedA = ValueId
  qaRegister = Kleisli qaRegisterM
    where
    qaRegisterM (x@(QA name _ _ p sampleQ _), sources) =
      do
      xId <- newId x sources
      return $ QA name (Just xId) sourceIds p sampleQ (Kleisli $ makeQCached xId)
      where
      makeQCached = getAnswer p
      sourceIds = catMaybes $ map qaId sources
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
    printLog lg
    putStrLn $ show result

printLog :: QANetLog -> IO ()
printLog = aux 0
    where
    aux _ [] = return ()
    aux level (item : rest) =
        do
        putStrLn $ indent ++ show item
        aux level' rest
        where
        indent = replicate (int levelNow) ' '
        (levelNow, level') =
            case item of
                QANetLog_Query _ _ -> (level + 1, level + 1)
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
    | QANetLog_Query
        ValueId -- the value being queried
        String -- description of query
    | QANetLogAnswer
        ValueId -- the value being described
        String -- information about the use of cache
        String -- description of answer
    deriving Show

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

newId :: (QAProtocolCacheable p) => (QA QACachedA p) -> [QA QACachedA p] -> QACachedM ValueId
newId (QA name Nothing _ p _sampleQ (Kleisli q2a)) sources =
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
    sourceIds = catMaybes $ map qaId sources
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
        logItem = QANetLog_Query valueId (show q)
    aux ni1 =
      do
      (a, usedCache, cache') <-case lookupQACache p cache q of
        Just a -> return (a, "used cache", cache)
        _ ->
          do
          a <- q2a q
          return (a, "not used cache", updateQACache p cache q a)
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
