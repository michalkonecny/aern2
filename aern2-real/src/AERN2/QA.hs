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
  , QA(..), AnyProtocolQA(..), addUnsafeMemoisation
  , QAArrow(..), qaMakeQueryOnManyA, (-:-), qaArr
  , QACachedA, QANetInfo(..)
  , executeQACachedA, printQANetLogThenResult
)
where

import Numeric.MixedTypes
import qualified Prelude as P

import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce

import Data.Functor.Identity

import Control.Arrow
import Control.Concurrent.MVar

-- import Data.Maybe
import Data.List
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
data QA to p = QA__
  {
    qaName :: String,
    qaId :: Maybe (QAId to),
    qaSources :: [QAId to],
    qaProtocol :: p,
    qaSampleQ :: Q p,
    qaMakeQuery :: (Q p) `to` (A p)
  }

data AnyProtocolQA to =
  forall p. (QAProtocolCacheable p) => AnyProtocolQA (QA to p)

anyPqaId :: AnyProtocolQA to -> (Maybe (QAId to))
anyPqaId (AnyProtocolQA qa) = qaId qa

anyPqaSources :: AnyProtocolQA to -> [QAId to]
anyPqaSources (AnyProtocolQA qa) = qaSources qa

{-| Apply an arrow morphism on all elements of a list -}
mapA :: (ArrowChoice to) => (t1 `to` t2) -> ([t1] `to` [t2])
mapA fA =
  proc list -> do
    case list of
      [] -> returnA -< []
      (x : xs) -> do
        y <- fA -< x
        ys <-mapA fA -< xs
        returnA -< y : ys

{-|
  A class of Arrows suitable for use in QA objects.
-}
class (ArrowChoice to, P.Eq (QAId to)) => QAArrow to where
  type QAId to
  type QAPromise to :: * -> *
  {-|
    Register a QA object, which leads to a change in its
    query processing mechanism so that, eg, answers can be cached
    or computations assigned to different threads/processes.

    The "sources" component of the incoming QA object can be
    used to record the dependency graph among QA objects.
    After registration, the QA object should have its list
    of dependencies **empty**
    as the registration has recorded them elsewhere.
  -}
  qaRegister :: (QAProtocolCacheable p) =>
    (QA to p) `to` (QA to p)
  qaMakeQueryGetPromiseA :: (QA to p, Q p) `to` (QAPromise to (A p))
  qaFulfilPromiseA :: (QAPromise to a) `to` a
  qaMakeQueryA :: (QA to p, Q p) `to` (A p)
  qaMakeQueryA = qaMakeQueryGetPromiseA >>> qaFulfilPromiseA
  qaMakeQueriesA :: [(QA to p, Q p)] `to` [A p]
  qaMakeQueriesA = (mapA qaMakeQueryGetPromiseA) >>> (mapA qaFulfilPromiseA)
  newQA :: (QAProtocolCacheable p) =>
    String -> [AnyProtocolQA to] -> p -> Q p -> (Q p) `to` (A p) -> QA to p
  newQA = defaultNewQA

defaultNewQA ::
  (QAArrow to, QAProtocolCacheable p) =>
  String -> [AnyProtocolQA to] -> p -> Q p -> (Q p) `to` (A p) -> QA to p
defaultNewQA name sources =
  QA__ name Nothing (nub $ concat $ map getSourceIds sources)
  where
  getSourceIds source =
    case anyPqaId source of
      Just id1 -> [id1]
      Nothing -> anyPqaSources source

qaMakeQueryOnManyA :: (QAArrow to) => ([QA to p], Q p) `to` [A p]
qaMakeQueryOnManyA =
  proc (qas, q) -> qaMakeQueriesA -< map (flip (,) q) qas

(-:-) :: (QAArrow to, QAProtocolCacheable p) => (QA to p) `to` (QA to p)
(-:-) = qaRegister

infix 0 -:-

-- (//..) :: a -> b -> (a,b)
-- a //..b = (a,b)

{- Simple QAArrow instances -}

{-|
  Normal Haskell functions are a trivial QAArrow instance
  where registration has no effect.
-}
instance QAArrow (->) where
  type QAId (->) = ()
  type QAPromise (->) = Identity
  qaMakeQueryGetPromiseA (qa, q) = Identity $ qaMakeQuery qa q
  qaFulfilPromiseA = runIdentity
  qaMakeQueryA (qa, q) = qaMakeQuery qa q
  qaMakeQueriesA = map qaMakeQueryA
  qaRegister = id
  newQA name sources p sampleQ makeQ =
    addUnsafeMemoisation $ defaultNewQA name sources p sampleQ makeQ

{-| Turn a pure QA object into any QAArrow QA object. -}
qaArr :: (QAArrow to, QAProtocolCacheable p) => (QA (->) p) -> (QA to p)
qaArr qa =
  newQA (qaName qa) [] (qaProtocol qa) (qaSampleQ qa) (arr (qaMakeQuery qa))

{-|
  Add caching to pure (->) QA objects via unsafe memoization, inspired by
  https://hackage.haskell.org/package/ireal-0.2.3/docs/src/Data-Number-IReal-UnsafeMemo.html#unsafeMemo,
  which, in turn, is inspired by Lennart Augustsson's uglymemo.
-}
addUnsafeMemoisation :: (QAProtocolCacheable p) => QA (->) p -> QA (->) p
addUnsafeMemoisation qa = qa { qaMakeQuery = unsafeMemo }
  where
  unsafeMemo = unsafePerformIO . unsafePerformIO memoIO
  p = qaProtocol qa
  memoIO =
    do
    cacheVar <- newMVar $ newQACache p
    return $ useMVar cacheVar
    where
    useMVar cacheVar q =
      do
      cache <- readMVar cacheVar
      case lookupQACache p cache q of
        Just a -> return a
        _ ->
          do
          let a = qaMakeQuery qa q
          modifyMVar_ cacheVar (const (return (updateQACache p cache q a)))
          return a

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
