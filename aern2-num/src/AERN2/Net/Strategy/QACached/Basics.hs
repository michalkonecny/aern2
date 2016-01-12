{-# LANGUAGE CPP, ExistentialQuantification, GeneralizedNewtypeDeriving, TypeOperators, FlexibleContexts #-}
{-| Types for cached execution of general QA-networks. -}
module AERN2.Net.Strategy.QACached.Basics where

import AERN2.Num

import Control.Arrow
import qualified Data.Map as Map

import Control.Monad.State

import Unsafe.Coerce

import Debug.Trace (trace)

#if 1
shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id
#endif

type QACachedA = Kleisli QACachedM
type QACachedM = State QANetInfo

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

data QANetInfo =
    QANetInfo
    {
        net_id2value :: Map.Map ValueId AnyQAComputation,
        net_log :: QANetLog
    }

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

initQANetInfo :: QANetInfo
initQANetInfo =
    QANetInfo
    {
        net_id2value = Map.empty,
        net_log = []
    }   

data AnyQAComputation =
    forall p . (QAProtocol p) => -- existentially quantified type
        AnyQAComputation (QAComputation p)

class (Show (Q p), Show (A p), Show p) => QAProtocol p where
    type Q p
    type A p
    type QACache p
    newCache :: p -> QACache p
    getAnswerUsingCacheIfPossible :: p -> QAComputation p -> Q p -> QACachedM (A p, QACache p, String)

data QAComputation p = 
    QAComputation  
        p 
        (QACache p) 
        (Q p -> QACachedM (A p)) -- ^ used only if a suitable answer is not in the above cache  

newtype ValueId = ValueId Integer
    deriving (Show, Eq, Ord, Enum)

newId :: (QAProtocol p) => p -> ([ValueId], Maybe String, Q p -> (QACachedM (A p))) -> QACachedM ValueId
newId p (sources, name, q2a) =
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
        id2value' = Map.insert i (AnyQAComputation (QAComputation p (newCache p) q2a)) id2value
        net_log' = lg ++ [logItem]
        logItem =
            QANetLogCreate i sources nameS
        nameS =
            case name of
                Just n -> n
                _ -> "(anonymous)"

getAnswer :: (QAProtocol p) => p -> (ValueId, Q p) -> QACachedM (A p)
getAnswer p (valueId, q) =
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
    aux ni =
        do
        (a, cache', usedCache) <- getAnswerUsingCacheIfPossible p qaComputation q
        ni2 <- get
        put $ ni2 
            { 
                net_id2value = id2value' ni2 cache',
                net_log = lg2 ni2 a usedCache
            }
        return a
        where
        id2value = net_id2value ni
        qaComputation = case Map.lookup valueId id2value of 
            Just (AnyQAComputation comp) -> unsafeCoerce comp
            Nothing -> error $ "unknown valueId " ++ show valueId
        QAComputation _ _ q2a = qaComputation
        id2value' ni2 cache' =
            Map.insert valueId 
                (AnyQAComputation (QAComputation p cache' q2a)) 
                (net_id2value ni2)
        lg2 ni2 a usedCache = (net_log ni2) ++ [logItem a usedCache]
        logItem a usedCache = 
            QANetLogAnswer valueId usedCache (show a)
            