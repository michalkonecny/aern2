{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}
{-| Types for cached execution of general QA-networks. -}
module AERN2.Net.Execution.QACached.Basics where

import AERN2.Num
import Data.String (IsString(..),fromString)

import Control.Arrow
import qualified Data.Map as Map

import Control.Monad.State

import Unsafe.Coerce

type QACachedA = Kleisli QACachedM
type QACachedM = State QANetInfo

executeQACachedM :: (QACachedM a) -> a
executeQACachedM code =
    fst $ (runState code) initQANetInfo

data QANetInfo =
    QANetInfo
    {
        net_id2value :: Map.Map ValueId AnyQAComputation,
        net_log :: [String]
    }

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

class QAProtocol p where
    type Q p
    type A p
    type QACache p
    newCache :: p -> QACache p
    getAnswerUsingCacheIfPossible :: p -> QAComputation p -> Q p -> QACachedM (A p, QACache p)

data QAComputation p = 
    QAComputation  
        p 
        (QACache p) 
        (Q p -> QACachedM (A p)) -- ^ used only if a suitable answer is not in the above cache  

newtype ValueId = ValueId Integer
    deriving (Show, Eq, Ord, Enum)

newId :: (QAProtocol p) => p -> (Q p -> (QACachedM (A p))) -> QACachedM ValueId
newId p q2a =
    do
    ni <- get
    let (i, ni') = aux ni
    put ni'
    return i
    where
    aux ni =
        (i, ni { net_id2value = id2value' } )
        where
        id2value = net_id2value ni
        i | Map.null id2value = (ValueId 1)
          | otherwise = succ $ fst (Map.findMax id2value)
        id2value' = Map.insert i (AnyQAComputation (QAComputation p (newCache p) q2a)) id2value

getAnswer :: (QAProtocol p) => p -> ValueId -> Q p -> QACachedM (A p)
getAnswer p valueId q =
    do
    ni <- get
    aux ni
    where
    aux ni =
        do
        (a, cache') <- getAnswerUsingCacheIfPossible p qaComputation q
        ni2 <- get
        put $ ni2 { net_id2value = Map.insert valueId (AnyQAComputation (QAComputation p cache' q2a)) id2value }
        return a
        where
        id2value = net_id2value ni
        qaComputation = case Map.lookup valueId id2value of 
            Just (AnyQAComputation comp) -> unsafeCoerce comp
            Nothing -> error $ "unknown valueId " ++ show valueId
        QAComputation _ _ q2a = qaComputation

