{-# LANGUAGE StandaloneDeriving, ExistentialQuantification, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module AERN2.Net.Execution.Cached where

import AERN2.Num hiding (id, (.))
import Data.String (IsString(..),fromString)

import AERN2.Net.Spec.Arrow
import Control.Category
import Control.Arrow
import qualified Data.Map as Map

import Control.Monad.State

import Unsafe.Coerce

{- Types for cached execution of general QA-networks. -}

class QAProtocol p where
    type Q p
    type A p
    type QACache p
    newCache :: p -> QACache p
    getAnswerUsingCacheIfPossible :: p -> QAComputation p -> Q p -> CachedM (A p, QACache p)

data QAComputation p = 
    QAComputation  
        p 
        (QACache p) 
        (Q p -> CachedM (A p)) -- ^ used only if a suitable answer is not in the above cache  

type CachedA = Kleisli CachedM
type CachedM = State NetInfo

data NetInfo =
    NetInfo
    {
        net_id2value :: Map.Map ValueId AnyQA,
        net_log :: [String]
    }

newtype ValueId = ValueId Integer
    deriving (Show, Eq, Ord, Enum)

data AnyQA =
    forall p . (QAProtocol p) => 
        AnyQA (QAComputation p)

newId :: (QAProtocol p) => p -> (Q p -> (CachedM (A p))) -> CachedM ValueId
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
        id2value' = Map.insert i (AnyQA (QAComputation p (newCache p) q2a)) id2value

getAnswer :: (QAProtocol p) => p -> ValueId -> Q p -> CachedM (A p)
getAnswer p valueId q =
    do
    ni <- get
    aux ni
    where
    aux ni =
        do
        (a, cache') <- getAnswerUsingCacheIfPossible p qaComputation q
        ni2 <- get
        put $ ni2 { net_id2value = Map.insert valueId (AnyQA (QAComputation p cache' q2a)) id2value }
        return a
        where
        id2value = net_id2value ni
        qaComputation = case Map.lookup valueId id2value of 
            Just (AnyQA comp) -> unsafeCoerce comp
            Nothing -> error $ "unknown valueId " ++ show valueId
        QAComputation _ _ q2a = qaComputation

{- Concrete types and instances for QA-networks with real numbers -}

data QACauchyReal = QACauchyReal

instance QAProtocol QACauchyReal where
    type Q QACauchyReal = Accuracy
    type A QACauchyReal = MPBall
    type QACache QACauchyReal = Map.Map Accuracy MPBall
    newCache _ = Map.empty
    getAnswerUsingCacheIfPossible _ (QAComputation _ cacheMap q2a) q
        | Map.null cacheMap || qMax < q =
            do
            a <- q2a q
            return (a, Map.insert q a cacheMap)
        | otherwise = return (aMax, cacheMap)
        where
        (qMax, aMax) = Map.findMax cacheMap

newtype CachedCauchyReal = CachedCauchyReal ValueId

instance ArrowRational CachedA CachedCauchyReal where
--    lessA = uncurry (<)
--    leqA = uncurry (<=)
    addA = Kleisli $ binaryCached "+" (+) return return
    subA = Kleisli $ binaryCached "-" (-) return return
--    subA = uncurry (-)
--    mulA = uncurry (*)
--    rationalConstA _name r = const $ rational r
--    rationalListA _name rs = const $ map rational rs
--    rationalOpA = error "rationalOpA not implemented for CauchyReal"

    
binaryCached ::
    String -> 
    (MPBall -> MPBall -> MPBall) -> 
    (Accuracy -> CachedM Accuracy) -> 
    (Accuracy -> CachedM Accuracy) -> 
    (CachedCauchyReal, CachedCauchyReal) -> CachedM CachedCauchyReal
binaryCached _valName op getQ1 getQ2 (CachedCauchyReal id1, CachedCauchyReal id2) =    
    fmap CachedCauchyReal $ newId QACauchyReal handleQuery
    where
    handleQuery q =
        do
        qi1 <- getQ1 q
        qi2 <- getQ2 q
        ensureAccuracyM2 q qi1 qi2 opWithQ1Q2
        where
        opWithQ1Q2 q1 q2 =
            do
            a1 <- getAnswer QACauchyReal id1 q1 
            a2 <- getAnswer QACauchyReal id2 q2 
            return $ op a1 a2 
