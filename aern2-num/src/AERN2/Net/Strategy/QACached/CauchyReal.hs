{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-| Concrete types and instances for QA-networks with real numbers -}
module AERN2.Net.Strategy.QACached.CauchyReal 
(
    QAP_CauchyReal, QACached_CauchyReal
)
where

import AERN2.Num

import Control.Arrow

import AERN2.Net.Strategy.QACached.Basics 

import qualified Data.Map as Map

data QAP_CauchyReal = QAP_CauchyReal deriving Show

instance QAProtocol QAP_CauchyReal where
    type Q QAP_CauchyReal = Accuracy
    type A QAP_CauchyReal = MPBall
    type QACache QAP_CauchyReal = Map.Map Accuracy MPBall
    newCache _ = Map.empty
    getAnswerUsingCacheIfPossible _ (QAComputation _ cacheMap q2a) q
        | Map.null cacheMap || qMax < q =
            do
            a <- q2a q
            return (a, Map.insert (getAccuracy a) a cacheMap, message)
        | otherwise = return (aMax, cacheMap, "used cache")
        where
        (qMax, aMax) = Map.findMax cacheMap
        message 
            | Map.null cacheMap =
                "cache was empty"
            | otherwise = 
                "not used cache; q = " ++ show q ++ "; qMax = " ++ show qMax
            


type QACached_CauchyReal = AsCauchyReal QACached_CauchyReal_

data QACached_CauchyReal_ = 
    QACached_CauchyReal_ { cachedCR_name :: Maybe String, cachedCR_id :: ValueId }

instance CanAsCauchyRealA QACachedA QACached_CauchyReal_

instance CanReadAsCauchyRealA QACachedA QACached_CauchyReal_ where
    getAnswerCRA = 
        Kleisli $ \(r, ac) -> getAnswer QAP_CauchyReal (cachedCR_id $ unAsCauchyReal r, ac)
    getNameCRA = arr $ cachedCR_name . unAsCauchyReal

instance CanCreateAsCauchyRealA QACachedA QACached_CauchyReal_ where
    newCRA = 
        Kleisli $ \ (sources, name, ac2b) ->
            do
            valueId <- newId  QAP_CauchyReal (sources, name, runKleisli ac2b)
            return $ AsCauchyReal $ QACached_CauchyReal_ name valueId 

instance (Arrow to) => SupportsSenderIdA to QACached_CauchyReal_ where
    type SenderId to QACached_CauchyReal_ = ValueId
instance (Arrow to) => HasSenderIdA to QACached_CauchyReal_ where
    getSenderIdA =
        proc r -> returnA -< cachedCR_id r

instance CanCombineCRsA QACachedA QACached_CauchyReal_ QACached_CauchyReal_ where
    type CombinedCRs QACachedA QACached_CauchyReal_ QACached_CauchyReal_ = QACached_CauchyReal_
    getSourcesOfCombinedCRs =
        proc (AsCauchyReal r1, AsCauchyReal r2) ->
            do
            r1Id <- getSenderIdA -< r1  
            r2Id <- getSenderIdA -< r2  
            returnA -< [r1Id, r2Id]

instance CanCombineCRwithA QACachedA QACached_CauchyReal_ QACached_CauchyReal_

instance CanCombineCRsA QACachedA QACached_CauchyReal_ CauchyReal_ where
    type CombinedCRs QACachedA QACached_CauchyReal_ CauchyReal_ = QACached_CauchyReal_
    getSourcesOfCombinedCRs =
        proc (AsCauchyReal r1, _) ->
            do
            r1Id <- getSenderIdA -< r1  
            returnA -< [r1Id]

instance CanCombineCRsA QACachedA CauchyReal_ QACached_CauchyReal_ where
    type CombinedCRs QACachedA CauchyReal_ QACached_CauchyReal_ = QACached_CauchyReal_
    getSourcesOfCombinedCRs =
        proc (_, AsCauchyReal r2) ->
            do
            r2Id <- getSenderIdA -< r2  
            returnA -< [r2Id]

instance CanCombineCRwithA QACachedA QACached_CauchyReal_ CauchyReal_



