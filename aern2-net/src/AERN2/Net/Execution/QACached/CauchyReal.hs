{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-| Concrete types and instances for QA-networks with real numbers -}
module AERN2.Net.Execution.QACached.CauchyReal 
(
    QAP_CauchyReal, QACached_CauchyReal,
    module AERN2.Net.Execution.QACached.CauchyReal.InnerType
)
where

import AERN2.Num

import Control.Arrow
import AERN2.Net.Spec.Arrow

import AERN2.Net.Execution.QACached.Basics 
import AERN2.Net.Execution.QACached.CauchyReal.InnerType 

import qualified Data.Map as Map

data QAP_CauchyReal = QAP_CauchyReal

instance QAProtocol QAP_CauchyReal where
    type Q QAP_CauchyReal = Accuracy
    type A QAP_CauchyReal = MPBall
    type QACache QAP_CauchyReal = Map.Map Accuracy MPBall
    newCache _ = Map.empty
    getAnswerUsingCacheIfPossible _ (QAComputation _ cacheMap q2a) q
        | Map.null cacheMap || qMax < q =
            do
            a <- q2a q
            return (a, Map.insert (getAccuracy a) a cacheMap)
        | otherwise = return (aMax, cacheMap)
        where
        (qMax, aMax) = Map.findMax cacheMap


type QACached_CauchyReal = AsCauchyReal QACached_CauchyReal_

instance CanAsCauchyRealA QACachedA QACached_CauchyReal_

instance CanReadAsCauchyRealA QACachedA QACached_CauchyReal_ where
    getAnswerCRA = 
        Kleisli $ \(r, ac) -> getAnswer QAP_CauchyReal (cachedCR_id r, ac)
    getNameCRA = arr cachedCR_name

instance CanCreateAsCauchyRealA QACachedA QACached_CauchyReal_ where
    newCRA = 
        Kleisli $ \ (name, ac2b) ->
            do
            valueId <- newId  QAP_CauchyReal (name, runKleisli ac2b)
            return $ QACached_CauchyReal_ name valueId 


instance RealA QACachedA QACached_CauchyReal
