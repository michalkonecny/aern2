{-# LANGUAGE StandaloneDeriving, ExistentialQuantification, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module AERN2.Net.Execution.Cached 

where

import AERN2.Num hiding (id, (.))
import Data.String (IsString(..),fromString)

import AERN2.Net.Spec.Arrow
import Control.Category
import Control.Arrow
import qualified Data.Map as Map

import Control.Monad.State

import Unsafe.Coerce


_anet0cachedCauchy :: Integer -> MPBall
_anet0cachedCauchy p =
    executeCachedM $
        do
        (CachedCauchyReal rId) <- runKleisli (_anet0 :: CachedA () CachedCauchyReal) ()
        a <- getAnswer QACauchyReal rId (bits p)
        return a

_anet3cachedCauchy :: (Rational, Rational, Rational) -> Integer -> MPBall
_anet3cachedCauchy (x,y,z) p =
    executeCachedM $
        do
        channels <- mapM mkInput $ [("x",x), ("y",y), ("z",z)]
        let envCh = Map.fromList channels
        (CachedCauchyReal rId) <- runKleisli _anet3 envCh
        a <- getAnswer QACauchyReal rId (bits p)
        return a
        where
        mkInput (name, value) =
            do
            ch <- constRCached name (cauchyReal2ball (rational value)) ()
            return (name, ch)

executeCachedM :: (CachedM a) -> a
executeCachedM code =
    fst $ (runState code) initNetInfo

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

initNetInfo :: NetInfo
initNetInfo =
    NetInfo
    {
        net_id2value = Map.empty,
        net_log = []
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
    addA = Kleisli $ binaryRCached "+" (+) (\q -> return (q,q))
    subA = Kleisli $ binaryRCached "-" (-) (\q -> return (q,q))
    mulA = Kleisli mulRCached
    rationalConstA name r =
        Kleisli $ constRCached name $ cauchyReal2ball $ rational r
    rationalListA name rs =
        Kleisli $ const $ mapM (\r -> constRCached name (cauchyReal2ball (rational r)) ()) rs
    rationalOpA = error "rationalOpA not implemented for CachedCauchyReal"

instance ArrowReal CachedA CachedCauchyReal where
    realConstA name r = Kleisli $ constRCached name $ cauchyReal2ball r
    sqrtA = Kleisli sqrtCached

sqrtCached ::
    (CachedCauchyReal) -> CachedM CachedCauchyReal
sqrtCached r = 
    unaryRCached "sqrt" sqrt getInitQ r
    where
    getInitQ q =
        do
        maybeSqrtNormLog <- getCachedRFunctionNormLog q r sqrt
        case maybeSqrtNormLog of
            NormBits sqrtNormLog -> return $ max 0 (q - 1 - sqrtNormLog)
            NormZero -> return q

mulRCached :: (CachedCauchyReal, CachedCauchyReal) -> CachedM CachedCauchyReal
mulRCached (a1,a2) =
    binaryRCached "*" (*) getInitQ1Q2 (a1,a2)
    where
    getInitQ1Q2 q =
        do
        maybeA1NormLog <- getCachedRFunctionNormLog q a1 id   
        maybeA2NormLog <- getCachedRFunctionNormLog q a2 id   
        return $ aux maybeA1NormLog maybeA2NormLog
        where
        aux maybeA1NormLog maybeA2NormLog =
            (initQ1, initQ2)
            where
            initQ1 = 
                case maybeA2NormLog of
                    NormBits a2NormLog -> max (bits 0) (q + a2NormLog + 1)
                    NormZero -> bits 0
            initQ2 = 
                case maybeA1NormLog of
                    NormBits a1NormLog -> max (bits 0) (q + a1NormLog + 1)
                    NormZero -> bits 0

getCachedRFunctionNormLog :: 
    Accuracy -> 
    CachedCauchyReal -> 
    (MPBall -> MPBall) -> 
    CachedM NormLog
getCachedRFunctionNormLog q (CachedCauchyReal rId) fn =
    do
    x0 <- getAnswer QACauchyReal rId q
    let fnx0 = fn x0
    case 1 < fnx0 of
        Just True -> return $ getNormLog fnx0
        _ -> 
            do
            x <- getAnswer QACauchyReal rId q
            let fnx = fn x
            return $ getNormLog fnx

constRCached :: 
    String -> 
    (Accuracy -> MPBall) -> 
    () -> CachedM CachedCauchyReal
constRCached _constName op () =
    fmap CachedCauchyReal $ 
        newId QACauchyReal $ 
            \ac -> (return $ op ac)

unaryRCached ::
    String -> 
    (MPBall -> MPBall) -> 
    (Accuracy -> CachedM Accuracy) -> 
    (CachedCauchyReal) -> CachedM CachedCauchyReal
unaryRCached _valName op getQ1 (CachedCauchyReal id1) =    
    fmap CachedCauchyReal $ newId QACauchyReal handleQuery
    where
    handleQuery q =
        do
        qi1 <- getQ1 q
        ensureAccuracyM1 q qi1 opWithQ1
        where
        opWithQ1 q1 =
            do
            a1 <- getAnswer QACauchyReal id1 q1 
            return $ op a1 

binaryRCached ::
    String -> 
    (MPBall -> MPBall -> MPBall) -> 
    (Accuracy -> CachedM (Accuracy, Accuracy)) -> 
    (CachedCauchyReal, CachedCauchyReal) -> CachedM CachedCauchyReal
binaryRCached _valName op getQ1Q2 (CachedCauchyReal id1, CachedCauchyReal id2) =    
    fmap CachedCauchyReal $ newId QACauchyReal handleQuery
    where
    handleQuery q =
        do
        (qi1, qi2) <- getQ1Q2 q
        ensureAccuracyM2 q qi1 qi2 opWithQ1Q2
        where
        opWithQ1Q2 q1 q2 =
            do
            a1 <- getAnswer QACauchyReal id1 q1 
            a2 <- getAnswer QACauchyReal id2 q2 
            return $ op a1 a2
            