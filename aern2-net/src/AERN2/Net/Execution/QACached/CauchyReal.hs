{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-| Concrete types and instances for QA-networks with real numbers -}
module AERN2.Net.Execution.QACached.CauchyReal where

import AERN2.Num

import AERN2.Net.Execution.QACached.Basics 

import AERN2.Net.Spec.Arrow
import Control.Arrow
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
            return (a, Map.insert q a cacheMap)
        | otherwise = return (aMax, cacheMap)
        where
        (qMax, aMax) = Map.findMax cacheMap


newtype QACached_CauchyReal = QACached_CauchyReal ValueId

instance RingA QACachedA QACached_CauchyReal
instance FieldA QACachedA QACached_CauchyReal
instance RealA QACachedA QACached_CauchyReal
instance RealExprA QACachedA QACached_CauchyReal
instance RealPredA QACachedA QACached_CauchyReal


-- | HasIntegersA QACached_CauchyReal
instance ConvertibleA QACachedA Integer QACached_CauchyReal where
    convertNamedA name =
        Kleisli $ constCRCachedM name
    convertA = convertNamedA "anon"
        
-- | HasRationalsA QACached_CauchyReal
instance ConvertibleA QACachedA Rational QACached_CauchyReal where
    convertNamedA name =
        Kleisli $ constCRCachedM name
    convertA = convertNamedA "anon"

-- | HasCauchyRealsA QACached_CauchyReal
instance ConvertibleA QACachedA CauchyReal QACached_CauchyReal where
    convertNamedA name =
        Kleisli $ constCRCachedM name
    convertA = convertNamedA "anon"


instance HasEqA QACachedA QACached_CauchyReal QACached_CauchyReal where
    equalToA =
        Kleisli $ binrelCRCachedM "==" equalToA
    notEqualToA =
        Kleisli $ binrelCRCachedM "/=" notEqualToA

instance HasOrderA QACachedA QACached_CauchyReal QACached_CauchyReal where
    lessThanA =
        Kleisli $ binrelCRCachedM "<" lessThanA
    leqA =
        Kleisli $ binrelCRCachedM "/=" leqA
    greaterThanA =
        Kleisli $ binrelCRCachedM "<" greaterThanA
    geqA =
        Kleisli $ binrelCRCachedM "/=" geqA
        
instance CanNegA QACachedA QACached_CauchyReal where
    negA =
        Kleisli $ unaryCRCachedM "neg" neg return

instance CanNegSameTypeA QACachedA QACached_CauchyReal
        
instance CanAddA QACachedA QACached_CauchyReal QACached_CauchyReal where
    addA =
        Kleisli $ binaryCRCachedM "+" add (\q -> return (q,q))

instance CanAddThisA QACachedA QACached_CauchyReal QACached_CauchyReal
instance CanAddSameTypeA QACachedA QACached_CauchyReal

instance CanSubA QACachedA QACached_CauchyReal QACached_CauchyReal where
    subA =
        Kleisli $ binaryCRCachedM "-" sub (\q -> return (q,q))

instance CanSubThisA QACachedA QACached_CauchyReal QACached_CauchyReal
instance CanSubSameTypeA QACachedA QACached_CauchyReal

instance CanMulA QACachedA QACached_CauchyReal QACached_CauchyReal where
    mulA =
        Kleisli $ mulCRCachedM

instance CanMulByA QACachedA QACached_CauchyReal QACached_CauchyReal
instance CanMulSameTypeA QACachedA QACached_CauchyReal

mulCRCachedM :: (QACached_CauchyReal, QACached_CauchyReal) -> QACachedM QACached_CauchyReal
mulCRCachedM (a1,a2) =
    binaryCRCachedM "*" (*) getInitQ1Q2 (a1,a2)
    where
    getInitQ1Q2 q =
        do
        maybeA1NormLog <- getCachedCRFunctionNormLog q a1 id   
        maybeA2NormLog <- getCachedCRFunctionNormLog q a2 id   
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

instance CanRecipA QACachedA QACached_CauchyReal where
    recipA =
        error "QACached_CauchyReal reciprocal not implemented yet"        

instance CanRecipSameTypeA QACachedA QACached_CauchyReal

instance CanDivA QACachedA QACached_CauchyReal QACached_CauchyReal where
    divA =
        error "QACached_CauchyReal division not implemented yet"

instance CanDivByA QACachedA QACached_CauchyReal QACached_CauchyReal
instance CanDivSameTypeA QACachedA QACached_CauchyReal


instance CanSqrtA QACachedA QACached_CauchyReal where
    sqrtA = Kleisli sqrtCRCachedM

instance CanSqrtSameTypeA QACachedA QACached_CauchyReal

sqrtCRCachedM ::
    (QACached_CauchyReal) -> QACachedM QACached_CauchyReal
sqrtCRCachedM r = 
    unaryCRCachedM "sqrt" sqrt getInitQ r
    where
    getInitQ q =
        do
        maybeSqrtNormLog <- getCachedCRFunctionNormLog q r sqrt
        case maybeSqrtNormLog of
            NormBits sqrtNormLog -> return $ max 0 (q - 1 - sqrtNormLog)
            NormZero -> return q

instance CanExpA QACachedA QACached_CauchyReal where
    expA = Kleisli expCRCachedM

instance CanExpSameTypeA QACachedA QACached_CauchyReal

expCRCachedM ::
    (QACached_CauchyReal) -> QACachedM QACached_CauchyReal
expCRCachedM r = 
    unaryCRCachedM "exp" exp getInitQ r
    where
    getInitQ q =
        do
        maybeExpNormLog <- getCachedCRFunctionNormLog q r exp
        case maybeExpNormLog of
            NormBits expNormLog -> return $ q + expNormLog + 1
            NormZero -> return q

instance CanSineCosineA QACachedA QACached_CauchyReal where
    sinA = Kleisli $ unaryCRCachedM "sin" sin return
    cosA = Kleisli $ unaryCRCachedM "cos" cos return

instance CanSineCosineSameTypeA QACachedA QACached_CauchyReal


{- QACached_CauchyReal mixed with Integer -}

instance HasEqA QACachedA QACached_CauchyReal Integer where
    type EqCompareTypeA QACachedA QACached_CauchyReal Integer = Bool
    equalToA = convertSecondA equalToA

instance HasEqA QACachedA Integer QACached_CauchyReal where
    type EqCompareTypeA QACachedA Integer QACached_CauchyReal = Bool
    equalToA = convertFirstA equalToA

instance HasOrderA QACachedA QACached_CauchyReal Integer where
    type OrderCompareTypeA QACachedA QACached_CauchyReal Integer = Bool
    lessThanA = convertSecondA lessThanA
    leqA = convertSecondA leqA

instance HasOrderA QACachedA Integer QACached_CauchyReal where
    type OrderCompareTypeA QACachedA Integer QACached_CauchyReal = Bool
    lessThanA = convertFirstA lessThanA
    leqA = convertFirstA leqA

instance CanAddA QACachedA QACached_CauchyReal Integer where
    type AddTypeA QACachedA QACached_CauchyReal Integer = QACached_CauchyReal
    addA = convertSecondA addA

instance CanAddA QACachedA Integer QACached_CauchyReal where
    type AddTypeA QACachedA Integer QACached_CauchyReal = QACached_CauchyReal
    addA = convertFirstA addA

instance CanAddThisA QACachedA QACached_CauchyReal Integer

instance CanSubA QACachedA QACached_CauchyReal Integer where
    type SubTypeA QACachedA QACached_CauchyReal Integer = QACached_CauchyReal
    subA = convertSecondA subA

instance CanSubA QACachedA Integer QACached_CauchyReal where
    type SubTypeA QACachedA Integer QACached_CauchyReal = QACached_CauchyReal
    subA = convertFirstA subA

instance CanSubThisA QACachedA QACached_CauchyReal Integer

instance CanMulA QACachedA QACached_CauchyReal Integer where
    type MulTypeA QACachedA QACached_CauchyReal Integer = QACached_CauchyReal
    mulA = convertSecondA mulA

instance CanMulA QACachedA Integer QACached_CauchyReal where
    type MulTypeA QACachedA Integer QACached_CauchyReal = QACached_CauchyReal
    mulA = convertFirstA mulA

instance CanMulByA QACachedA QACached_CauchyReal Integer

instance CanDivA QACachedA QACached_CauchyReal Integer where
    type DivTypeA QACachedA QACached_CauchyReal Integer = QACached_CauchyReal
    divA = convertSecondA divA

instance CanDivA QACachedA Integer QACached_CauchyReal where
    type DivTypeA QACachedA Integer QACached_CauchyReal = QACached_CauchyReal
    divA = convertFirstA divA

instance CanDivByA QACachedA QACached_CauchyReal Integer

instance CanAddMulScalarA QACachedA QACached_CauchyReal Integer
instance CanAddMulDivScalarA QACachedA QACached_CauchyReal Integer

{- QACached_CauchyReal mixed with Rational -}

instance HasEqA QACachedA QACached_CauchyReal Rational where
    type EqCompareTypeA QACachedA QACached_CauchyReal Rational = Bool
    equalToA = convertSecondA equalToA

instance HasEqA QACachedA Rational QACached_CauchyReal where
    type EqCompareTypeA QACachedA Rational QACached_CauchyReal = Bool
    equalToA = convertFirstA equalToA

instance HasOrderA QACachedA QACached_CauchyReal Rational where
    type OrderCompareTypeA QACachedA QACached_CauchyReal Rational = Bool
    lessThanA = convertSecondA lessThanA
    leqA = convertSecondA leqA

instance HasOrderA QACachedA Rational QACached_CauchyReal where
    type OrderCompareTypeA QACachedA Rational QACached_CauchyReal = Bool
    lessThanA = convertFirstA lessThanA
    leqA = convertFirstA leqA

instance CanAddA QACachedA QACached_CauchyReal Rational where
    type AddTypeA QACachedA QACached_CauchyReal Rational = QACached_CauchyReal
    addA = convertSecondA addA

instance CanAddA QACachedA Rational QACached_CauchyReal where
    type AddTypeA QACachedA Rational QACached_CauchyReal = QACached_CauchyReal
    addA = convertFirstA addA

instance CanAddThisA QACachedA QACached_CauchyReal Rational

instance CanSubA QACachedA QACached_CauchyReal Rational where
    type SubTypeA QACachedA QACached_CauchyReal Rational = QACached_CauchyReal
    subA = convertSecondA subA

instance CanSubA QACachedA Rational QACached_CauchyReal where
    type SubTypeA QACachedA Rational QACached_CauchyReal = QACached_CauchyReal
    subA = convertFirstA subA

instance CanSubThisA QACachedA QACached_CauchyReal Rational

instance CanMulA QACachedA QACached_CauchyReal Rational where
    type MulTypeA QACachedA QACached_CauchyReal Rational = QACached_CauchyReal
    mulA = convertSecondA mulA

instance CanMulA QACachedA Rational QACached_CauchyReal where
    type MulTypeA QACachedA Rational QACached_CauchyReal = QACached_CauchyReal
    mulA = convertFirstA mulA

instance CanMulByA QACachedA QACached_CauchyReal Rational

instance CanDivA QACachedA QACached_CauchyReal Rational where
    type DivTypeA QACachedA QACached_CauchyReal Rational = QACached_CauchyReal
    divA = convertSecondA divA

instance CanDivA QACachedA Rational QACached_CauchyReal where
    type DivTypeA QACachedA Rational QACached_CauchyReal = QACached_CauchyReal
    divA = convertFirstA divA

instance CanDivByA QACachedA QACached_CauchyReal Rational

instance CanAddMulScalarA QACachedA QACached_CauchyReal Rational
instance CanAddMulDivScalarA QACachedA QACached_CauchyReal Rational

{- QACached_CauchyReal mixed with CauchyReal -}

instance HasEqA QACachedA QACached_CauchyReal CauchyReal where
    type EqCompareTypeA QACachedA QACached_CauchyReal CauchyReal = Bool
    equalToA = convertSecondA equalToA

instance HasEqA QACachedA CauchyReal QACached_CauchyReal where
    type EqCompareTypeA QACachedA CauchyReal QACached_CauchyReal = Bool
    equalToA = convertFirstA equalToA

instance HasOrderA QACachedA QACached_CauchyReal CauchyReal where
    type OrderCompareTypeA QACachedA QACached_CauchyReal CauchyReal = Bool
    lessThanA = convertSecondA lessThanA
    leqA = convertSecondA leqA

instance HasOrderA QACachedA CauchyReal QACached_CauchyReal where
    type OrderCompareTypeA QACachedA CauchyReal QACached_CauchyReal = Bool
    lessThanA = convertFirstA lessThanA
    leqA = convertFirstA leqA

instance CanAddA QACachedA QACached_CauchyReal CauchyReal where
    type AddTypeA QACachedA QACached_CauchyReal CauchyReal = QACached_CauchyReal
    addA = convertSecondA addA

instance CanAddA QACachedA CauchyReal QACached_CauchyReal where
    type AddTypeA QACachedA CauchyReal QACached_CauchyReal = QACached_CauchyReal
    addA = convertFirstA addA

instance CanAddThisA QACachedA QACached_CauchyReal CauchyReal

instance CanSubA QACachedA QACached_CauchyReal CauchyReal where
    type SubTypeA QACachedA QACached_CauchyReal CauchyReal = QACached_CauchyReal
    subA = convertSecondA subA

instance CanSubA QACachedA CauchyReal QACached_CauchyReal where
    type SubTypeA QACachedA CauchyReal QACached_CauchyReal = QACached_CauchyReal
    subA = convertFirstA subA

instance CanSubThisA QACachedA QACached_CauchyReal CauchyReal

instance CanMulA QACachedA QACached_CauchyReal CauchyReal where
    type MulTypeA QACachedA QACached_CauchyReal CauchyReal = QACached_CauchyReal
    mulA = convertSecondA mulA

instance CanMulA QACachedA CauchyReal QACached_CauchyReal where
    type MulTypeA QACachedA CauchyReal QACached_CauchyReal = QACached_CauchyReal
    mulA = convertFirstA mulA

instance CanMulByA QACachedA QACached_CauchyReal CauchyReal

instance CanDivA QACachedA QACached_CauchyReal CauchyReal where
    type DivTypeA QACachedA QACached_CauchyReal CauchyReal = QACached_CauchyReal
    divA = convertSecondA divA

instance CanDivA QACachedA CauchyReal QACached_CauchyReal where
    type DivTypeA QACachedA CauchyReal QACached_CauchyReal = QACached_CauchyReal
    divA = convertFirstA divA

instance CanDivByA QACachedA QACached_CauchyReal CauchyReal

instance CanAddMulScalarA QACachedA QACached_CauchyReal CauchyReal
instance CanAddMulDivScalarA QACachedA QACached_CauchyReal CauchyReal


getCachedCRFunctionNormLog :: 
    Accuracy -> 
    QACached_CauchyReal -> 
    (MPBall -> MPBall) -> 
    QACachedM NormLog
getCachedCRFunctionNormLog q (QACached_CauchyReal rId) fn =
    do
    x0 <- getAnswer QAP_CauchyReal rId q
    let fnx0 = fn x0
    case 1 < fnx0 of
        Just True -> return $ getNormLog fnx0
        _ -> 
            do
            x <- getAnswer QAP_CauchyReal rId q
            let fnx = fn x
            return $ getNormLog fnx

constCRCachedM ::
    (CanBeCauchyReal a) => 
    String -> 
    a -> QACachedM QACached_CauchyReal
constCRCachedM _constName r =
    fmap QACached_CauchyReal $
        newId QAP_CauchyReal $ 
            \ac -> (return $ cauchyReal2ball (cauchyReal r) ac)
            
unaryCRCachedM ::
    String -> 
    (MPBall -> MPBall) -> 
    (Accuracy -> QACachedM Accuracy) -> 
    (QACached_CauchyReal) -> QACachedM QACached_CauchyReal
unaryCRCachedM _valName op getQ1 (QACached_CauchyReal id1) =    
    fmap QACached_CauchyReal $ newId QAP_CauchyReal handleQuery
    where
    handleQuery q =
        do
        qi1 <- getQ1 q
        ensureAccuracyM1 q qi1 opWithQ1
        where
        opWithQ1 q1 =
            do
            a1 <- getAnswer QAP_CauchyReal id1 q1 
            return $ op a1 

binaryCRCachedM ::
    String -> 
    (MPBall -> MPBall -> MPBall) -> 
    (Accuracy -> QACachedM (Accuracy, Accuracy)) -> 
    (QACached_CauchyReal, QACached_CauchyReal) -> QACachedM QACached_CauchyReal
binaryCRCachedM _valName op getQ1Q2 (QACached_CauchyReal id1, QACached_CauchyReal id2) =    
    fmap QACached_CauchyReal $ newId QAP_CauchyReal handleQuery
    where
    handleQuery q =
        do
        (qi1, qi2) <- getQ1Q2 q
        ensureAccuracyM2 q qi1 qi2 opWithQ1Q2
        where
        opWithQ1Q2 q1 q2 =
            do
            a1 <- getAnswer QAP_CauchyReal id1 q1 
            a2 <- getAnswer QAP_CauchyReal id2 q2 
            return $ op a1 a2

relCRCachedM ::
    String -> 
    ([MPBall] -> Maybe a) -> 
    [QACached_CauchyReal] -> QACachedM a
relCRCachedM _valName rel rs =
    do
    aux compareTryAccuracies
    where
    aux (ac:rest) =
        do
        bs <- mapM (\(QACached_CauchyReal rId) -> getAnswer QAP_CauchyReal rId ac) rs
        case rel bs of
            Just result -> return result
            Nothing -> aux rest
    aux [] =
        error "CauchyReal comparison undecided even using maximum standard accuracy"

binrelCRCachedM ::
    String -> 
    ((MPBall, MPBall) -> Maybe a) -> 
    (QACached_CauchyReal, QACached_CauchyReal) -> QACachedM a
binrelCRCachedM valName rel rs =
    relCRCachedM valName (rel . list2pair) (pair2list rs)

list2pair :: (Show a) => [a] -> (a,a)
list2pair [a1,a2] = (a1,a2)
list2pair list = error $ "list2pair: not a pair: " ++ show list

pair2list :: (a,a) -> [a]
pair2list (a1,a2) = [a1,a2]
