{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-| Concrete types and instances for QA-networks with balls approximating real numbers -}
module AERN2.Net.Execution.QACached.MPBall where

import AERN2.Num

import AERN2.Net.Execution.QACached.Basics 

import AERN2.Net.Spec.Arrow
import Control.Arrow
import qualified Data.Map as Map

data QAP_MPBall = QAP_MPBall

instance QAProtocol QAP_MPBall where
    type Q QAP_MPBall = Precision
    type A QAP_MPBall = MPBall
    type QACache QAP_MPBall = Map.Map Precision MPBall
    newCache _ = Map.empty
--    getAnswerUsingCacheIfPossible _ (QAComputation _ cacheMap q2a) q
--        | Map.null cacheMap || qMax < q =
--            do
--            a <- q2a q
--            return (a, Map.insert q a cacheMap)
--        | otherwise = return (aMax, cacheMap)
--        where
--        (qMax, aMax) = Map.findMax cacheMap


newtype QACached_MPBall = QACached_MPBall ValueId

instance RingA QACachedA QACached_MPBall
instance FieldA QACachedA QACached_MPBall
instance RealA QACachedA QACached_MPBall
instance RealExprA QACachedA QACached_MPBall
instance RealPredA QACachedA QACached_MPBall


-- | HasIntegersA QACached_MPBall
instance ConvertibleA QACachedA Integer QACached_MPBall where
    convertNamedA name =
        error "conversion from Integer to QACached_MPBall not implemented yet"
--        Kleisli $ constMPBCachedM name integer2BallP
    convertA = convertNamedA "anon"
        
-- | HasRationalsA QACached_MPBall
instance ConvertibleA QACachedA Rational QACached_MPBall where
    convertNamedA name =
        error "conversion from Rational to QACached_MPBall not implemented yet"
--        Kleisli $ constMPBCachedM name rational2BallP
    convertA = convertNamedA "anon"

-- | HasCauchyRealsA QACached_MPBall
instance ConvertibleA QACachedA CauchyReal QACached_MPBall where
    convertNamedA name =
        error "conversion from CauchyReal to QACached_MPBall not implemented yet"
--        Kleisli $ constMPBCachedM name cauchyReal2ball
    convertA = convertNamedA "anon"


instance HasEqA QACachedA QACached_MPBall QACached_MPBall where
    equalToA =
        Kleisli $ binrelMPBCachedM "==" equalToA
    notEqualToA =
        Kleisli $ binrelMPBCachedM "/=" notEqualToA

instance HasOrderA QACachedA QACached_MPBall QACached_MPBall where
    lessThanA =
        Kleisli $ binrelMPBCachedM "<" lessThanA
    leqA =
        Kleisli $ binrelMPBCachedM "/=" leqA
    greaterThanA =
        Kleisli $ binrelMPBCachedM "<" greaterThanA
    geqA =
        Kleisli $ binrelMPBCachedM "/=" geqA
        
instance CanNegA QACachedA QACached_MPBall where
    negA =
        Kleisli $ unaryMPBCachedM "neg" neg

instance CanNegSameTypeA QACachedA QACached_MPBall
        
instance CanAddA QACachedA QACached_MPBall QACached_MPBall where
    addA =
        Kleisli $ binaryMPBCachedM "+" add

instance CanAddThisA QACachedA QACached_MPBall QACached_MPBall
instance CanAddSameTypeA QACachedA QACached_MPBall

instance CanSubA QACachedA QACached_MPBall QACached_MPBall where
    subA =
        Kleisli $ binaryMPBCachedM "-" sub

instance CanSubThisA QACachedA QACached_MPBall QACached_MPBall
instance CanSubSameTypeA QACachedA QACached_MPBall

instance CanMulA QACachedA QACached_MPBall QACached_MPBall where
    mulA =
        Kleisli $ mulCRCachedM

instance CanMulByA QACachedA QACached_MPBall QACached_MPBall
instance CanMulSameTypeA QACachedA QACached_MPBall

mulCRCachedM :: (QACached_MPBall, QACached_MPBall) -> QACachedM QACached_MPBall
mulCRCachedM (a1,a2) =
    binaryMPBCachedM "*" (*) (a1,a2)

instance CanRecipA QACachedA QACached_MPBall where
    recipA =
        error "QACached_MPBall reciprocal not implemented yet"        

instance CanRecipSameTypeA QACachedA QACached_MPBall

instance CanDivA QACachedA QACached_MPBall QACached_MPBall where
    divA =
        error "QACached_MPBall division not implemented yet"

instance CanDivByA QACachedA QACached_MPBall QACached_MPBall
instance CanDivSameTypeA QACachedA QACached_MPBall


instance CanSqrtA QACachedA QACached_MPBall where
    sqrtA = Kleisli sqrtCRCachedM

instance CanSqrtSameTypeA QACachedA QACached_MPBall

sqrtCRCachedM ::
    (QACached_MPBall) -> QACachedM QACached_MPBall
sqrtCRCachedM r = 
    unaryMPBCachedM "sqrt" sqrt r

instance CanExpA QACachedA QACached_MPBall where
    expA = Kleisli expCRCachedM

instance CanExpSameTypeA QACachedA QACached_MPBall

expCRCachedM ::
    (QACached_MPBall) -> QACachedM QACached_MPBall
expCRCachedM r = 
    unaryMPBCachedM "exp" exp r

instance CanSineCosineA QACachedA QACached_MPBall where
    sinA = Kleisli $ unaryMPBCachedM "sin" sin
    cosA = Kleisli $ unaryMPBCachedM "cos" cos

instance CanSineCosineSameTypeA QACachedA QACached_MPBall


{- QACached_MPBall mixed with Integer -}

instance HasEqA QACachedA QACached_MPBall Integer where
    type EqCompareTypeA QACachedA QACached_MPBall Integer = Bool
    equalToA = convertSecondA equalToA

instance HasEqA QACachedA Integer QACached_MPBall where
    type EqCompareTypeA QACachedA Integer QACached_MPBall = Bool
    equalToA = convertFirstA equalToA

instance HasOrderA QACachedA QACached_MPBall Integer where
    type OrderCompareTypeA QACachedA QACached_MPBall Integer = Bool
    lessThanA = convertSecondA lessThanA
    leqA = convertSecondA leqA

instance HasOrderA QACachedA Integer QACached_MPBall where
    type OrderCompareTypeA QACachedA Integer QACached_MPBall = Bool
    lessThanA = convertFirstA lessThanA
    leqA = convertFirstA leqA

instance CanAddA QACachedA QACached_MPBall Integer where
    type AddTypeA QACachedA QACached_MPBall Integer = QACached_MPBall
    addA = convertSecondA addA

instance CanAddA QACachedA Integer QACached_MPBall where
    type AddTypeA QACachedA Integer QACached_MPBall = QACached_MPBall
    addA = convertFirstA addA

instance CanAddThisA QACachedA QACached_MPBall Integer

instance CanSubA QACachedA QACached_MPBall Integer where
    type SubTypeA QACachedA QACached_MPBall Integer = QACached_MPBall
    subA = convertSecondA subA

instance CanSubA QACachedA Integer QACached_MPBall where
    type SubTypeA QACachedA Integer QACached_MPBall = QACached_MPBall
    subA = convertFirstA subA

instance CanSubThisA QACachedA QACached_MPBall Integer

instance CanMulA QACachedA QACached_MPBall Integer where
    type MulTypeA QACachedA QACached_MPBall Integer = QACached_MPBall
    mulA = convertSecondA mulA

instance CanMulA QACachedA Integer QACached_MPBall where
    type MulTypeA QACachedA Integer QACached_MPBall = QACached_MPBall
    mulA = convertFirstA mulA

instance CanMulByA QACachedA QACached_MPBall Integer

instance CanDivA QACachedA QACached_MPBall Integer where
    type DivTypeA QACachedA QACached_MPBall Integer = QACached_MPBall
    divA = convertSecondA divA

instance CanDivA QACachedA Integer QACached_MPBall where
    type DivTypeA QACachedA Integer QACached_MPBall = QACached_MPBall
    divA = convertFirstA divA

instance CanDivByA QACachedA QACached_MPBall Integer

instance CanAddMulScalarA QACachedA QACached_MPBall Integer
instance CanAddMulDivScalarA QACachedA QACached_MPBall Integer

{- QACached_MPBall mixed with Rational -}

instance HasEqA QACachedA QACached_MPBall Rational where
    type EqCompareTypeA QACachedA QACached_MPBall Rational = Bool
    equalToA = convertSecondA equalToA

instance HasEqA QACachedA Rational QACached_MPBall where
    type EqCompareTypeA QACachedA Rational QACached_MPBall = Bool
    equalToA = convertFirstA equalToA

instance HasOrderA QACachedA QACached_MPBall Rational where
    type OrderCompareTypeA QACachedA QACached_MPBall Rational = Bool
    lessThanA = convertSecondA lessThanA
    leqA = convertSecondA leqA

instance HasOrderA QACachedA Rational QACached_MPBall where
    type OrderCompareTypeA QACachedA Rational QACached_MPBall = Bool
    lessThanA = convertFirstA lessThanA
    leqA = convertFirstA leqA

instance CanAddA QACachedA QACached_MPBall Rational where
    type AddTypeA QACachedA QACached_MPBall Rational = QACached_MPBall
    addA = convertSecondA addA

instance CanAddA QACachedA Rational QACached_MPBall where
    type AddTypeA QACachedA Rational QACached_MPBall = QACached_MPBall
    addA = convertFirstA addA

instance CanAddThisA QACachedA QACached_MPBall Rational

instance CanSubA QACachedA QACached_MPBall Rational where
    type SubTypeA QACachedA QACached_MPBall Rational = QACached_MPBall
    subA = convertSecondA subA

instance CanSubA QACachedA Rational QACached_MPBall where
    type SubTypeA QACachedA Rational QACached_MPBall = QACached_MPBall
    subA = convertFirstA subA

instance CanSubThisA QACachedA QACached_MPBall Rational

instance CanMulA QACachedA QACached_MPBall Rational where
    type MulTypeA QACachedA QACached_MPBall Rational = QACached_MPBall
    mulA = convertSecondA mulA

instance CanMulA QACachedA Rational QACached_MPBall where
    type MulTypeA QACachedA Rational QACached_MPBall = QACached_MPBall
    mulA = convertFirstA mulA

instance CanMulByA QACachedA QACached_MPBall Rational

instance CanDivA QACachedA QACached_MPBall Rational where
    type DivTypeA QACachedA QACached_MPBall Rational = QACached_MPBall
    divA = convertSecondA divA

instance CanDivA QACachedA Rational QACached_MPBall where
    type DivTypeA QACachedA Rational QACached_MPBall = QACached_MPBall
    divA = convertFirstA divA

instance CanDivByA QACachedA QACached_MPBall Rational

instance CanAddMulScalarA QACachedA QACached_MPBall Rational
instance CanAddMulDivScalarA QACachedA QACached_MPBall Rational

{- QACached_MPBall mixed with CauchyReal -}

instance HasEqA QACachedA QACached_MPBall CauchyReal where
    type EqCompareTypeA QACachedA QACached_MPBall CauchyReal = Bool
    equalToA = convertSecondA equalToA

instance HasEqA QACachedA CauchyReal QACached_MPBall where
    type EqCompareTypeA QACachedA CauchyReal QACached_MPBall = Bool
    equalToA = convertFirstA equalToA

instance HasOrderA QACachedA QACached_MPBall CauchyReal where
    type OrderCompareTypeA QACachedA QACached_MPBall CauchyReal = Bool
    lessThanA = convertSecondA lessThanA
    leqA = convertSecondA leqA

instance HasOrderA QACachedA CauchyReal QACached_MPBall where
    type OrderCompareTypeA QACachedA CauchyReal QACached_MPBall = Bool
    lessThanA = convertFirstA lessThanA
    leqA = convertFirstA leqA

instance CanAddA QACachedA QACached_MPBall CauchyReal where
    type AddTypeA QACachedA QACached_MPBall CauchyReal = QACached_MPBall
    addA = convertSecondA addA

instance CanAddA QACachedA CauchyReal QACached_MPBall where
    type AddTypeA QACachedA CauchyReal QACached_MPBall = QACached_MPBall
    addA = convertFirstA addA

instance CanAddThisA QACachedA QACached_MPBall CauchyReal

instance CanSubA QACachedA QACached_MPBall CauchyReal where
    type SubTypeA QACachedA QACached_MPBall CauchyReal = QACached_MPBall
    subA = convertSecondA subA

instance CanSubA QACachedA CauchyReal QACached_MPBall where
    type SubTypeA QACachedA CauchyReal QACached_MPBall = QACached_MPBall
    subA = convertFirstA subA

instance CanSubThisA QACachedA QACached_MPBall CauchyReal

instance CanMulA QACachedA QACached_MPBall CauchyReal where
    type MulTypeA QACachedA QACached_MPBall CauchyReal = QACached_MPBall
    mulA = convertSecondA mulA

instance CanMulA QACachedA CauchyReal QACached_MPBall where
    type MulTypeA QACachedA CauchyReal QACached_MPBall = QACached_MPBall
    mulA = convertFirstA mulA

instance CanMulByA QACachedA QACached_MPBall CauchyReal

instance CanDivA QACachedA QACached_MPBall CauchyReal where
    type DivTypeA QACachedA QACached_MPBall CauchyReal = QACached_MPBall
    divA = convertSecondA divA

instance CanDivA QACachedA CauchyReal QACached_MPBall where
    type DivTypeA QACachedA CauchyReal QACached_MPBall = QACached_MPBall
    divA = convertFirstA divA

instance CanDivByA QACachedA QACached_MPBall CauchyReal

instance CanAddMulScalarA QACachedA QACached_MPBall CauchyReal
instance CanAddMulDivScalarA QACachedA QACached_MPBall CauchyReal


getCachedMPBFunctionNormLog :: 
    Precision -> 
    QACached_MPBall -> 
    (MPBall -> MPBall) -> 
    QACachedM NormLog
getCachedMPBFunctionNormLog q (QACached_MPBall rId) fn =
    do
    x0 <- getAnswer QAP_MPBall (rId, q)
    let fnx0 = fn x0
    case 1 < fnx0 of
        Just True -> return $ getNormLog fnx0
        _ -> 
            do
            x <- getAnswer QAP_MPBall (rId, q)
            let fnx = fn x
            return $ getNormLog fnx

constMPBCachedM ::
    String -> 
    (Precision -> MPBall) -> QACachedM QACached_MPBall
constMPBCachedM constName r =
    fmap QACached_MPBall $
        newId QAP_MPBall $ 
            (Just constName, \p -> (return $ r p))
            
unaryMPBCachedM ::
    String -> 
    (MPBall -> MPBall) -> 
    (QACached_MPBall) -> QACachedM QACached_MPBall
unaryMPBCachedM valName op (QACached_MPBall id1) =    
    fmap QACached_MPBall $ newId QAP_MPBall (Just valName, handleQuery)
    where
    handleQuery q =
        do
        a1 <- getAnswer QAP_MPBall (id1, q) 
        return $ op a1 

binaryMPBCachedM ::
    String -> 
    (MPBall -> MPBall -> MPBall) -> 
    (QACached_MPBall, QACached_MPBall) -> QACachedM QACached_MPBall
binaryMPBCachedM valName op (QACached_MPBall id1, QACached_MPBall id2) =    
    fmap QACached_MPBall $ newId QAP_MPBall (Just valName, handleQuery)
    where
    handleQuery q =
        do
        a1 <- getAnswer QAP_MPBall (id1, q)
        a2 <- getAnswer QAP_MPBall (id2, q) 
        return $ op a1 a2

relMPBCachedM ::
    String -> 
    ([MPBall] -> Maybe a) -> 
    [QACached_MPBall] -> QACachedM a
relMPBCachedM _valName rel rs =
    do
    aux standardPrecisions
    where
    aux (ac:rest) =
        do
        bs <- mapM (\(QACached_MPBall rId) -> getAnswer QAP_MPBall (rId, ac)) rs
        case rel bs of
            Just result -> return result
            Nothing -> aux rest
    aux [] =
        error "CauchyReal comparison undecided even using maximum standard accuracy"

binrelMPBCachedM ::
    String -> 
    ((MPBall, MPBall) -> Maybe a) -> 
    (QACached_MPBall, QACached_MPBall) -> QACachedM a
binrelMPBCachedM valName rel rs =
    relMPBCachedM valName (rel . list2pair) (pair2list rs)

list2pair :: (Show a) => [a] -> (a,a)
list2pair [a1,a2] = (a1,a2)
list2pair list = error $ "list2pair: not a pair: " ++ show list

pair2list :: (a,a) -> [a]
pair2list (a1,a2) = [a1,a2]
