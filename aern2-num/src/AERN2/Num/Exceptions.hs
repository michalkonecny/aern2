{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-|
    A wrapper type CatchingExceptions that tracks errors such as DivByZero
    instead of throwing an exception.
    
    For example:
    
        *AERN2.Net> 1 / (catchingExceptions $ mpBall 1)
        [1e+00 ± 0e+00]
        *AERN2.Net> 1 / (catchingExceptions $ mpBall 0)
        <no value>{![DivByZero]}
        *AERN2.Net> 1 / (catchingExceptions $ mpBall 0 + pi - pi)
        <no value>{?[DivByZero]}
-}
module AERN2.Num.Exceptions where

import AERN2.Num.Operations
import Control.Arrow
import Control.Applicative

import qualified Data.Set as Set

import AERN2.Num.Norm
import AERN2.Num.MPBall
import AERN2.Num.CauchyReal

data NumericalError =
    DivByZero | OutOfRange String | NumericalError String
    deriving (Eq, Ord, Show)

data CatchingExceptions t =
    CatchingExceptions 
    {
        catchingExceptions_maybeValue :: Maybe t,
        catchingExceptions_potentialErrors :: Set.Set NumericalError,
        catchingExceptions_certainErrors :: Set.Set NumericalError
    } 

catchingExceptions :: t -> CatchingExceptions t
catchingExceptions val = CatchingExceptions (Just val) Set.empty Set.empty

hasError :: CatchingExceptions t -> Bool
hasError (CatchingExceptions Nothing _ _) = True
hasError (CatchingExceptions _ pE cE) = not $ Set.null cE

filterNoException :: (Show t) => Integer -> Bool -> [CatchingExceptions t] -> [t]
filterNoException maxConsequentExceptions shouldErrorOnMaxReached maybeValues =
    aux maxConsequentExceptions maybeValues
    where
    aux _ ((CatchingExceptions (Just val) pE cE) : rest)
        | Set.null pE && Set.null cE = 
            val : aux maxConsequentExceptions rest
    aux n (c@(CatchingExceptions _ _ certainErrors) : rest)
        | Set.null certainErrors && n > 0 =
            aux (n - 1) rest
        | shouldErrorOnMaxReached =
            error $ "filterNoException: " ++ show c
        | otherwise =
            []
    aux _ [] = []

ifExceptionDie :: (Show t) => String -> CatchingExceptions t -> t
ifExceptionDie contextDescription c =
    case c of
        CatchingExceptions (Just v) _pE cE | Set.null cE -> v
        _ -> error $ contextDescription ++ ": " ++ show c

instance Show t => (Show (CatchingExceptions t))
    where
    show (CatchingExceptions maybeVal potentialErrors certainErrors)
        | Set.null potentialErrors && Set.null certainErrors =
            valS
        | Set.null certainErrors =
            valS ++ "{?" ++ show (Set.toList potentialErrors) ++ "}"
        | otherwise = -- if there are certain errors report only those; do not report potential errors:
            valS ++ "{!" ++ show (Set.toList certainErrors) ++ "}"
        where
        valS = case maybeVal of (Just val) -> show val; _ -> "<no value>"

instance Functor CatchingExceptions where
    fmap f (CatchingExceptions maybeVal pE cE) =
        CatchingExceptions (fmap f maybeVal) pE cE

gunzip :: (Functor f) => (f (a,b)) -> (f a, f b)
gunzip ab =(fmap fst ab, fmap snd ab)

zipCatchingExceptionsWith :: (a -> b -> c) -> (CatchingExceptions a) -> (CatchingExceptions b) -> (CatchingExceptions c)
zipCatchingExceptionsWith fn 
    (CatchingExceptions (Just a) pEf cEf) (CatchingExceptions (Just b) pE cE) =
        CatchingExceptions (Just (fn a b)) (Set.union pEf pE) (Set.union cEf cE)
zipCatchingExceptionsWith _ 
    (CatchingExceptions _ pEf cEf) (CatchingExceptions _ pE cE) =
        CatchingExceptions Nothing (Set.union pEf pE) (Set.union cEf cE)


instance Applicative CatchingExceptions where
    pure = catchingExceptions
    (<*>) = zipCatchingExceptionsWith ($)


instance (RingA to t) => RingA to (CatchingExceptions t)
instance (FieldA to t, CanTestZero t) => FieldA to (CatchingExceptions t)
instance (ArrowChoice to, CanAddMulScalarA to t t) => 
    CanAddMulScalarA to (CatchingExceptions t) (CatchingExceptions t)
instance (ArrowChoice to, CanAddMulDivScalarA to t t, CanTestZero t) => 
    CanAddMulDivScalarA to (CatchingExceptions t) (CatchingExceptions t)
instance (ArrowChoice to, CanAddMulScalarA to t Integer) =>
    CanAddMulScalarA to (CatchingExceptions t) Integer
instance (ArrowChoice to, CanAddMulDivScalarA to t Integer) =>
    CanAddMulDivScalarA to (CatchingExceptions t) Integer
instance (ArrowChoice to, CanAddMulScalarA to t Rational) =>
    CanAddMulScalarA to (CatchingExceptions t) Rational
instance (ArrowChoice to, CanAddMulDivScalarA to t Rational) =>
    CanAddMulDivScalarA to (CatchingExceptions t) Rational
instance (ArrowChoice to, CanAddMulScalarA to t CauchyReal) =>
    CanAddMulScalarA to (CatchingExceptions t) CauchyReal
instance (ArrowChoice to, CanAddMulDivScalarA to t CauchyReal) =>
    CanAddMulDivScalarA to (CatchingExceptions t) CauchyReal

{----- conversions and extraction operations -----}

instance 
    (ArrowChoice to, ConvertibleA to t1 t2) => 
    ConvertibleA to (CatchingExceptions t1) (CatchingExceptions t2) 
    where
    convertA = 
        proc (CatchingExceptions maybeVal pE cE) ->
            case maybeVal of
                Just val -> 
                    do
                    val' <- convertA -< val
                    returnA -< CatchingExceptions (Just val') pE cE
                _ ->
                    returnA -< CatchingExceptions Nothing pE cE

instance (Arrow to, ConvertibleA to Integer t) => ConvertibleA to Integer (CatchingExceptions t) where
    convertA = convertA >>> arr catchingExceptions 

instance (Arrow to, ConvertibleA to Rational t) => ConvertibleA to Rational (CatchingExceptions t) where
    convertA = convertA >>> arr catchingExceptions 

instance (Arrow to, ConvertibleA to MPBall t) => ConvertibleA to MPBall (CatchingExceptions t) where
    convertA = convertA >>> arr catchingExceptions 

instance (HasAccuracy t) => HasAccuracy (CatchingExceptions t) where
    getAccuracy (CatchingExceptions (Just val) _ certainErrors) 
        | Set.null certainErrors = getAccuracy val
        | otherwise = NoInformation
    getAccuracy (CatchingExceptions _ _ _) = NoInformation

instance (HasApproximate t) => HasApproximate (CatchingExceptions t) where
    type Approximate (CatchingExceptions t) = Approximate t
    getApproximate ac (CatchingExceptions (Just value) _ _) = getApproximate ac value
    getApproximate _ (CatchingExceptions _ pE cE) = 
        error $ "getApproximate: " ++ show (CatchingExceptions Nothing pE cE :: CatchingExceptions ())

instance (HasPrecision t) => HasPrecision (CatchingExceptions t) where
    getPrecision (CatchingExceptions (Just value) _ _) = getPrecision value
    getPrecision (CatchingExceptions _ pE cE) = 
        error $ "getPrecision: " ++ show (CatchingExceptions Nothing pE cE :: CatchingExceptions ())

instance (CanSetPrecision t) => CanSetPrecision (CatchingExceptions t) where
    setPrecision p (CatchingExceptions (Just value) pE cE) =
        CatchingExceptions (Just $ setPrecision p value) pE cE
    setPrecision _ c = c

instance (HasNorm t, Show t) => HasNorm (CatchingExceptions t) where
    getNormLog (CatchingExceptions (Just v) _ _) = getNormLog v
    getNormLog c = 
        error $ "getNormLog: " ++ show c

{----- order operations -----}

instance (ArrowChoice to, HasEqA to t1 t2) => 
    HasEqA to (CatchingExceptions t1) (CatchingExceptions t2) 
    where
    type EqCompareTypeA to (CatchingExceptions t1) (CatchingExceptions t2) = 
        Maybe (EqCompareTypeA to t1 t2)
    equalToA = liftCCtoO equalToA

instance (ArrowChoice to, HasOrderA to t1 t2) => 
    HasOrderA to (CatchingExceptions t1) (CatchingExceptions t2)
    where
    type OrderCompareTypeA to (CatchingExceptions t1) (CatchingExceptions t2) = 
        Maybe (OrderCompareTypeA to t1 t2)
    lessThanA = liftCCtoO lessThanA 
    leqA = liftCCtoO leqA 
    greaterThanA = liftCCtoO greaterThanA 
    geqA = liftCCtoO geqA 

instance (ArrowChoice to, CanMinMaxA to t1 t2) => 
    CanMinMaxA to (CatchingExceptions t1) (CatchingExceptions t2) 
    where
    type MinMaxTypeA to (CatchingExceptions t1) (CatchingExceptions t2) = CatchingExceptions (MinMaxTypeA to t1 t2)
    minA = liftCCtoC minA 
    maxA = liftCCtoC maxA

instance (ArrowChoice to, CanMinMaxThisA to t1 t2) => CanMinMaxThisA to (CatchingExceptions t1) (CatchingExceptions t2)
instance (ArrowChoice to, CanMinMaxSameTypeA to t) => CanMinMaxSameTypeA to (CatchingExceptions t)

{- TODO: comparisons with Integer and Rational -}

{----- elementary unary arithmetic operations -----}

instance (ArrowChoice to, CanNegA to t) => CanNegA to (CatchingExceptions t) where
    type NegTypeA to (CatchingExceptions t) = CatchingExceptions (NegTypeA to t) 
    negA = liftCtoC negA

instance (ArrowChoice to, CanNegSameTypeA to t) => CanNegSameTypeA to (CatchingExceptions t)

instance (ArrowChoice to, CanAbsA to t) => CanAbsA to (CatchingExceptions t) where
    type AbsTypeA to (CatchingExceptions t) = CatchingExceptions (AbsTypeA to t) 
    absA = liftCtoC absA

instance (ArrowChoice to, CanAbsSameTypeA to t) => CanAbsSameTypeA to (CatchingExceptions t)

instance (ArrowChoice to, CanRecipA to t) => CanRecipA to (CatchingExceptions t) where
    type RecipTypeA to (CatchingExceptions t) = CatchingExceptions (RecipTypeA to t) 
    recipA = liftCtoC recipA

instance (ArrowChoice to, CanRecipSameTypeA to t) => CanRecipSameTypeA to (CatchingExceptions t)

{----- elementary binary arithmetic operations -----}

instance (ArrowChoice to, CanAddA to t1 t2) => 
    CanAddA to (CatchingExceptions t1) (CatchingExceptions t2) 
    where
    type AddTypeA to (CatchingExceptions t1) (CatchingExceptions t2) = CatchingExceptions (AddTypeA to t1 t2)
    addA = liftCCtoC addA 

instance (ArrowChoice to, CanAddThisA to t1 t2) => CanAddThisA to (CatchingExceptions t1) (CatchingExceptions t2)
instance (ArrowChoice to, CanAddSameTypeA to t) => CanAddSameTypeA to (CatchingExceptions t)

instance (ArrowChoice to, CanSubA to t1 t2) => 
    CanSubA to (CatchingExceptions t1) (CatchingExceptions t2) 
    where
    type SubTypeA to (CatchingExceptions t1) (CatchingExceptions t2) = CatchingExceptions (SubTypeA to t1 t2)
    subA = liftCCtoC subA 

instance (ArrowChoice to, CanSubThisA to t1 t2) => CanSubThisA to (CatchingExceptions t1) (CatchingExceptions t2)
instance (ArrowChoice to, CanSubSameTypeA to t) => CanSubSameTypeA to (CatchingExceptions t)

instance (ArrowChoice to, CanMulA to t1 t2) => 
    CanMulA to (CatchingExceptions t1) (CatchingExceptions t2) 
    where
    type MulTypeA to (CatchingExceptions t1) (CatchingExceptions t2) = CatchingExceptions (MulTypeA to t1 t2)
    mulA = liftCCtoC mulA 

instance (ArrowChoice to, CanMulByA to t1 t2) => CanMulByA to (CatchingExceptions t1) (CatchingExceptions t2)
instance (ArrowChoice to, CanMulSameTypeA to t) => CanMulSameTypeA to (CatchingExceptions t)

instance (ArrowChoice to, CanPowA to t o) => CanPowA to (CatchingExceptions t) o where
    type PowTypeA to (CatchingExceptions t) o = CatchingExceptions (PowTypeA to t o)
    powA = liftCOtoC powA
instance (ArrowChoice to, CanPowSameTypeA to t o) => CanPowSameTypeA to (CatchingExceptions t) o

instance (ArrowChoice to, CanDivA to t1 t2, CanTestZero t2) => 
    CanDivA to (CatchingExceptions t1) (CatchingExceptions t2) 
    where
    type DivTypeA to (CatchingExceptions t1) (CatchingExceptions t2) = CatchingExceptions (DivTypeA to t1 t2)
    divA = liftCCtoCwithTest (\_a b -> testNonZero b) divA

testNonZero :: 
    CanTestZero t =>
    t -> Maybe (Set.Set NumericalError, Set.Set NumericalError)
testNonZero b = 
    case (isCertainlyZero b, isNonZero b) of
        (_, True) -> Nothing
        (True, _) -> Just (Set.empty, Set.singleton DivByZero) -- certain error
        _ -> Just (Set.singleton DivByZero, Set.empty) -- potential error

instance (ArrowChoice to, CanDivByA to t1 t2, CanTestZero t2) => CanDivByA to (CatchingExceptions t1) (CatchingExceptions t2)
instance (ArrowChoice to, CanDivSameTypeA to t, CanTestZero t) => CanDivSameTypeA to (CatchingExceptions t)

{----- elementary binary operations mixed with Integer -----}

instance (ArrowChoice to, CanAddA to t1 Integer) => 
    CanAddA to (CatchingExceptions t1) Integer 
    where
    type AddTypeA to (CatchingExceptions t1) Integer = CatchingExceptions (AddTypeA to t1 Integer)
    addA = liftCOtoC addA 

instance (ArrowChoice to, CanAddA to Integer t2) => 
    CanAddA to Integer (CatchingExceptions t2) 
    where
    type AddTypeA to Integer (CatchingExceptions t2) = CatchingExceptions (AddTypeA to Integer t2)
    addA = liftOCtoC addA 

instance (ArrowChoice to, CanAddThisA to t1 Integer) => CanAddThisA to (CatchingExceptions t1) Integer

instance (ArrowChoice to, CanSubA to t1 Integer) => 
    CanSubA to (CatchingExceptions t1) Integer 
    where
    type SubTypeA to (CatchingExceptions t1) Integer = CatchingExceptions (SubTypeA to t1 Integer)
    subA = liftCOtoC subA 

instance (ArrowChoice to, CanSubThisA to t1 Integer) => CanSubThisA to (CatchingExceptions t1) Integer

instance (ArrowChoice to, CanSubA to Integer t2) => 
    CanSubA to Integer (CatchingExceptions t2) 
    where
    type SubTypeA to Integer (CatchingExceptions t2) = CatchingExceptions (SubTypeA to Integer t2)
    subA = liftOCtoC subA 

instance (ArrowChoice to, CanMulA to t1 Integer) => 
    CanMulA to (CatchingExceptions t1) Integer 
    where
    type MulTypeA to (CatchingExceptions t1) Integer = CatchingExceptions (MulTypeA to t1 Integer)
    mulA = liftCOtoC mulA 

instance (ArrowChoice to, CanMulA to Integer t2) => 
    CanMulA to Integer (CatchingExceptions t2) 
    where
    type MulTypeA to Integer (CatchingExceptions t2) = CatchingExceptions (MulTypeA to Integer t2)
    mulA = liftOCtoC mulA 

instance (ArrowChoice to, CanMulByA to t1 Integer) => CanMulByA to (CatchingExceptions t1) Integer

instance (ArrowChoice to, CanDivA to t1 Integer) => 
    CanDivA to (CatchingExceptions t1) Integer 
    where
    type DivTypeA to (CatchingExceptions t1) Integer = CatchingExceptions (DivTypeA to t1 Integer)
    divA = liftCOtoC divA 

instance (ArrowChoice to, CanDivByA to t1 Integer) => CanDivByA to (CatchingExceptions t1) Integer

instance (ArrowChoice to, CanDivA to Integer t2, CanTestZero t2) => 
    CanDivA to Integer (CatchingExceptions t2) 
    where
    type DivTypeA to Integer (CatchingExceptions t2) = CatchingExceptions (DivTypeA to Integer t2)
    divA = liftOCtoCwithTest testNonZero divA 

{----- elementary binary operations mixed with Rational -----}

instance (ArrowChoice to, CanAddA to t1 Rational) => 
    CanAddA to (CatchingExceptions t1) Rational 
    where
    type AddTypeA to (CatchingExceptions t1) Rational = CatchingExceptions (AddTypeA to t1 Rational)
    addA = liftCOtoC addA 

instance (ArrowChoice to, CanAddA to Rational t2) => 
    CanAddA to Rational (CatchingExceptions t2) 
    where
    type AddTypeA to Rational (CatchingExceptions t2) = CatchingExceptions (AddTypeA to Rational t2)
    addA = liftOCtoC addA 

instance (ArrowChoice to, CanAddThisA to t1 Rational) => CanAddThisA to (CatchingExceptions t1) Rational

instance (ArrowChoice to, CanSubA to t1 Rational) => 
    CanSubA to (CatchingExceptions t1) Rational 
    where
    type SubTypeA to (CatchingExceptions t1) Rational = CatchingExceptions (SubTypeA to t1 Rational)
    subA = liftCOtoC subA 

instance (ArrowChoice to, CanSubThisA to t1 Rational) => CanSubThisA to (CatchingExceptions t1) Rational

instance (ArrowChoice to, CanSubA to Rational t2) => 
    CanSubA to Rational (CatchingExceptions t2) 
    where
    type SubTypeA to Rational (CatchingExceptions t2) = CatchingExceptions (SubTypeA to Rational t2)
    subA = liftOCtoC subA 

instance (ArrowChoice to, CanMulA to t1 Rational) => 
    CanMulA to (CatchingExceptions t1) Rational 
    where
    type MulTypeA to (CatchingExceptions t1) Rational = CatchingExceptions (MulTypeA to t1 Rational)
    mulA = liftCOtoC mulA 

instance (ArrowChoice to, CanMulA to Rational t2) => 
    CanMulA to Rational (CatchingExceptions t2) 
    where
    type MulTypeA to Rational (CatchingExceptions t2) = CatchingExceptions (MulTypeA to Rational t2)
    mulA = liftOCtoC mulA 

instance (ArrowChoice to, CanMulByA to t1 Rational) => CanMulByA to (CatchingExceptions t1) Rational

instance (ArrowChoice to, CanDivA to t1 Rational) => 
    CanDivA to (CatchingExceptions t1) Rational 
    where
    type DivTypeA to (CatchingExceptions t1) Rational = CatchingExceptions (DivTypeA to t1 Rational)
    divA = liftCOtoC divA 

instance (ArrowChoice to, CanDivByA to t1 Rational) => CanDivByA to (CatchingExceptions t1) Rational

instance (ArrowChoice to, CanDivA to Rational t2, CanTestZero t2) => 
    CanDivA to Rational (CatchingExceptions t2) 
    where
    type DivTypeA to Rational (CatchingExceptions t2) = CatchingExceptions (DivTypeA to Rational t2)
    divA = liftOCtoCwithTest testNonZero divA 

{----- elementary binary operations mixed with CauchyReal -----}

instance (ArrowChoice to, CanAddA to t1 CauchyReal) => 
    CanAddA to (CatchingExceptions t1) CauchyReal 
    where
    type AddTypeA to (CatchingExceptions t1) CauchyReal = CatchingExceptions (AddTypeA to t1 CauchyReal)
    addA = liftCOtoC addA 

instance (ArrowChoice to, CanAddA to CauchyReal t2) => 
    CanAddA to CauchyReal (CatchingExceptions t2) 
    where
    type AddTypeA to CauchyReal (CatchingExceptions t2) = CatchingExceptions (AddTypeA to CauchyReal t2)
    addA = liftOCtoC addA 

instance (ArrowChoice to, CanAddThisA to t1 CauchyReal) => CanAddThisA to (CatchingExceptions t1) CauchyReal

instance (ArrowChoice to, CanSubA to t1 CauchyReal) => 
    CanSubA to (CatchingExceptions t1) CauchyReal 
    where
    type SubTypeA to (CatchingExceptions t1) CauchyReal = CatchingExceptions (SubTypeA to t1 CauchyReal)
    subA = liftCOtoC subA 

instance (ArrowChoice to, CanSubThisA to t1 CauchyReal) => CanSubThisA to (CatchingExceptions t1) CauchyReal

instance (ArrowChoice to, CanSubA to CauchyReal t2) => 
    CanSubA to CauchyReal (CatchingExceptions t2) 
    where
    type SubTypeA to CauchyReal (CatchingExceptions t2) = CatchingExceptions (SubTypeA to CauchyReal t2)
    subA = liftOCtoC subA 

instance (ArrowChoice to, CanMulA to t1 CauchyReal) => 
    CanMulA to (CatchingExceptions t1) CauchyReal 
    where
    type MulTypeA to (CatchingExceptions t1) CauchyReal = CatchingExceptions (MulTypeA to t1 CauchyReal)
    mulA = liftCOtoC mulA 

instance (ArrowChoice to, CanMulA to CauchyReal t2) => 
    CanMulA to CauchyReal (CatchingExceptions t2) 
    where
    type MulTypeA to CauchyReal (CatchingExceptions t2) = CatchingExceptions (MulTypeA to CauchyReal t2)
    mulA = liftOCtoC mulA 

instance (ArrowChoice to, CanMulByA to t1 CauchyReal) => CanMulByA to (CatchingExceptions t1) CauchyReal

instance (ArrowChoice to, CanDivA to t1 CauchyReal) => 
    CanDivA to (CatchingExceptions t1) CauchyReal 
    where
    type DivTypeA to (CatchingExceptions t1) CauchyReal = CatchingExceptions (DivTypeA to t1 CauchyReal)
    divA = liftCOtoC divA 

instance (ArrowChoice to, CanDivByA to t1 CauchyReal) => CanDivByA to (CatchingExceptions t1) CauchyReal

instance (ArrowChoice to, CanDivA to CauchyReal t2, CanTestZero t2) => 
    CanDivA to CauchyReal (CatchingExceptions t2) 
    where
    type DivTypeA to CauchyReal (CatchingExceptions t2) = CatchingExceptions (DivTypeA to CauchyReal t2)
    divA = liftOCtoCwithTest testNonZero divA 

{----- auxiliary function -----}

liftCtoC ::
    (ArrowChoice to) => 
    (t1 `to` t) -> 
    ((CatchingExceptions t1) `to` (CatchingExceptions t))
liftCtoC fnA =
    proc (aC) ->
        case (aC) of
            (CatchingExceptions (Just a) pE1 cE1) ->
                do
                res <- fnA -< a
                returnA -< CatchingExceptions (Just res) pE1 cE1
            (CatchingExceptions _ pE1 cE1) -> 
                returnA -< CatchingExceptions Nothing pE1 cE1
                
liftCCtoC ::
    (ArrowChoice to) => 
    ((t1, t2) `to` t) -> 
    ((CatchingExceptions t1, CatchingExceptions t2) `to` (CatchingExceptions t))
liftCCtoC fnA =
    proc (aC,bC) ->
        case (aC,bC) of
            (CatchingExceptions (Just a) pE1 cE1, CatchingExceptions (Just b) pE2 cE2) ->
                do
                res <- fnA -< (a,b)
                returnA -< CatchingExceptions (Just res) (Set.union pE1 pE2) (Set.union cE1 cE2)
            (CatchingExceptions _ pE1 cE1, CatchingExceptions _ pE2 cE2) -> 
                returnA -< CatchingExceptions Nothing (Set.union pE1 pE2) (Set.union cE1 cE2)

liftCCtoCwithTest ::
    (ArrowChoice to) =>
    (t1 -> t2 -> Maybe (Set.Set NumericalError, Set.Set NumericalError)) -> 
    ((t1, t2) `to` t) -> 
    ((CatchingExceptions t1, CatchingExceptions t2) `to` (CatchingExceptions t))
liftCCtoCwithTest test fnA =
    proc (aC,bC) ->
        case (aC,bC) of
            (CatchingExceptions (Just a) pE1 cE1, CatchingExceptions (Just b) pE2 cE2) ->
                case test a b of
                    Just (pE, cE) ->
                        returnA -< CatchingExceptions Nothing (Set.unions [pE1, pE2, pE]) (Set.unions [cE1, cE2, cE]) 
                    _ -> 
                        do
                        res <- fnA -< (a,b)
                        returnA -< CatchingExceptions (Just res) (Set.union pE1 pE2) (Set.union cE1 cE2)
            (CatchingExceptions _ pE1 cE1, CatchingExceptions _ pE2 cE2) -> 
                returnA -< CatchingExceptions Nothing (Set.union pE1 pE2) (Set.union cE1 cE2)

liftCOtoC ::
    (ArrowChoice to) => 
    ((t1, o) `to` t) -> 
    ((CatchingExceptions t1, o) `to` (CatchingExceptions t))
liftCOtoC fnA =
    proc (aC,b) ->
        case aC of
            (CatchingExceptions (Just a) pE1 cE1) ->
                do
                res <- fnA -< (a,b)
                returnA -< CatchingExceptions (Just res) pE1 cE1
            (CatchingExceptions _ pE1 cE1) ->
                returnA -< CatchingExceptions Nothing pE1 cE1

liftOCtoC ::
    (ArrowChoice to) => 
    ((o, t2) `to` t) -> 
    ((o, CatchingExceptions t2) `to` (CatchingExceptions t))
liftOCtoC fnA =
    proc (a,bC) ->
        case bC of
            (CatchingExceptions (Just b) pE1 cE1) ->
                do
                res <- fnA -< (a,b)
                returnA -< CatchingExceptions (Just res) pE1 cE1
            (CatchingExceptions _ pE1 cE1) ->
                returnA -< CatchingExceptions Nothing pE1 cE1

liftOCtoCwithTest ::
    (ArrowChoice to) => 
    (t2 -> Maybe (Set.Set NumericalError, Set.Set NumericalError)) -> 
    ((o, t2) `to` t) -> 
    ((o, CatchingExceptions t2) `to` (CatchingExceptions t))
liftOCtoCwithTest test fnA =
    proc (a,bC) ->
        case bC of
            (CatchingExceptions (Just b) pE1 cE1) ->
                case test b of
                    Just (pE, cE) ->
                        returnA -< CatchingExceptions Nothing (Set.unions [pE1, pE]) (Set.unions [cE1, cE]) 
                    _ -> 
                        do
                        res <- fnA -< (a,b)
                        returnA -< CatchingExceptions (Just res) pE1 cE1
            (CatchingExceptions _ pE1 cE1) ->
                returnA -< CatchingExceptions Nothing pE1 cE1

liftCCtoO ::
    (ArrowChoice to) => 
    ((t1, t2) `to` o) -> 
    ((CatchingExceptions t1, CatchingExceptions t2) `to` (Maybe o))
liftCCtoO fnA =
    proc (aC,bC) ->
        case (aC,bC) of
            (CatchingExceptions (Just a) _ _, CatchingExceptions (Just b) _ _) ->
                do
                res <- fnA -< (a,b)
                returnA -< Just res
            _ -> 
                returnA -< Nothing 

