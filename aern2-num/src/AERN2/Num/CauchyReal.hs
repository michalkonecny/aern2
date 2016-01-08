{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances, TypeOperators, ConstraintKinds, Arrows #-}
module AERN2.Num.CauchyReal
(
    AsCauchyReal(..), CauchyReal_, CauchyReal,  
    CanReadAsCauchyRealA(..), CanCreateAsCauchyRealA(..), CanAsCauchyRealA,
    SupportsSenderIdA(..), HasSenderIdA(..),
    CanCombineCRsA(..), CanCombineCRwithA,
    showCauchyReal,
    mapCauchyRealUnsafe,
    cauchyReal2ball,
    cauchyRealName,
    HasCauchyRealsA, HasCauchyReals,
    CanBeCauchyRealA, cauchyRealA, cauchyRealNamedA, cauchyRealsA, cauchyRealsNamedA, CanBeCauchyReal, cauchyReal, cauchyReals,
    integer2CauchyReal, rational2CauchyReal,
    convergent2CauchyReal,
    compareTryAccuracies,
    ensureAccuracyA2, ensureAccuracyA1, 
    pi
)
where

import AERN2.Num.Operations
import qualified Prelude

import Control.Arrow

import AERN2.Num.Interval

import AERN2.Num.Norm
import AERN2.Num.Accuracy

import AERN2.Num.MPBall
import AERN2.Num.IntegerRational ()

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id


type CauchyReal = AsCauchyReal CauchyReal_

data CauchyReal_ = 
    CauchyReal_ { cr_name :: Maybe String, cr_seq :: Accuracy -> MPBall } 

newtype AsCauchyReal r = AsCauchyReal { unAsCauchyReal :: r }

cauchyReal2ball :: CauchyReal -> Accuracy -> MPBall
cauchyReal2ball = cr_seq . unAsCauchyReal

cauchyRealName :: CauchyReal -> Maybe String
cauchyRealName = cr_name . unAsCauchyReal

showCauchyReal :: Accuracy -> CauchyReal -> String
showCauchyReal a r = show (cauchyReal2ball r a)

mapCauchyRealUnsafe :: (Accuracy -> MPBall -> MPBall) -> CauchyReal -> CauchyReal
mapCauchyRealUnsafe f (AsCauchyReal (CauchyReal_ name sq)) = 
    AsCauchyReal (CauchyReal_ name (\ ac -> f ac (sq ac) )) 

convergent2CauchyReal :: 
    Maybe String -> [MPBall] -> CauchyReal
convergent2CauchyReal name convergentSeq =
    AsCauchyReal $ CauchyReal_ name sq
    where
    sq i =
        findAccurate convergentSeq
        where
        findAccurate [] =
            error "convergent2CauchyReal: the sequence either converges too slowly or it does not converge"
        findAccurate (b : rest)
            | getAccuracy b >= i = b
            | otherwise = findAccurate rest

seqByPrecision2Cauchy :: 
    Maybe String -> (Precision -> MPBall) -> CauchyReal
seqByPrecision2Cauchy name seqByPrecision =
    AsCauchyReal $ CauchyReal_ name $
        seqByPrecision2CauchySeq seqByPrecision

seqByPrecision2CauchySeq :: 
    (Precision -> MPBall) -> (Accuracy -> MPBall)
seqByPrecision2CauchySeq seqByPrecision i =
    findAccurate $ map seqByPrecision $ dropWhile lowPrec standardPrecisions
    where
    lowPrec p = 
        case i of 
            Exact -> False
            _ -> bits (prec2integer p) < i
    findAccurate [] =
        error "seqByPrecision2CauchySeq: the sequence either converges too slowly or it does not converge"
    findAccurate (b : rest)
        | getAccuracy b >= i = b
        | otherwise = findAccurate rest

pi :: CauchyReal
pi = seqByPrecision2Cauchy (Just "pi") (\ p -> piBallP p)


{- Arrow class for arrow-generic CauchyReal operations -}

class (CanReadAsCauchyRealA to r, CanCreateAsCauchyRealA to r) => CanAsCauchyRealA to r

-- TODO: Make the following classes specialisations of general QA classes, evolution of QAProtocol
{-| Invariant: For any instance it should hold: @width(getAnswerCRA i) <= 2^^(-i)@ -}
class (ArrowChoice to, HasSenderIdA to r) => CanReadAsCauchyRealA to r where
    getAnswerCRA :: (AsCauchyReal r,Accuracy) `to` MPBall
    getNameCRA :: AsCauchyReal r `to` Maybe String

class (ArrowChoice to, SupportsSenderIdA to r) => CanCreateAsCauchyRealA to r where
    newCRA :: ([SenderId to r], Maybe String, Accuracy `to` MPBall) `to` AsCauchyReal r

-- TODO: this should move to a general QA module
class (ArrowLoop to) => SupportsSenderIdA to r where
    type SenderId to r

class (SupportsSenderIdA to r) => HasSenderIdA to r where
    getSenderIdA :: r `to` (SenderId to r)


    
instance CanAsCauchyRealA (->) CauchyReal_

instance (ArrowChoice to, ArrowLoop to) => CanReadAsCauchyRealA to CauchyReal_ where
    getAnswerCRA = arr $ \ (AsCauchyReal r,ac) -> cr_seq r ac
    getNameCRA = arr $ cr_name . unAsCauchyReal 

instance CanCreateAsCauchyRealA (->) CauchyReal_ where
    newCRA (_, name, ac2b) = AsCauchyReal $ CauchyReal_ name ac2b

instance (ArrowLoop to) => SupportsSenderIdA to CauchyReal_ where
    type SenderId to CauchyReal_ = ()
instance (ArrowLoop to) => HasSenderIdA to CauchyReal_ where
    getSenderIdA = arr $ const ()

instance SupportsSenderIdA to r => SupportsSenderIdA to (AsCauchyReal r) where
    type SenderId to (AsCauchyReal r) = SenderId to r
instance HasSenderIdA to r => HasSenderIdA to (AsCauchyReal r) where
    getSenderIdA = proc (AsCauchyReal r) -> getSenderIdA -< r


class 
    (CanReadAsCauchyRealA to r1, CanReadAsCauchyRealA to r2, 
    CanCreateAsCauchyRealA to (CombinedCRs to r1 r2)) 
    => 
    CanCombineCRsA to r1 r2 
    where
    type CombinedCRs to r1 r2
    getSourcesOfCombinedCRs :: 
        (AsCauchyReal r1, AsCauchyReal r2) `to` [SenderId to (CombinedCRs to r1 r2)]  
    

class
    (CanCombineCRsA to r1 r2, CombinedCRs to r1 r2 ~ r1,
     CanCombineCRsA to r2 r1, CombinedCRs to r2 r1 ~ r1,
     CanAsCauchyRealA to r1)
    =>
    CanCombineCRwithA to r1 r2 

instance CanCombineCRsA (->) CauchyReal_ CauchyReal_ where
    type CombinedCRs (->) CauchyReal_ CauchyReal_ = CauchyReal_
    getSourcesOfCombinedCRs = const []

instance CanCombineCRwithA (->) CauchyReal_ CauchyReal_

{- conversions -}

--instance (CanAsCauchyRealA to r) => ConvertibleA to (AsCauchyReal r) (AsCauchyReal r) where 
--    convertA = id
--    convertListA = id

type HasCauchyRealsA to = ConvertibleA to CauchyReal
type HasCauchyReals = HasCauchyRealsA (->)

type CanBeCauchyRealA to a = ConvertibleA to a CauchyReal
cauchyRealA :: (CanBeCauchyRealA to a) => a `to` CauchyReal
cauchyRealA = convertA
cauchyRealNamedA :: (CanBeCauchyRealA to a) => String -> a `to` CauchyReal
cauchyRealNamedA = convertNamedA
cauchyRealsA :: (CanBeCauchyRealA to a) => [a] `to` [CauchyReal]
cauchyRealsA = convertListA
cauchyRealsNamedA :: (CanBeCauchyRealA to a) => String -> [a] `to` [CauchyReal]
cauchyRealsNamedA = convertListNamedA
type CanBeCauchyReal a = CanBeCauchyRealA (->) a
cauchyReal :: (CanBeCauchyReal a) => a -> CauchyReal
cauchyReal = convert
cauchyReals :: (CanBeCauchyReal a) => [a] -> [CauchyReal]
cauchyReals = convertList

-- | HasIntegers CauchyReal, CanBeCauchyReal Integer
instance (CanAsCauchyRealA to r, SupportsSenderIdA to r) => ConvertibleA to Integer (AsCauchyReal r) where
    convertA = proc n ->
        newCRA -< ([], Just $ show n, arr $ seqByPrecision2CauchySeq $ \ p -> integer2BallP p n)
    convertNamedA name = proc n ->
        newCRA -< ([], Just name, arr $ seqByPrecision2CauchySeq $ \ p -> integer2BallP p n)

integer2CauchyReal :: Integer -> CauchyReal
integer2CauchyReal = convert


-- | HasRationals CauchyReal, CanBeCauchyReal Rational
instance (CanAsCauchyRealA to r, SupportsSenderIdA to r) => ConvertibleA to Rational (AsCauchyReal r) where
    convertA = proc n ->
        newCRA -< ([], Just $ show n, arr $ seqByPrecision2CauchySeq $ \ p -> rational2BallP p n)
    convertNamedA name = proc n ->
        newCRA -< ([], Just name, arr $ seqByPrecision2CauchySeq $ \ p -> rational2BallP p n) 

rational2CauchyReal :: Rational -> CauchyReal
rational2CauchyReal = convert

instance 
    (CanReadAsCauchyRealA to r1,
     CanCreateAsCauchyRealA to r2) 
    => 
    ConvertibleA to (AsCauchyReal r1) (AsCauchyReal r2) 
    where
    convertA = proc r1 ->
        do
        name <- getNameCRA -< r1
        newCRA -< ([], name, proc ac -> getAnswerCRA -< (r1,ac))
    convertNamedA name = proc r1 ->
        do
        newCRA -< ([], Just name, proc ac -> getAnswerCRA -< (r1,ac))

{- Comparisons of CauchyReals -}

compareTryAccuracies :: [Accuracy]
compareTryAccuracies =
    map bits $ aux 8 13
    where
    aux j j' 
        | j <= maximumCompareAccuracy = j : (aux j' (j+j'))
        | otherwise = []

maximumCompareAccuracy :: Integer
maximumCompareAccuracy = 10000

tryStandardCompareAccuracies ::
   (CanAsCauchyRealA to r) => 
   ([MPBall] -> Maybe t, [AsCauchyReal r]) `to` t
tryStandardCompareAccuracies =
    proc (rel, rs) -> 
        aux compareTryAccuracies -< (rel, rs)
    where
    aux (ac : rest) =
        proc (rel, rs) ->
            do
            bs <- mapA getAnswerCRA -< map (flip (,) ac) rs
            case rel bs of
                Just tv -> returnA -< tv
                Nothing -> aux rest -< (rel,rs)
    aux [] =
        error "CauchyReal comparison undecided even using maximum standard accuracy"

instance (CanAsCauchyRealA to r1, CanAsCauchyRealA to r2, SenderId to r2 ~ SenderId to r1) => 
    HasEqA to (AsCauchyReal r1) (AsCauchyReal r2) 
    where
    equalToA = 
        convertFirstA $
        proc (r1,r2) ->
            tryStandardCompareAccuracies -< (\[b1,b2] -> b1 == b2, [r1,r2])

instance (CanAsCauchyRealA to r1, CanAsCauchyRealA to r2, SenderId to r2 ~ SenderId to r1) => 
    HasOrderA to (AsCauchyReal r1) (AsCauchyReal r2) 
    where
    lessThanA =
        convertFirstA $ 
        proc (r1,r2) ->
            tryStandardCompareAccuracies -< (\[b1,b2] -> b1 < b2, [r1,r2])
    leqA = 
        convertFirstA $ 
        proc (r1,r2) ->
            tryStandardCompareAccuracies -< (\[b1,b2] -> b1 <= b2, [r1,r2])

instance (CanAsCauchyRealA to r) => HasParallelComparisonsA to (AsCauchyReal r) where
    pickNonZeroA = proc rvs ->
        tryStandardCompareAccuracies -< (findNonZero rvs, map fst rvs)
        where
        findNonZero rvs bs =
            aux True $ zip bs rvs
            where
            aux False [] = Nothing
            aux True [] = Just Nothing
            aux allFalse ((b, result) : rest) =
                case b /= 0 of
                    Just True -> Just (Just result)
                    Just False -> aux allFalse rest
                    Nothing -> aux False rest

instance (CanAsCauchyRealA to r) => HasEqA to Integer (AsCauchyReal r) where
    equalToA = convertFirstA equalToA 

instance (CanAsCauchyRealA to r) => HasOrderA to Integer (AsCauchyReal r) where
    lessThanA = convertFirstA lessThanA
    leqA = convertFirstA leqA
    
instance (CanAsCauchyRealA to r) => HasEqA to (AsCauchyReal r) Integer where
    equalToA = convertSecondA equalToA 

instance (CanAsCauchyRealA to r) => HasOrderA to (AsCauchyReal r) Integer where
    lessThanA = convertSecondA lessThanA
    leqA = convertSecondA leqA
    
instance (CanAsCauchyRealA to r) => HasEqA to Rational (AsCauchyReal r) where
    equalToA = convertFirstA equalToA 

instance (CanAsCauchyRealA to r) => HasOrderA to Rational (AsCauchyReal r) where
    lessThanA = convertFirstA lessThanA
    leqA = convertFirstA leqA
    
instance (CanAsCauchyRealA to r) => HasEqA to (AsCauchyReal r) Rational where
    equalToA = convertSecondA equalToA 

instance (CanAsCauchyRealA to r) => HasOrderA to (AsCauchyReal r) Rational where
    lessThanA = convertSecondA lessThanA
    leqA = convertSecondA leqA

{- Operations among CauchyReal's -}

-- providing this only because Template Haskell translates (-x) to (Prelude.negate x)  
instance Num CauchyReal where
    fromInteger = convert
    negate = negate
    (+) = (+)
    (*) = (*)
    abs = error "RealExpr Prelude.abs not implemented"
    signum = error "RealExpr Prelude.signum not implemented"

instance 
    (CanCombineCRwithA to r r) => 
    RingA to (AsCauchyReal r)

instance 
    (CanCombineCRwithA to r r) => 
    FieldA to (AsCauchyReal r)

instance 
    (CanCombineCRwithA to r1 r2) => 
    CanAddMulScalarA to (AsCauchyReal r1) (AsCauchyReal r2)
instance 
    (CanCombineCRwithA to r1 r2) 
    => 
    CanAddMulDivScalarA to (AsCauchyReal r1) (AsCauchyReal r2)

unaryOp ::
    (CanAsCauchyRealA to r1, CanAsCauchyRealA to r, SenderId to r1 ~ SenderId to r) 
    =>
    String ->
    (MPBall -> MPBall) -> 
    ((Accuracy, AsCauchyReal r1) `to` (Accuracy, Maybe MPBall)) -> 
    (AsCauchyReal r1) `to` (AsCauchyReal r)
unaryOp name op getInitQ1 =
    proc r1 ->
        do
        r1Id <- getSenderIdA -< r1
        newCRA -< ([r1Id], Just name, ac2b r1)
    where
    ac2b r1 = proc ac ->
        do
        q1InitMB <- getInitQ1 -< (ac, r1)
        ensureAccuracyA1 getA1 op -< (ac, q1InitMB)
        where
        getA1 =
            proc q1 -> getAnswerCRA -< (r1,q1)

unaryOpWithPureArg ::
    (CanReadAsCauchyRealA to r1, CanCreateAsCauchyRealA to r, 
     SenderId to r1 ~ SenderId to r) 
    =>
    String ->
    (MPBall -> t -> MPBall) -> 
    ((Accuracy, AsCauchyReal r1, t) `to` (Accuracy, Maybe MPBall)) -> 
    (AsCauchyReal r1, t) `to` (AsCauchyReal r)
unaryOpWithPureArg name op getInitQ1T =
    proc (r1, t) ->
        do
        r1Id <- getSenderIdA -< r1
        newCRA -< ([r1Id], Just name, ac2b r1 t)
    where
    ac2b r1 t = proc ac ->
        do
        q1InitMB <- getInitQ1T -< (ac, r1, t)
        ensureAccuracyA1 getA1 (flip op t) -< (ac, q1InitMB)
        where
        getA1 =
            proc q1 -> getAnswerCRA -< (r1,q1)

binaryOp ::
    (CanReadAsCauchyRealA to r1, CanReadAsCauchyRealA to r2, 
     CanCombineCRsA to r1 r2) 
    =>
    String ->
    (MPBall -> MPBall -> MPBall) -> 
    ((Accuracy, AsCauchyReal r1, AsCauchyReal r2) `to` ((Accuracy, Maybe MPBall), (Accuracy, Maybe MPBall))) -> 
    (AsCauchyReal r1, AsCauchyReal r2) `to` (AsCauchyReal (CombinedCRs to r1 r2))
binaryOp name op getInitQ1Q2 =
    proc (r1, r2) ->
        do
        sources <- getSourcesOfCombinedCRs -< (r1, r2)
        newCRA -< (sources, Just name, ac2b r1 r2)
    where
    ac2b r1 r2 = proc ac ->
        do
        (q1InitMB, q2InitMB) <- getInitQ1Q2 -< (ac,r1,r2)
        ensureAccuracyA2 getA1 getA2 op -< (ac, q1InitMB, q2InitMB)
        where
        getA1 =
            proc q1 -> getAnswerCRA -< (r1,q1)
        getA2 =
            proc q2 -> getAnswerCRA -< (r2,q2)

getInitQ1FromSimple ::
    Arrow to =>
    Accuracy `to` q -> 
    (Accuracy, r1) `to` (q, Maybe MPBall)
getInitQ1FromSimple simpleA  = proc (q, _) ->
    do 
    initQ <- simpleA -< q
    returnA -< (initQ, Nothing)

getInitQ1TFromSimple ::
    Arrow to =>
    Accuracy `to` q -> 
    (Accuracy, r1, t) `to` (q, Maybe MPBall)
getInitQ1TFromSimple simpleA  = proc (q, _, _) ->
    do 
    initQ <- simpleA -< q
    returnA -< (initQ, Nothing)

getInitQ1Q2FromSimple ::
    Arrow to =>
    Accuracy `to` (q,q) -> 
    (Accuracy, r1, r2) `to` ((q, Maybe MPBall), (q, Maybe MPBall))
getInitQ1Q2FromSimple simpleA  = proc (q, _, _) ->
    do 
    (initQ1, initQ2) <- simpleA -< q
    returnA -< ((initQ1, Nothing), (initQ2, Nothing))

getCRFnNormLog :: 
    (CanReadAsCauchyRealA to r) => (AsCauchyReal r, Accuracy, MPBall -> MPBall) `to` (NormLog, MPBall)
getCRFnNormLog = proc (r,q,fn) ->
    do
    b <- getAnswerCRA -< (r, q)
    returnA -< (getNormLog (fn b), b)
-- the following seems to be wasteful unless the cost of asking grows very fast with accuracy:
--    b0 <- getAnswerCRA -< (r, bits 0)
--    case 1 < b0 of
--        Just True -> returnA -< (getNormLog (fn b0), b0)
--        _ ->
--            do
--            b <- getAnswerCRA -< (r, q) 
--            returnA -< (getNormLog (fn b), b)

{-

Typically ensureAccuracy1 is called with a j such that the result is of
accuracy >= i.  In some cases j needs to be slightly increased.  

For example (using old version of ensureAccuracy1):

*AERN2.Num.Examples> cauchyReal2ball (10 * pi) 138
ensureAccuracy1: i = 138; j = 141; result accuracy = 137
ensureAccuracy1: i = 138; j = 142; result accuracy = 137
ensureAccuracy1: i = 138; j = 143; result accuracy = 226
[31.41592653589793 ± 5.216071149404186e-69]

*AERN2.Num.Examples> cauchyReal2ball (pi / 10) 56
ensureAccuracy1: i = 56; j = 53; result accuracy = 55
ensureAccuracy1: i = 56; j = 54; result accuracy = 89
[3.141592653589793e-1 ± 1.454028420503369e-27]

-}

ensureAccuracyA1 ::
    (ArrowChoice to) =>
    (Accuracy `to` MPBall) ->
    (MPBall -> MPBall) ->
    ((Accuracy, (Accuracy, Maybe MPBall)) `to` MPBall)
ensureAccuracyA1 getA1 op = 
    proc (q,(j1, mB)) ->
        do
        let mResult = fmap op mB
        case mResult of
            Just result | getAccuracy result >= q -> 
                returnA -< 
                    maybeTrace (
                        "ensureAccuracy1: Pre-computed result sufficient. (q = " ++ show q ++ 
                        "; j1 = " ++ show j1 ++ 
                        "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                    ) $ 
                    result
            _ -> aux -< (q,j1)
    where
    aux =
        proc (q,j1) ->
            do
            a1 <- getA1 -< j1
            let result = op a1
            if getAccuracy result >= q
                then returnA -< 
                    maybeTrace (
                        "ensureAccuracy1: Succeeded. (q = " ++ show q ++ 
                        "; j1 = " ++ show j1 ++ 
                        "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                    ) $ 
                    result
                else aux -< 
                    maybeTrace (
                        "ensureAccuracy1: Not enough ... (q = " ++ show q ++ 
                        "; j1 = " ++ show j1 ++ 
                        "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                    ) $ 
                    (q, j1+1)

ensureAccuracyA2 ::
    (ArrowChoice to) =>
    (Accuracy `to` MPBall) ->
    (Accuracy `to` MPBall) ->
    (MPBall -> MPBall -> MPBall) ->
    ((Accuracy, (Accuracy, Maybe MPBall), (Accuracy, Maybe MPBall)) `to` MPBall)
ensureAccuracyA2 getA1 getA2 op =
    proc (q,(j1, mB1),(j2, mB2)) ->
        do
        let mResult = do b1 <- mB1; b2 <- mB2; Just $ op b1 b2
        case mResult of
            Just result | getAccuracy result >= q -> 
                returnA -< 
                    maybeTrace (
                        "ensureAccuracy2: Pre-computed result sufficient. (q = " ++ show q ++ 
                        "; j1 = " ++ show j1 ++ 
                        "; j2 = " ++ show j2 ++ 
                        "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                    ) $ 
                result
            _ -> aux -< (q,j1,j2)
    where
    aux =
        proc (q, j1, j2) ->
            do
            a1 <- getA1 -< j1
            a2 <- getA2 -< j2
            let result = op a1 a2 
            if getAccuracy result >= q
                then returnA -< 
                    maybeTrace (
                        "ensureAccuracy2: Succeeded. (q = " ++ show q ++ 
                        "; j1 = " ++ show j1 ++ 
                        "; j2 = " ++ show j2 ++ 
                        "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                    ) $ 
                    result
                else aux -< 
                    maybeTrace (
                        "ensureAccuracy2: Not enough ... (q = " ++ show q ++ 
                        "; j1 = " ++ show j1 ++ 
                        "; j2 = " ++ show j2 ++ 
                        "; result accuracy = " ++ (show $ getAccuracy result) ++ ")"
                    ) $ 
                    (q,j1+1,j2+1)


instance (CanAsCauchyRealA to r) => CanNegA to (AsCauchyReal r) where
    negA = unaryOp "neg" neg (getInitQ1FromSimple id)
    
instance (CanAsCauchyRealA to r) => CanNegSameTypeA to (AsCauchyReal r)

instance (CanAsCauchyRealA to r) => CanAbsA to (AsCauchyReal r) where
    absA = unaryOp "abs" abs (getInitQ1FromSimple id)

instance (CanAsCauchyRealA to r) => CanAbsSameTypeA to (AsCauchyReal r)

instance (CanAsCauchyRealA to r) => CanRecipA to (AsCauchyReal r) where
    recipA = proc a -> divA -< (1, a)

instance (CanAsCauchyRealA to r) => CanRecipSameTypeA to (AsCauchyReal r)

instance 
    (CanReadAsCauchyRealA to r1, CanReadAsCauchyRealA to r2,
     CanCombineCRsA to r1 r2) 
    => 
    CanAddA to (AsCauchyReal r1) (AsCauchyReal r2)
    where
    type AddTypeA to (AsCauchyReal r1) (AsCauchyReal r2) = AsCauchyReal (CombinedCRs to r1 r2)
    addA = binaryOp "+" add (getInitQ1Q2FromSimple $ proc q -> returnA -< (q,q))

instance 
    (CanAsCauchyRealA to r1, CanReadAsCauchyRealA to r2, CanCombineCRwithA to r1 r2) => 
    CanAddThisA to (AsCauchyReal r1) (AsCauchyReal r2)
instance 
    (CanAsCauchyRealA to r, CanCombineCRwithA to r r) => 
    CanAddSameTypeA to (AsCauchyReal r)

instance
    (CanReadAsCauchyRealA to r1, CanReadAsCauchyRealA to r2,
     CanCombineCRsA to r1 r2) 
    => 
    CanSubA to (AsCauchyReal r1) (AsCauchyReal r2) 
    where
    type SubTypeA to (AsCauchyReal r1) (AsCauchyReal r2) = AsCauchyReal (CombinedCRs to r1 r2)
    subA = binaryOp "-" sub (getInitQ1Q2FromSimple $ proc q -> returnA -< (q,q))

instance 
    (CanAsCauchyRealA to r1, CanReadAsCauchyRealA to r2, 
     CanCombineCRwithA to r1 r2) 
    => 
    CanSubThisA to (AsCauchyReal r1) (AsCauchyReal r2)
instance (CanAsCauchyRealA to r, CanCombineCRwithA to r r) => 
    CanSubSameTypeA to (AsCauchyReal r)


instance
    (CanReadAsCauchyRealA to r1, CanReadAsCauchyRealA to r2, 
     CanCombineCRsA to r1 r2) 
    => 
    CanMulA to (AsCauchyReal r1) (AsCauchyReal r2) 
    where
    type MulTypeA to (AsCauchyReal r1) (AsCauchyReal r2) = AsCauchyReal (CombinedCRs to r1 r2)
    mulA = binaryOp "*" mul getInitQ1Q2 
        where
        getInitQ1Q2 =
            proc (q, a1, a2) ->
                do
                (a1NormLog, b1) <- getCRFnNormLog -< (a1,q,id)
                (a2NormLog, b2) <- getCRFnNormLog -< (a2,q,id)
                let jInit1 = case a2NormLog of 
                        NormBits a2NL -> max (bits 0) (q + a2NL + 1)
                        NormZero -> bits 0
                let jInit2 = case a1NormLog of 
                        NormBits a1NL -> max (bits 0) (q + a1NL + 1)
                        NormZero -> bits 0
                returnA -< ((jInit1, Just b1), (jInit2, Just b2))

instance 
    (CanAsCauchyRealA to r1, CanReadAsCauchyRealA to r2, 
     CanCombineCRwithA to r1 r2) 
    => 
    CanMulByA to (AsCauchyReal r1) (AsCauchyReal r2) 
instance 
    (CanAsCauchyRealA to r, CanCombineCRwithA to r r) => 
    CanMulSameTypeA to (AsCauchyReal r) 

instance
    (CanReadAsCauchyRealA to r1, CanReadAsCauchyRealA to r2,
     CanCombineCRsA to r1 r2) 
    => 
    CanDivA to (AsCauchyReal r1) (AsCauchyReal r2) 
    where
    type DivTypeA to (AsCauchyReal r1) (AsCauchyReal r2) = AsCauchyReal (CombinedCRs to r1 r2)
    divA = binaryOp "/" div getInitQ1Q2 
        where
        getInitQ1Q2 =
            proc (q, a1, a2) ->
                do
                (a1NormLog, b1) <- getCRFnNormLog -< (a1,q,id)
                (a2NormLog, b2) <- getCRFnNormLog -< (a2,q,id)
                let jInit1 = case a2NormLog of 
                        NormBits a2NL -> max 0 (q - a2NL + 1)
                        NormZero -> bits 0 -- denominator == 0, we have no chance...
                let jInit2 = case (a1NormLog, a2NormLog) of
                        (_, NormZero) -> bits 0 -- denominator == 0, we have no chance... 
                        (NormZero, _) -> bits 0 -- numerator == 0, it does not matter 
                        (NormBits a1NL, NormBits a2NL) -> max 0 (q + a1NL + 1 - 2 * a2NL)
                returnA -< ((jInit1, Just b1), (jInit2, Just b2))


instance 
    (CanAsCauchyRealA to r1, CanReadAsCauchyRealA to r2, 
     CanCombineCRwithA to r1 r2) 
    => 
    CanDivByA to (AsCauchyReal r1) (AsCauchyReal r2) 
instance 
    (CanAsCauchyRealA to r, CanCombineCRwithA to r r) => 
    CanDivSameTypeA to (AsCauchyReal r) 

instance (CanAsCauchyRealA to r) => CanSqrtA to (AsCauchyReal r) where
    sqrtA = unaryOp "sqrt" sqrt getInitQ1
        where
        getInitQ1 =
            proc (q, a1) ->
                do
                (a1NormLog, b) <- getCRFnNormLog -< (a1,q, sqrt)
                let jInit = case a1NormLog of
                        NormBits sqrtNormLog -> max 0 (q - 1 - sqrtNormLog)
                        NormZero -> q
                returnA -< (jInit, Just b)



instance (CanAsCauchyRealA to r) => CanSqrtSameTypeA to (AsCauchyReal r)

instance (CanAsCauchyRealA to r) => CanExpA to (AsCauchyReal r) where
    expA = unaryOp "exp" exp getInitQ1
        where
        getInitQ1 =
            proc (q, a1) ->
                do
                (a1NormLog, b) <- getCRFnNormLog -< (a1,q, exp)
                let jInit = case a1NormLog of
                        NormBits expNormLog -> q + expNormLog + 1
                        NormZero -> q -- this should never happen
                returnA -< (jInit, Just b)

instance (CanAsCauchyRealA to r) => CanExpSameTypeA to (AsCauchyReal r)


instance (CanAsCauchyRealA to r) => CanSineCosineA to (AsCauchyReal r) where
    sinA = unaryOp "sin" sin (getInitQ1FromSimple id)
    cosA = unaryOp "cos" cos (getInitQ1FromSimple id)

instance (CanAsCauchyRealA to r) => CanSineCosineSameTypeA to (AsCauchyReal r)



{- CauchyReal-Integer operations -}

instance (CanAsCauchyRealA to r) => CanAddMulScalarA to (AsCauchyReal r) Integer
instance (CanAsCauchyRealA to r) => CanAddMulDivScalarA to (AsCauchyReal r) Integer

instance (CanAsCauchyRealA to r) => CanAddA to Integer (AsCauchyReal r) where
    type AddTypeA to Integer (AsCauchyReal r) = (AsCauchyReal r)
    addA =
        proc (n,r) -> 
            unaryOpWithPureArg "+" (add) (getInitQ1TFromSimple id) -< (r, n)
        
instance (CanAsCauchyRealA to r) => CanSubA to Integer (AsCauchyReal r)


instance (CanAsCauchyRealA to r) => CanAddA to (AsCauchyReal r) Integer where
    type AddTypeA to (AsCauchyReal r) Integer = (AsCauchyReal r)
    addA =
        proc (r,n) -> 
            unaryOpWithPureArg "+" add (getInitQ1TFromSimple id) -< (r, n)


instance (CanAsCauchyRealA to r) => CanAddThisA to (AsCauchyReal r) Integer

instance (CanAsCauchyRealA to r) => CanSubA to (AsCauchyReal r) Integer

instance (CanAsCauchyRealA to r) => CanSubThisA to (AsCauchyReal r) Integer

instance (CanAsCauchyRealA to r) => CanMulA to Integer (AsCauchyReal r) where
    type MulTypeA to Integer (AsCauchyReal r) = (AsCauchyReal r)
    mulA =
        proc (n,r) ->
            unaryOpWithPureArg "*" mul getInitQ1T -< (r,n)
        where
        getInitQ1T =
            proc (q, _a1, n) ->
                do
                let nNormLog = getNormLog n
                let jInit1 = case nNormLog of 
                        NormBits nNL -> max (bits 0) (q + nNL + 1)
                        NormZero -> bits 0
                returnA -< (jInit1, Nothing)

instance (CanAsCauchyRealA to r) => CanMulA to (AsCauchyReal r) Integer where
    type MulTypeA to (AsCauchyReal r) Integer = (AsCauchyReal r)
    mulA = flipA mulA 

instance (CanAsCauchyRealA to r) => CanMulByA to (AsCauchyReal r) Integer


instance (CanAsCauchyRealA to r) => CanDivA to Integer (AsCauchyReal r) where
    type DivTypeA to Integer (AsCauchyReal r) = (AsCauchyReal r)
    divA =
        proc (n,r) ->
            unaryOpWithPureArg "/" div getInitQ1T -< (r,n) 
            -- TODO: wrap div with an operation checking for division by zero to avoid crashes
        where
        getInitQ1T =
            proc (q, a2, n) ->
                do
                let nNormLog = getNormLog n
                (a2NormLog, b2) <- getCRFnNormLog -< (a2,q,id)
                let jInit2 = case (nNormLog, a2NormLog) of
                        (_, NormZero) -> bits 0 -- denominator == 0, we have no chance... 
                        (NormZero, _) -> bits 0 -- numerator == 0, it does not matter 
                        (NormBits nNL, NormBits a2NL) -> max 0 (q + nNL + 1 - 2 * a2NL)
                returnA -< ((jInit2, Just b2))

instance (CanAsCauchyRealA to r) => CanDivA to (AsCauchyReal r) Integer where
    type DivTypeA to (AsCauchyReal r) Integer = (AsCauchyReal r)
    divA =
        proc (r,n) ->
            unaryOpWithPureArg "*" div getInitQ1T -< (r,n)
        where
        getInitQ1T =
            proc (q, _a1, n) ->
                do
                let nNormLog = getNormLog n
                let jInit1 = case nNormLog of 
                        NormBits nNL -> max (bits 0) (q - nNL + 1)
                        NormZero -> bits 0 -- denominator == 0, we have no chance...
                returnA -< (jInit1, Nothing)

instance (CanAsCauchyRealA to r) => CanDivByA to (AsCauchyReal r) Integer

instance (Arrow to ) => CanSqrtA to Integer where
    type SqrtTypeA to Integer = CauchyReal
    sqrtA = proc x ->
        returnA -< seqByPrecision2Cauchy (Just $ "sqrt " ++ show x) $ 
                        \p -> sqrt (integer2BallP p x)      

instance (Arrow to ) => CanExpA to Integer where
    type ExpTypeA to Integer = CauchyReal
    expA = proc x -> 
        returnA -< seqByPrecision2Cauchy (Just $ "exp " ++ show x) $ 
                        \p -> exp (integer2BallP p x)

instance (Arrow to) => CanSineCosineA to Integer where
    type SineCosineTypeA to Integer = CauchyReal
    sinA = proc x -> 
        returnA -< seqByPrecision2Cauchy (Just $ "sin " ++ show x) $ 
                        \p -> sin (integer2BallP p x)
    cosA = proc x -> 
        returnA -< seqByPrecision2Cauchy (Just $ "cos " ++ show x) $ 
                        \p -> cos (integer2BallP p x)

{- CauchyReal-Rational operations -}

instance (CanAsCauchyRealA to r) => CanAddMulScalarA to (AsCauchyReal r) Rational
instance (CanAsCauchyRealA to r) => CanAddMulDivScalarA to (AsCauchyReal r) Rational

instance (CanAsCauchyRealA to r) => CanAddA to Rational (AsCauchyReal r) where
    type AddTypeA to Rational (AsCauchyReal r) = (AsCauchyReal r)
    addA =
        proc (n,r) -> 
            unaryOpWithPureArg "+" (add) (getInitQ1TFromSimple id) -< (r, n)
        
instance (CanAsCauchyRealA to r) => CanSubA to Rational (AsCauchyReal r)


instance (CanAsCauchyRealA to r) => CanAddA to (AsCauchyReal r) Rational where
    type AddTypeA to (AsCauchyReal r) Rational = (AsCauchyReal r)
    addA =
        proc (r,n) -> 
            unaryOpWithPureArg "+" add (getInitQ1TFromSimple id) -< (r, n)


instance (CanAsCauchyRealA to r) => CanAddThisA to (AsCauchyReal r) Rational

instance (CanAsCauchyRealA to r) => CanSubA to (AsCauchyReal r) Rational

instance (CanAsCauchyRealA to r) => CanSubThisA to (AsCauchyReal r) Rational

instance (CanAsCauchyRealA to r) => CanMulA to Rational (AsCauchyReal r) where
    type MulTypeA to Rational (AsCauchyReal r) = (AsCauchyReal r)
    mulA =
        proc (n,r) ->
            unaryOpWithPureArg "*" mul getInitQ1T -< (r,n)
        where
        getInitQ1T =
            proc (q, _a1, n) ->
                do
                let nNormLog = getNormLog n
                let jInit1 = case nNormLog of 
                        NormBits nNL -> max (bits 0) (q + nNL + 1)
                        NormZero -> bits 0
                returnA -< (jInit1, Nothing)

instance (CanAsCauchyRealA to r) => CanMulA to (AsCauchyReal r) Rational where
    type MulTypeA to (AsCauchyReal r) Rational = (AsCauchyReal r)
    mulA = flipA mulA 

instance (CanAsCauchyRealA to r) => CanMulByA to (AsCauchyReal r) Rational


instance (CanAsCauchyRealA to r) => CanDivA to Rational (AsCauchyReal r) where
    type DivTypeA to Rational (AsCauchyReal r) = (AsCauchyReal r)
    divA =
        proc (n,r) ->
            unaryOpWithPureArg "/" div getInitQ1T -< (r,n) 
            -- TODO: wrap div with an operation checking for division by zero to avoid crashes
        where
        getInitQ1T =
            proc (q, a2, n) ->
                do
                let nNormLog = getNormLog n
                (a2NormLog, b2) <- getCRFnNormLog -< (a2,q,id)
                let jInit2 = case (nNormLog, a2NormLog) of
                        (_, NormZero) -> bits 0 -- denominator == 0, we have no chance... 
                        (NormZero, _) -> bits 0 -- numerator == 0, it does not matter 
                        (NormBits nNL, NormBits a2NL) -> max 0 (q + nNL + 1 - 2 * a2NL)
                returnA -< ((jInit2, Just b2))

instance (CanAsCauchyRealA to r) => CanDivA to (AsCauchyReal r) Rational where
    type DivTypeA to (AsCauchyReal r) Rational = (AsCauchyReal r)
    divA =
        proc (r,n) ->
            unaryOpWithPureArg "*" div getInitQ1T -< (r,n)
        where
        getInitQ1T =
            proc (q, _a1, n) ->
                do
                let nNormLog = getNormLog n
                let jInit1 = case nNormLog of 
                        NormBits nNL -> max (bits 0) (q - nNL + 1)
                        NormZero -> bits 0 -- denominator == 0, we have no chance...
                returnA -< (jInit1, Nothing)

instance (CanAsCauchyRealA to r) => CanDivByA to (AsCauchyReal r) Rational

instance (Arrow to ) => CanSqrtA to Rational where
    type SqrtTypeA to Rational = CauchyReal
    sqrtA = proc x ->
        returnA -< seqByPrecision2Cauchy (Just $ "sqrt " ++ show x) $ 
                        \p -> sqrt (rational2BallP p x)      

instance (Arrow to ) => CanExpA to Rational where
    type ExpTypeA to Rational = CauchyReal
    expA = proc x -> 
        returnA -< seqByPrecision2Cauchy (Just $ "exp " ++ show x) $ 
                        \p -> exp (rational2BallP p x)

instance (Arrow to) => CanSineCosineA to Rational where
    type SineCosineTypeA to Rational = CauchyReal
    sinA = proc x -> 
        returnA -< seqByPrecision2Cauchy (Just $ "sin " ++ show x) $ 
                        \p -> sin (rational2BallP p x)
    cosA = proc x -> 
        returnA -< seqByPrecision2Cauchy (Just $ "cos " ++ show x) $ 
                        \p -> cos (rational2BallP p x)

{- operations mixing MPBall and CauchyReal, resulting in an MPBall -}

instance (CanAsCauchyRealA to r) => CanAddMulScalarA to MPBall (AsCauchyReal r)
instance (CanAsCauchyRealA to r) => CanAddMulDivScalarA to MPBall (AsCauchyReal r)

binaryMPRealA :: 
    CanAsCauchyRealA to r =>
    (MPBall -> MPBall -> t) -> (MPBall, AsCauchyReal r) `to` t
binaryMPRealA op =
    proc (a, r) ->
        do
        let ac = getAccuracyIfExactUsePrec a
        b <- getAnswerCRA -< (r,ac+1)
        returnA -< a `op` b

getAccuracyIfExactUsePrec :: MPBall -> Accuracy
getAccuracyIfExactUsePrec ball =
    case getAccuracy ball of
        Exact -> bits (prec2integer $ getPrecision ball) -- should we also consider the norm of the ball? 
        result -> result

instance
    (CanAsCauchyRealA to r) => CanAddA to MPBall (AsCauchyReal r) 
    where
    type AddTypeA to MPBall (AsCauchyReal r) = MPBall
    addA = binaryMPRealA add

instance
    (CanAsCauchyRealA to r) => CanAddA to (AsCauchyReal r) MPBall 
    where
    type AddTypeA to (AsCauchyReal r) MPBall = MPBall
    addA = flipA addA


instance (CanAsCauchyRealA to r) => CanAddThisA to MPBall (AsCauchyReal r)

instance (CanAsCauchyRealA to r) => CanSubA to MPBall (AsCauchyReal r) 
instance (CanAsCauchyRealA to r) => CanSubA to (AsCauchyReal r) MPBall 
instance (CanAsCauchyRealA to r) => CanSubThisA to MPBall (AsCauchyReal r)

instance
    (CanAsCauchyRealA to r) => CanMulA to MPBall (AsCauchyReal r) 
    where
    type MulTypeA to MPBall (AsCauchyReal r) = MPBall
    mulA = binaryMPRealA mul

instance
    (CanAsCauchyRealA to r) => CanMulA to (AsCauchyReal r)  MPBall 
    where
    type MulTypeA to (AsCauchyReal r) MPBall = MPBall
    mulA = flipA mulA


instance (CanAsCauchyRealA to r) => CanMulByA to MPBall (AsCauchyReal r)

instance
    (CanAsCauchyRealA to r) => CanDivA to MPBall (AsCauchyReal r) 
    where
    type DivTypeA to MPBall (AsCauchyReal r) = MPBall
    divA = binaryMPRealA div

instance
    (CanAsCauchyRealA to r) => CanDivA to (AsCauchyReal r)  MPBall 
    where
    type DivTypeA to (AsCauchyReal r) MPBall = MPBall
    divA = proc (r,b) -> mulA -< (r,1/b)

instance (CanAsCauchyRealA to r) => CanDivByA to MPBall (AsCauchyReal r)

{- Interval operations -}

instance (Arrow to, CanAbsSameTypeA to r2, CanReadAsCauchyRealA to r1, CanAsCauchyRealA to r2,
          CanCombineCRsA to r1 r2) => CanPlusMinusA to (AsCauchyReal r1) (AsCauchyReal r2) where
        type PlusMinusTypeA to (AsCauchyReal r1) (AsCauchyReal r2) = Interval (AsCauchyReal (CombinedCRs to r1 r2))
        plusMinusA = proc (x, y) ->
                        do
                        absY <- absA -< y
                        l <- subA -< (x,absY)
                        r <- addA -< (x,absY)
                        returnA -< Interval l r

                        
{- (CanReadAsCauchyRealA to r1, CanReadAsCauchyRealA to r2,
     CanCombineCRsA to r1 r2) 
    => 
    CanAddA to (AsCauchyReal r1) (AsCauchyReal r2) -}                        