{-# LANGUAGE CPP #-}
{-# LANGUAGE Arrows, EmptyDataDecls, GADTs, StandaloneDeriving, TypeOperators, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module AERN2.Net.Spec.Arrow where

import AERN2.Real hiding (id, (.))
--import Data.String (IsString(..),fromString)

import Control.Category
import Control.Arrow
import qualified Data.Map as Map

import Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent (forkIO)
import Control.Monad (forever)

{- TODO
    Provide instances of "AERN2.Real.Operations" for arbitrary arrows,
    so that networks can be defined by simple arithmetic expressions, such as:
    
    > net :: (HasRealOps to r) => r `to` r
    > net = let x = var "x" in pi * sqrt(x) * x@ 
-}

{- mini examples -}

-- | sqrt(pi) + pi
_anet0 :: (HasRealOps to r) => () `to` r
_anet0 =
    proc _ -> do
        p <- piR -< ()
        sp <- sqrtR -< p
        psp <- addR -< (p,sp)
        returnA -< psp

-- | pi * sqrt(x) * x
_anet1 :: (HasRealOps to r) => r `to` r
_anet1 =
    proc x -> do
        p <- piR -< ()
        sx <- sqrtR -< x
        psx <- mulR -< (p,sx)
        psxx <- mulR -< (psx,x)
        returnA -< psxx

-- | sqrt(x^2+y^2+z^2)    
_anet2 :: (HasRealOps to r) => (r,r,r) `to` r
_anet2 =
    proc (x,y,z) -> do
        x2 <- mulR -< (x,x)
        y2 <- mulR -< (y,y)
        z2 <- mulR -< (z,z)
        x2y2 <- addR -< (x2,y2)
        x2y2z2 <- addR -< (x2y2, z2)
        r <- sqrtR -< x2y2z2
        returnA -< r

{-| An arrow enriched with arithmetic operations. -}
class (Arrow a) => HasRealOps a r where
    piR :: a () r -- TODO: change () to (SizeLimits r)
    sqrtR :: a r r
    mulR :: a (r,r) r
    addR :: a (r,r) r
-- TODO: add more operators

{- Direct evaluation using CauchyReal -}

instance HasRealOps (->) CauchyReal where
    piR = const pi
    sqrtR = sqrt
    addR = uncurry (+)
    mulR = uncurry (*)

{- Direct evaluation using MPBall -}

instance HasRealOps (->) MPBall where
--    piR p = cauchyReal2ball (prec2integer p) pi -- TODO: enable when we have (SizeLimits MPBall)
    sqrtR = sqrt
    addR = uncurry (+)
    mulR = uncurry (*)

{- Evaluation using Cauchy reals with each process running in parallel -}

{- TODO: move this to various more appropriate modules -}

instance HasRealOps KIO CauchyRealChannelPair where
    piR = Kleisli $ constSTM pi
    sqrtR = Kleisli sqrtSTM
    addR = Kleisli addSTM
    -- TODO: complete and test
    
type KIO = Kleisli IO
type CauchyRealChannelPair = QAChannel Accuracy MPBall 
type QAChannel q a = (QChannel q, AChannel q a)
type QChannel q = STM.TChan q
type AChannel q a = STM.TVar (Map.Map q (Maybe a))

addSTM ::
    (CauchyRealChannelPair, CauchyRealChannelPair) -> IO CauchyRealChannelPair
addSTM = binarySTM (+) return return

sqrtSTM ::
    (CauchyRealChannelPair) -> IO CauchyRealChannelPair
sqrtSTM ch = 
    unarySTM sqrt getInitQ ch -- TODO: improve the initial query
    where
    getInitQ q =
        do
        maybeSqrtNormLog <- getChannelFunctionNormLog q ch sqrt
        case maybeSqrtNormLog of
            NormBits sqrtNormLog -> return $ max 0 (q - 1 - sqrtNormLog)
            NormZero -> return q

{-|
    Investigate the approximate magnitude of @(fn x)@ where @x@ is the value of the channel @ch@.
    First try with a very low accuracy and, if the value is close to 0, try with the 
    given accuracy @q@ (assuming it is higher).
-}
getChannelFunctionNormLog ::
    Accuracy {-^ @q@ -} -> 
    CauchyRealChannelPair {-^ @ch@ -} -> 
    (MPBall -> MPBall) {-^ @fn@ -} -> 
    IO NormLog {-^ approximate log norm of @fn(value of ch)@ -}
getChannelFunctionNormLog q ch fn =
    do
    aMap0 <- getAnswers [(bits 0, ch)]
    let (Just x0) = Map.lookup 1 aMap0
    let fnx0 = fn x0
    case 1 < fnx0 of
        Just True -> return $ getNormLog x0
        _ -> 
            do
            aMap <- getAnswers [(q,ch)]
            let (Just x) = Map.lookup 1 aMap
            let fnx = fn x
            return $ getNormLog fnx

constSTM ::
    CauchyReal ->
    () -> IO CauchyRealChannelPair
constSTM r _ = 
    qaChannel handleQuery
    where
    handleQuery aTV q =
        do
        let a = cauchyReal2ball r q
        atomically $
            do
            qaMap <- STM.readTVar aTV
            STM.writeTVar aTV (Map.insert q (Just a) qaMap) 

unarySTM ::
    (MPBall -> MPBall)
    -> (Accuracy -> IO Accuracy)
    -> (CauchyRealChannelPair)
    -> IO CauchyRealChannelPair
unarySTM op getQ1 (ch1) = 
    qaChannel handleQuery
    where
    handleQuery aTV q =
        do
        qi1 <- getQ1 q
        a <- ensureAccuracyM1 q qi1 opWithQ1
        atomically $
            do
            qaMap <- STM.readTVar aTV
            STM.writeTVar aTV (Map.insert q (Just a) qaMap) 
        where
        opWithQ1 q1 =
            do
            aMap <- getAnswers $ [(q1,ch1)]
            let (Just a1) = Map.lookup 1 aMap 
            return $ op a1 

binarySTM :: 
    (MPBall -> MPBall -> MPBall)
    -> (Accuracy -> IO Accuracy)
    -> (Accuracy -> IO Accuracy)
    -> (CauchyRealChannelPair, CauchyRealChannelPair)
    -> IO CauchyRealChannelPair
binarySTM op getQ1 getQ2 (ch1, ch2) = 
    qaChannel handleQuery
    where
    handleQuery aTV q =
        do
        qi1 <- getQ1 q
        qi2 <- getQ2 q
        a <- ensureAccuracyM2 q qi1 qi2 opWithQ1Q2
        atomically $
            do
            qaMap <- STM.readTVar aTV
            STM.writeTVar aTV (Map.insert q (Just a) qaMap) 
        where
        opWithQ1Q2 q1 q2 =
            do
            aMap <- getAnswers $ [(q1,ch1), (q2, ch2)]
            let (Just a1) = Map.lookup 1 aMap 
            let (Just a2) = Map.lookup 2 aMap 
            return $ op a1 a2 

ensureAccuracyM2 ::
    (Monad m) =>
    Accuracy -> Accuracy -> Accuracy -> (Accuracy -> Accuracy -> m MPBall) -> m MPBall
ensureAccuracyM2 i j1 j2 getB =
    do
    result <- getB j1 j2
    if getAccuracy result >= i 
        then return result
        else ensureAccuracyM2 i (j1+1)(j2+1) getB

ensureAccuracyM1 ::
    (Monad m) =>
    Accuracy -> Accuracy -> (Accuracy -> m MPBall) -> m MPBall
ensureAccuracyM1 i j1 getB =
    do
    result <- getB j1
    if getAccuracy result >= i 
        then return result
        else ensureAccuracyM1 i (j1+1) getB


qaChannel :: 
   (Ord q) => (AChannel q a -> q -> IO ()) -> IO (QAChannel q a)
qaChannel handleQuery =
    do
    qTC <- STM.newTChanIO
    aTV <- STM.newTVarIO Map.empty
    _ <- forkIO $ respondToQueries (qTC,aTV)
    return (qTC, aTV)
    where
    respondToQueries (qTC,aTV) =
        forever $
            do
            q <- atomically $ STM.peekTChan qTC
            isNewQuery <- atomically $ registerQueryIfNew q
            if isNewQuery
                then forkIO $ handleQuery aTV q
                else return undefined 
                {- ignore the query, it is either already answered 
                   or worked on by another handler -}
        where
        registerQueryIfNew q =
            do
            qaMap <- STM.readTVar aTV
            let mmA = Map.lookup q qaMap
            case mmA of
                Just _ -> return False
                _ -> do { STM.writeTVar aTV (Map.insert q Nothing qaMap); return True }


getAnswers :: (Ord q) => [(q, QAChannel q a)] -> IO (Map.Map Integer a)
getAnswers queries =
    do
    resultsTV <- STM.newTVarIO Map.empty
    mapM_ forkIO $ map (getAnswer resultsTV) $ zip [1..] queries
    atomically $ waitForResults resultsTV
    where
    getAnswer resultsTV (i,(q,(qTC, aTV))) =
        atomically $ 
        do
        aMap <- STM.readTVar aTV
        case Map.lookup q aMap of
            Just (Just a) -> gotResult a
            _ ->
                do
                STM.writeTChan qTC q
        where
        gotResult a =
            do
            results <- STM.readTVar resultsTV
            STM.writeTVar resultsTV (Map.insert i a results)
    waitForResults resultsTV =
        do
        results <- STM.readTVar resultsTV
        if Map.size results == length queries
            then return results
            else STM.retry 


{-

--newtype CauchyRealWithLog = CauchyRealWithLog (Accuracy -> (MPBall, [LogMessage]))
--
--newtype LogMessage = LogMessage String deriving (Eq, Ord, Show, IsString)

instance HasRealOps QueryTracing (CauchyReal, SocketId)
     
type QueryTracing = Kleisli (State ([SocketId], [LogMessage]))
newtype SocketId = SocketId Integer 
newtype LogMessage = LogMessage String


type CauchyRealA to = [Accuracy] `to` [(Accuracy, MPBall)]

instance (Arrow to) => CanNeg (CauchyRealA to) where
    neg x = 
        proc ac -> 
            do
            xB <- x -< ac
            returnA -< (neg xB)
--        (arr neg) <<< x -- is this equivalent or not?
    

instance (Arrow to) => CanAdd (CauchyRealA to) (CauchyRealA to) where
    add x y =
--        proc ac ->
--            do
            
ensureAccuracy1 ::
    (Arrow to) =>
    Maybe Accuracy -> (Maybe MPBall -> Accuracy -> Accuracy) -> (Accuracy `to` MPBall) -> (CauchyRealA to)
ensureAccuracy1 maybeProbeAc getInitialAc getBallA =
    proc ac ->
        do
        maybeProbeBall <- case maybeProbeAc of
            Just probeAc -> 
                do
                probeBall <- getBallA -< probeAc
                returnA -< probeBall
            Nothing -> returnA -< Nothing
        let initialAc = getInitialAc maybeProbeBall ac
        ball1 <- getBallA -< initialAc

--    | getAccuracy result >= i = 
--        result
--    | otherwise =
--        ensureAccuracy1 i (j+1) getB
--    where
--    result = getB j

            
--ensureAccuracy2 ::
--    Accuracy -> Accuracy -> Accuracy -> (Accuracy -> Accuracy -> MPBall) -> MPBall
--ensureAccuracy2 i j1 j2 getB 
--    | getAccuracy result >= i = 
--        result
--    | otherwise =
--        ensureAccuracy2 i (j1+1)(j2+1) getB
--    where
--    result = getB j1 j2

-}