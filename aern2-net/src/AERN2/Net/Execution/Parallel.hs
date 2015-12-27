{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts #-}
module AERN2.Net.Execution.Parallel where

import AERN2.Real hiding (id, (.))
import Data.String (IsString(..),fromString)

import AERN2.Net.Spec.Arrow
import Control.Category
import Control.Arrow
import qualified Data.Map as Map

import Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent (forkIO)
import Control.Monad.Reader


_anet0parCauchy :: Integer -> IO MPBall
_anet0parCauchy p =
    executeNetMPrintLog $
        do
        resultCh <- runKleisli (_anet0 :: NetA () CauchyRealChannelPair) ()
        aMap <- getAnswers [(bits p, resultCh)]
        endOfQueries [resultCh]
        let (Just a) = Map.lookup 1 aMap
        return a

_anet3parCauchy :: (Rational, Rational, Rational) -> Integer -> IO MPBall
_anet3parCauchy (x,y,z) p =
    executeNetMPrintLog $
        do
        channels <- mapM mkChannel $ [("x",x), ("y",y), ("z",z)]
        let envCh = Map.fromList channels
        resultCh <- runKleisli _anet3 envCh
        aMap <- getAnswers [(bits p, resultCh)]
        endOfQueries [resultCh]
        let (Just a) = Map.lookup 1 aMap
        return a
        where
        mkChannel (name, value) =
            do
            ch <- constSTM (ChannelName name) (rational value) ()
            return (name, ch)


{- Network evaluation using Cauchy reals with each process running in parallel -}

instance ArrowReal NetA CauchyRealChannelPair where
    realA r name = Kleisli $ constSTM (ChannelName name) r
    sqrtA = Kleisli sqrtSTM
    addA = Kleisli addSTM
    mulA = Kleisli mulSTM
    
type NetA = Kleisli NetM
type NetM = ReaderT (STM.TVar NetInfo) IO

data NetInfo =
    NetInfo
    {
        net_log :: [String]
    }
    deriving (Show)

initNetInfo :: NetInfo
initNetInfo =
    NetInfo
    {
        net_log = []
    }

executeNetMPrintLog :: NetM t -> IO t
executeNetMPrintLog code =
    do
    netInfoTV <- STM.newTVarIO initNetInfo
    _ <- forkIO $ monitorLog netInfoTV
    runReaderT code netInfoTV
    where
    monitorLog netInfoTV =
        do
        netInfo0 <- atomically $ STM.readTVar netInfoTV
        keepPrintingMessages (net_log netInfo0)
        where
        keepPrintingMessages messagesPrev =
            do
            (newMessages, messagesNow) <- atomically waitForChange
            mapM_ putStrLn newMessages
            keepPrintingMessages messagesNow
            where
            waitForChange =
                do
                netInfoNow <- STM.readTVar netInfoTV
                let messagesNow = net_log netInfoNow
                let newMessages = drop (length messagesPrev) messagesNow
                case newMessages of
                    [] -> STM.retry
                    _ -> return (newMessages, messagesNow)
            
disableNetLogs :: Bool
disableNetLogs = False
--disableNetLogs = True
            
netLogMessage :: String -> NetM ()
netLogMessage message 
    | disableNetLogs = return ()
    | otherwise =
        do
        netInfoTV <- ask
        atomicallyNetM $ 
            do
            netInfo <- STM.readTVar netInfoTV
            STM.writeTVar netInfoTV (netInfo { net_log = (net_log netInfo ++ [message]) } )

atomicallyNetM :: STM.STM a -> NetM a 
atomicallyNetM =
    liftIO . atomically

forkNetM :: (NetM ()) -> NetM ()
forkNetM code =
    do
    netInfoTV <- ask
    _ <- liftIO $ forkIO $ runReaderT code netInfoTV
    return ()


type CauchyRealChannelPair = QAChannel Accuracy MPBall 
type QAChannel q a = (QChannel q, AChannel q a)

type AChannel q a = STM.TVar (Map.Map q (Maybe a))
type QChannel q = STM.TChan (Query q)
data Query q = Query q | EndOfQueries
    deriving (Eq, Ord, Show)

addSTM ::
    (CauchyRealChannelPair, CauchyRealChannelPair) -> NetM CauchyRealChannelPair
addSTM = binarySTM "+" (+) return return

mulSTM ::
    (CauchyRealChannelPair, CauchyRealChannelPair) -> NetM CauchyRealChannelPair
mulSTM = binarySTM "*" (*) return return -- TODO

sqrtSTM ::
    (CauchyRealChannelPair) -> NetM CauchyRealChannelPair
sqrtSTM ch = 
    unarySTM "sqrt" sqrt getInitQ ch -- TODO: improve the initial query
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
--getChannelFunctionNormLog ::
--    Accuracy {-^ @q@ -} -> 
--    CauchyRealChannelPair {-^ @ch@ -} -> 
--    (MPBall -> MPBall) {-^ @fn@ -} -> 
--    NetM NormLog {-^ approximate log norm of @fn(value of ch)@ -}
getChannelFunctionNormLog :: 
    (HasNorm a, HasNorm t, HasOrder Integer a,
     OrderCompareType Integer a ~ Maybe Bool) 
    =>
    Accuracy {-^ @q@ -} ->
    QAChannel Accuracy t {-^ @ch@ -} ->
    (t -> a) {-^ @fn@ -} -> 
    NetM NormLog {-^ approximate log norm of @fn(value of ch)@ -}
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
    ChannelName ->
    CauchyReal ->
    () -> NetM CauchyRealChannelPair
constSTM chName r _ = 
    qaChannel chName handleQuery (return ())
    where
    handleQuery aTV q =
        do
--        putStrLn $ "constSTM: starting handleQuery q = " ++ show q
        let a = cauchyReal2ball r q
        atomicallyNetM $
            do
            qaMap <- STM.readTVar aTV
            STM.writeTVar aTV (Map.insert q (Just a) qaMap) 
--        putStrLn $ "constSTM: written a = " ++ show a
--        putStrLn $ "constSTM: ending handleQuery q = " ++ show q

unarySTM ::
    ChannelName
    -> (MPBall -> MPBall)
    -> (Accuracy -> NetM Accuracy)
    -> (CauchyRealChannelPair)
    -> NetM CauchyRealChannelPair
unarySTM chName op getQ1 (ch1) = 
    qaChannel chName handleQuery  (endOfQueries [ch1])
    where
    handleQuery aTV q =
        do
        qi1 <- getQ1 q
        a <- ensureAccuracyM1 q qi1 opWithQ1
        atomicallyNetM $
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
    ChannelName
    -> (MPBall -> MPBall -> MPBall)
    -> (Accuracy -> NetM Accuracy)
    -> (Accuracy -> NetM Accuracy)
    -> (CauchyRealChannelPair, CauchyRealChannelPair)
    -> NetM CauchyRealChannelPair
binarySTM chName op getQ1 getQ2 (ch1, ch2) = 
    qaChannel chName handleQuery (endOfQueries [ch1, ch2])
    where
    handleQuery aTV q =
        do
        qi1 <- getQ1 q
        qi2 <- getQ2 q
        a <- ensureAccuracyM2 q qi1 qi2 opWithQ1Q2
        atomicallyNetM $
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

newtype ChannelName = ChannelName String
    deriving (Show, IsString)

qaChannel :: 
   (Ord q, Show q, Show a) => 
   ChannelName ->
   (AChannel q a -> q -> NetM ()) -> 
   (NetM ()) ->
   NetM (QAChannel q a)
qaChannel chName handleQuery onEndOfQueries =
        do
        netLogMessage $ show chName ++ ": starting" 
        qTC <- atomicallyNetM $ STM.newTChan
        aTV <- atomicallyNetM $ STM.newTVar Map.empty
        forkNetM $ respondToQueries (qTC,aTV)
        return (qTC, aTV)
        where
        respondToQueries (qTC,aTV) =
            do
            qOrEnd <- atomicallyNetM $ STM.readTChan qTC
            case qOrEnd of
                EndOfQueries ->
                    do
                    netLogMessage $ show chName ++ ": terminating" 
                    onEndOfQueries
                Query q ->
                    do
                    netLogMessage $ show chName ++ ": received query " ++ show q 
                    isNewQuery <- atomicallyNetM $ registerQueryIfNew q
                    netLogMessage $ show chName ++ ": query " ++ show q ++ " is new? " ++ show isNewQuery 
                    if isNewQuery
                        then
                            do 
                            forkNetM $
                                do 
                                handleQuery aTV q
                                netLogMessage $ show chName ++ ": handler finished for query " ++ show q
                                (Just (Just a)) <- atomicallyNetM $ lookupQuery q
                                netLogMessage $ show chName ++ ": answer for query " ++ show q ++ ": " ++ show a
                        else
                            do
                            netLogMessage $ show chName ++ ": ignoring query: " ++ show q 
                            return ()
                            {- ignore the query, it is either already answered 
                               or worked on by another handler -}
                    respondToQueries (qTC,aTV)
            where
            registerQueryIfNew q =
                do
                qaMap <- STM.readTVar aTV
                let mmA = Map.lookup q qaMap
                case mmA of
                    Just _ -> return False
                    _ -> do { STM.writeTVar aTV (Map.insert q Nothing qaMap); return True }
            lookupQuery q =
                do
                qaMap <- STM.readTVar aTV
                return $ Map.lookup q qaMap

getAnswers :: (Ord q) => [(q, QAChannel q a)] -> NetM (Map.Map Integer a)
getAnswers queries =
    do
    resultsTV <- atomicallyNetM $ STM.newTVar Map.empty
    mapM_ forkNetM $ map (getAnswer resultsTV) $ zip [1..] queries
    atomicallyNetM $ waitForResults resultsTV
    where
    getAnswer resultsTV (i,(q,(qTC, aTV))) =
        do
        atomicallyNetM $ STM.writeTChan qTC (Query q)
        atomicallyNetM $ waitForResult
        where
        waitForResult =
            do
            aMap <- STM.readTVar aTV
            case Map.lookup q aMap of
                Just (Just a) -> gotResult a
                _ -> STM.retry
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

endOfQueries :: [QAChannel q a] -> NetM ()
endOfQueries channels =
    mapM_ (\(ch, _) -> atomicallyNetM $ STM.writeTChan ch EndOfQueries) channels

