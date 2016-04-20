{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, TypeOperators, FlexibleInstances #-} 
{-# LANGUAGE ScopedTypeVariables, FunctionalDependencies #-}
module Main where

import Prelude hiding ((.))

import System.Environment (getArgs)
import System.Random (randomRIO)

import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

import Control.Category (Category(..))
import Control.Arrow 
    (Arrow(..), returnA, 
        ArrowChoice(..),ArrowApply(..), 
        ArrowMonad(..), Kleisli(..))
import Control.Monad (forever)

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.STM as STM

import Control.Distributed.Process 
    (Process, liftIO, say,
     spawnLocal, NodeId, 
     send, expect, expectTimeout, 
     receiveWait, match, 
     newChan, sendChan, receiveChan, SendPort, 
     getSelfNode, register, nsend, nsendRemote)
import Control.Distributed.Process.Node 
    (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet 
    (Backend, initializeBackend, newLocalNode, findPeers)

main :: IO ()
main = do
    -- initialise distributed processing backend:
    [host, port] <- getArgs
    backend <- initializeBackend host port initRemoteTable
    node <- newLocalNode backend
    
    runProcess node $ 
        do
        say $ "Initialising network..." ++ show netId
        let peerTimeout = 1000000 -- 1 second
        nodes <- initialiseNodes backend peerTimeout
        say $ "Network initialised, found nodes:\n" ++ (unlines $ map show $ Set.toAscList nodes)
        
        let query = 30
        maybeQA <- runQAProcessArrow nodes CauchyRealP comput4A query
        case maybeQA of
            Nothing -> 
                return ()
            Just (q,a) ->
                do
                say $ "query  = " ++ show q
                say $ "answer = " ++ show a
                liftIO $ threadDelay 1000000 -- wait for 1 second so that the above "say" can complete

comput1A :: 
    (ArrowQA to) => 
    () `to` (CauchyRealA to)
comput1A = piA

comput2A :: 
    (ArrowQA to) => 
    () `to` (CauchyRealA to)
comput2A =
    proc () ->
        do 
        a1 <- piA -< ()
        a2 <- piA -< ()
        b <- addA -< (a1,a2) 
        returnA -< b

comput3A ::
    (ArrowQA to) => 
    () `to` (CauchyRealA to)
comput3A =
    proc () ->
        do 
        a1 <- piA -< ()
        b <- addA -< (a1,a1) 
        returnA -< b

comput4A ::
    (ArrowQA to) => 
    () `to` (CauchyRealA to)
comput4A =
    proc () ->
        do 
        a1 <- piA -< ()
        fn <- functionB2BA (+ (MPBall 1 0)) -< ()
        b <- evalA -< (fn,a1) 
        returnA -< b

    
{-------- QA PROCESS ARROW --------}

type QAProcessArrow = WithNodeInfo (Kleisli Process)

type WithNodeInfo to = StateA NodeInfo to

data NodeInfo =
    NodeInfo
    {
        nodeInfo_myIx :: Integer,
        nodeInfo_nodes :: Set.Set NodeId,
        nodeInfo_netInfoTV :: STM.TVar (QANetInfo (WithNodeInfo (Kleisli Process)))
    }

liftProcessQA :: 
    (a -> Process b) -> (QAProcessArrow a b)
liftProcessQA = liftA . Kleisli

getNodeInfoA :: QAProcessArrow () NodeInfo
getNodeInfoA = getA

data RemoteQuery p = 
    RemoteQuery p (SendPort (A p)) ComputId (Q p)
    deriving (Typeable, Generic)

instance (QAProtocol p) => Binary (RemoteQuery p)

instance (ArrowQA QAProcessArrow) where
    newQAComputation p name =
        StateA $ Kleisli $ \ (q2a, nodeInfo) ->
            do
            computId <- registerComputation (nodeInfo, q2a)
            say $ "newQAComputation: " ++ show name ++ " registered in netInfoTV " ++ show computId
            -- return a "redirect to QANetInfo" computation 
            -- (to support shared evolving computations, eg when caching):
            return (computLookupId computId, nodeInfo)
        where
        computLookupId computId =
            QAComputation p $ StateA $ Kleisli $ \(q, nodeInfo) ->
                do
                netInfo <- liftAtomically $ STM.readTVar $ nodeInfo_netInfoTV nodeInfo
                let (StateA (Kleisli q2aM)) = getAnswerA netInfo p computId
                (a,_) <- q2aM (q,nodeInfo)
                return (a, nodeInfo) 
        registerComputation (nodeInfo, q2a) =
            liftAtomically $
                do
                ni <- STM.readTVar netInfoTV
                let computId = newComputId ni
                let q2aDistributed = useCorrectNode q2a computId
                STM.writeTVar netInfoTV $ qaNetInfoAddComput computId (QAComputation p q2aDistributed) ni
                return computId
            where
            netInfoTV = nodeInfo_netInfoTV nodeInfo
        useCorrectNode q2a computId@(ComputId computId_i) =
            proc query ->
                do
--                () <- (liftProcessQA $ \() -> liftIO $ putStrLn "useCorrectNode: starting") -< ()
                nodeInfo <- getNodeInfoA -< ()
                let responsibleNodeIx = computId_i `mod` (toInteger $ Set.size $ nodeInfo_nodes nodeInfo)
                let responsibleNode = Set.elemAt (fromInteger responsibleNodeIx) $ nodeInfo_nodes nodeInfo
                case () of
                    _ | responsibleNodeIx == nodeInfo_myIx nodeInfo -> 
                     -- this node is responsible for this computation
                        do
                        () <- liftProcessQA say -< queryDescription query ++ " (local on node " ++ show responsibleNodeIx ++ ")"
                        q2a -< query
                    _ -> -- another node is responsible for this computation
                                   -- delegate...
                        do
                        () <- liftProcessQA say -< queryDescription query ++ " (sending to node " ++ show responsibleNodeIx ++ ")"
                        -- create a channel on which we will expect a response:
                        (sendPort, receivePort) <- liftProcessQA (const newChan) -< ()
                        -- forward the query to the "QAQuery" process on the other node:
                        liftProcessQA forwardToNode -< (responsibleNode, RemoteQuery p sendPort computId query)
                        -- wait for a response:
                        answer <- liftProcessQA receiveChan -< receivePort
                        returnA -< answer
            where
            queryDescription query =
                "Computation " ++ show computId_i ++ " got query " ++ show query
            forwardToNode (responsibleNode, msg) = 
                nsendRemote responsibleNode "ERNetQueries" msg 


runQAProcessArrow ::
    (QAProtocol p) =>
    Set.Set NodeId ->
    p ->
    QAProcessArrow () (QAComputation QAProcessArrow p) -> 
    (Q p) -> Process (Maybe (Q p, A p))
runQAProcessArrow nodes _p (StateA (Kleisli compM)) query =
    do
    say "runQAProcessArrow: starting"
    -- create a NodeInfo record, including netInfoTV:
    self <- getSelfNode
    let myIx = toInteger $ Set.findIndex self nodes
    netInfoTV <- liftAtomically $ STM.newTVar initQANetInfo
    let nodeInfo = NodeInfo myIx nodes netInfoTV
    
    -- work out the full computation network:
    ((QAComputation _p q2aA),_) <- compM ((), nodeInfo)
    let (StateA (Kleisli q2aM)) = q2aA
    
    netInfo <- liftAtomically $ STM.readTVar netInfoTV
    let netSize = Map.size $ net_id2comp netInfo
    say $ "netInfoTV has " ++ show netSize ++ " ER processes"

    -- start the "ERNetQueries" process which will deal with incoming queries using netInfoTV: 
    erNetQueriesProcess <- spawnLocal $ forever $ answerQueryWhenItComes netInfoTV nodeInfo
    register "ERNetQueries" erNetQueriesProcess
    liftIO $ threadDelay 10000 -- 10 ms

    -- if we are master, execute the query:
    let isMaster = myIx == 0
    if isMaster 
        then
            do
            say "runQAProcessArrow: this is master, sending the initial query here"
            (answer, _) <- q2aM (query, nodeInfo)
            
            -- send a signal to process "ERNetNodeStop" on each node:
            mapM_ (\n -> nsendRemote n "ERNetNodeStop" ()) $ Set.toList nodes 
            say $ "runQAProcessArrow: signalled all nodes on ERNetNodeStop, done"
            liftIO $ threadDelay 1000000 -- wait for 1 second
            return (Just (query, answer))
        else
            do
            -- listen on "ERNetNodeStop":
            stopTV <- liftAtomically $ STM.newTVar False
            erNetNodeStopProcess <- spawnLocal $ receiveStop stopTV
            register "ERNetNodeStop" erNetNodeStopProcess 
            -- wait for a message on "ERNetNodeStop":
            say $ "runQAProcessArrow: waiting for a ERNetNodeStop signal"
            liftAtomically $ STM.readTVar stopTV >>= (\stop -> if stop then return () else STM.retry)
            say $ "runQAProcessArrow: got ERNetNodeStop signal, done"
            liftIO $ threadDelay 1000000 -- wait for 1 second
            return Nothing
    where
    receiveStop stopTV =
        do
        () <- expect
        liftAtomically $ STM.writeTVar stopTV True
    answerQueryWhenItComes netInfoTV nodeInfo =
        do
        say "ERNetQueries: waiting for a query"
        receiveWait 
            {- we need to list all protocols below 
               since we cannot serialise values of an existentially quantified type -}
            [match $ answerQuery CauchyRealP, 
             match $ answerQuery FunctionB2BP]         
        where
        answerQuery :: (QAProtocol p) => p -> RemoteQuery p -> Process ()
        answerQuery _p (RemoteQuery p' sendPort computId query') =
            do
            say $ "ERNetQueries: received query " ++ show query' ++ " for " ++ show computId
            anyProtocolComput <- liftAtomically $ waitForComputId computId
            case anyProtocolComput of
                (AnyProtocolQAComputation (QAComputation p'' q2aA)) | sameProtocol p' p'' ->
                    do
                    let (StateA (Kleisli q2aM)) = q2aA
                    -- spawn the computation and answering in a separate process: 
                    let computeAndRespond =
                            do
                            (answer,_) <- q2aM (unsafeCoerce query', nodeInfo)
                            say $ "ERNetQueries: answering " ++ show answer
                            sendChan sendPort (unsafeCoerce answer)
                    _ <- spawnLocal computeAndRespond
                    return ()
                _ ->
                    do
                    say "answerQueryWhenItComes: protocol mismatch"
                    expect
        waitForComputId computId =
            do
            ni <- STM.readTVar netInfoTV
            case Map.lookup computId (net_id2comp ni) of
                Nothing -> STM.retry
                Just comput -> return comput



{-------- WithNetInfo Arrow transformer --------}

data QANetInfo to =
    QANetInfo
    {
        net_id2comp :: Map.Map ComputId (AnyProtocolQAComputation to)
--        ,net_log :: QANetLog -- MOCKUP
    }

initQANetInfo :: QANetInfo to
initQANetInfo = QANetInfo Map.empty

qaNetInfoAddComput ::
    QAProtocol p =>
    ComputId -> 
    QAComputation to p -> 
    QANetInfo to -> QANetInfo to
qaNetInfoAddComput computId comp ni =
    ni { net_id2comp = Map.insert computId (AnyProtocolQAComputation comp) (net_id2comp ni) }

newtype ComputId = ComputId Integer
    deriving (Show, Eq, Ord, Enum, Typeable, Generic)

instance Binary ComputId

newComputId :: QANetInfo to -> ComputId
newComputId ni 
        | Map.null id2comput = ComputId 0
        | otherwise = succ (fst $ Map.findMax id2comput)
        where
        id2comput = net_id2comp ni

getAnswerA ::
    (QAProtocol p, ArrowApply to, ArrowChoice to) =>
    QANetInfo to -> p -> ComputId -> to (Q p) (A p)
getAnswerA netInfo p computId =
    proc q ->
        case monadicPrg q of
            (ArrowMonad arrowPrg) ->
                app -< (arrowPrg, ())
    where
    monadicPrg q =
        do
        case netInfoLookupId netInfo computId of
            (AnyProtocolQAComputation (QAComputation (p'::p') q2a)) | sameProtocol p p' ->
                do
                a <- ArrowMonad $ proc () -> q2a -< ((unsafeCoerce q) :: Q p')
                return $ unsafeCoerce (a :: A p')
            (AnyProtocolQAComputation (QAComputation (p'::p') _)) -> 
                error $ "getAnswer protocol mismatch: " ++ show p ++ " /= " ++ show p'

{- The following version does not type check with ghc 7.8.4, probably due to a compiler bug: 
    proc q ->
        do
        netInfo <- getA -< ()
        case netInfoLookupId netInfo computId of
            (AnyProtocolQAComputation (QAComputation (p'::p') (q2a))) | sameProtocol p p' ->
                do
                a <- app -< (q2a, (unsafeCoerce q) :: Q p')
                returnA -< unsafeCoerce (a :: A p')
-}

netInfoLookupId :: QANetInfo to -> ComputId -> (AnyProtocolQAComputation (to))
netInfoLookupId (QANetInfo id2comput) computId =
    case Map.lookup computId id2comput of
        Just comp -> comp

        Nothing -> error $ "netInfoLookupId: unknown " ++ show computId

{-------- FUNCTION COMPUTATION --------}
    
type FunctionB2BA to = QAComputation to FunctionB2BP
type FunctionB2B = FunctionB2BA (->)

class CanEvalA to f r
    where
    type EvalTypeA to f r
    evalA :: (f,r) `to` (EvalTypeA to f r) 

instance
    (ArrowQA to)
    =>
    CanEvalA to (FunctionB2BA to) (CauchyRealA to)
    where
    type EvalTypeA to (FunctionB2BA to) (CauchyRealA to) = CauchyRealA to
    evalA =
        proc (QAComputation _ q2aF, QAComputation _ q2aX) ->
            newQAComputation CauchyRealP "eval" -< q2aF . q2aX

functionB2BA :: (ArrowQA to) => (MPBall -> MPBall) -> () `to` (FunctionB2BA to)
functionB2BA fn =
    proc () ->
        newQAComputation FunctionB2BP "fn" -< q2a
    where
    q2a =
        proc q -> returnA -< fn q

{-------- CAUCHY REAL COMPUTATION --------}

type CauchyRealA to = QAComputation to CauchyRealP
type CauchyReal = CauchyRealA (->)

piA :: (ArrowQA to) => () `to` CauchyRealA to
piA =
    proc () ->
        newQAComputation CauchyRealP "pi" -< answerQuery
    where
    answerQuery =
        proc accuracy ->
            do
            returnA -< MPBall pi (0.35^accuracy)

class CanAddA to t1 t2
    where
    type AddTypeA to t1 t2
    addA :: (t1,t2) `to` (AddTypeA to t1 t2)

instance 
    (ArrowQA to) => 
    CanAddA to (CauchyRealA to) (CauchyRealA to)
    where 
    type AddTypeA to (CauchyRealA to) (CauchyRealA to) =
        CauchyRealA to
    addA =
        proc (QAComputation _ q2a1, QAComputation _ q2a2) ->
            newQAComputation CauchyRealP "+" -< answerQuery q2a1 q2a2
        where
        answerQuery q2a1 q2a2 =
            proc accuracy ->
                do
                a1 <- q2a1 -< accuracy+1
                a2 <- q2a2 -< accuracy+1 -- MOCKUP; can be more efficient
                returnA -< a1 + a2

{-------- FUNCTION BALL->BALL PROTOCOL --------}

data FunctionB2BP = FunctionB2BP
    deriving (Eq, Show, Typeable, Generic)

instance Binary FunctionB2BP

instance QAProtocol FunctionB2BP
    where
    type Q FunctionB2BP = MPBall
    type A FunctionB2BP = MPBall



{-------- CAUCHY REAL PROTOCOL --------}

data CauchyRealP = CauchyRealP
    deriving (Eq, Show, Typeable, Generic)

instance Binary CauchyRealP

instance QAProtocol CauchyRealP
    where
    type Q CauchyRealP = Accuracy
    type A CauchyRealP = MPBall

type Accuracy = Integer

data MPBall = MPBall { centre :: MPNum, radius :: Double }
    deriving (Show, Typeable, Generic)
    
instance Binary MPBall

type MPNum = Double -- MOCKUP

instance Num MPBall where
    (MPBall c1 r1) + (MPBall c2 r2) = MPBall (c1+c2) (r1+r2)

{-------- GENERAL QA PROTOCOLS --------}

class (Arrow to) => ArrowQA to where
    newQAComputation :: (QAProtocol p) => p -> String -> ((Q p) `to` (A p)) `to` (QAComputation to p) 

class 
    (Eq p,
     Show p, Show (Q p), Show (A p), 
     Typeable p, Typeable (Q p), Typeable (A p), 
     Binary p, Binary (Q p), Binary (A p)
    ) => 
    QAProtocol p 
    where
    type Q p
    type A p

sameProtocol :: (QAProtocol p1, QAProtocol p2) => p1 -> p2 -> Bool
sameProtocol p1 p2 =
    show p1 == show p2

data QAComputation to p = 
    QAComputation
        p 
        (Q p `to` A p) 

data AnyProtocolQAComputation to =
    forall p . (QAProtocol p) => -- existentially quantified type
        AnyProtocolQAComputation (QAComputation to p)

{-------- AN ARROW TRANSFORMER THAT ADDS STATE --------}

newtype StateA s to a b =
    StateA { runStateA :: (a,s) `to` (b,s) }

class (Arrow to) => ArrowState s to | to -> s where
    getA :: () `to` s
    putA :: s `to` ()

instance (Arrow to) => ArrowState s (StateA s to) 
    where
    getA = StateA $ proc ((),s) -> returnA -< (s,s)
    putA = StateA $ proc (s,_s) -> returnA -< ((),s)  

class ArrowTrans t where
    liftA :: Arrow to => (to a b) -> (t to a b)

instance ArrowTrans (StateA s) where
    liftA compA =
        StateA $
            proc (a,s) ->
                do
                b <- compA -< a
                returnA -< (b,s)

instance (Arrow to) => Category (StateA s to)
    where
    id = StateA $ proc (a,s) -> returnA -< (a,s)
    (StateA f) . (StateA g) = StateA $ f . g

instance (Arrow to) => Arrow (StateA s to)
    where
    arr f = StateA $ proc (a,s) -> returnA -< (f a, s)
    first (StateA f) = 
        StateA $
            proc ((a,b),s) ->
                do
                (a',s') <- f -< (a,s)
                returnA -< ((a',b),s')

instance (ArrowApply to) => ArrowApply (StateA s to)
    where
    app =
        StateA $
            proc ((StateA f,a),s) ->
                do
                (fa,s') <- app -< (f,(a,s))
                returnA -< (fa,s')

instance (ArrowChoice to) => ArrowChoice (StateA s to)
    where
    left (StateA f) =
        StateA $
            proc (lORr, s) ->
                case lORr of
                    Left l -> 
                        do
                        (fl,s') <- f -< (l,s)
                        returnA -< (Left fl, s')
                    Right r ->
                        returnA -< (Right r, s)

{-------- NODE-LEVEL INITIALISATION --------}

initialiseNodes :: Backend -> Int -> Process (Set.Set NodeId)
initialiseNodes backend peerTimeout =
    do
    self <- getSelfNode
    -- create shared variable to signal when initialisation is finished: 
    nodesTV <- liftAtomically $ STM.newTVar Nothing

    sayL "Registering the ERNetNodeInit process..."
    erNetNodeInitProcess <- spawnLocal $ listenERNetNodeInit self nodesTV
    register "ERNetNodeInit" erNetNodeInitProcess
    
    sayL "Searching for peers..."
    peers <- liftIO delayAndFindPeers
    -- announce peers to myself:
    mapM_ (send erNetNodeInitProcess . NewNode netId) peers
    
    sayL $ "Sending NewNode message to peers " ++ show peers ++ "..."
    mapM_ (sendNewNode self) peers
    
    nodes <- waitUntilInitialised nodesTV
    return nodes
    where
--    sayL _ = return ()
    sayL = say 
    delayAndFindPeers = 
        do
        -- wait a random bit to avoid many nodes broadcasting at the same time:
        randomDelay <- randomRIO (1,10000) -- up to 0.1 seconds
        threadDelay randomDelay
        -- search for peers using broadcast:
        findPeers backend peerTimeout -- 0.1 seconds
    
    sendNewNode self peer =
        nsendRemote peer "ERNetNodeInit" (NewNode netId self)
    
    waitUntilInitialised nodesTV =
        do
        liftAtomically $ do
            maybeNodes <- STM.readTVar nodesTV
            case maybeNodes of
                Just nodes -> return nodes
                _ -> STM.retry
    
    listenERNetNodeInit self nodesTV =
        do
        announcedNodes <- waitForNewNodesUntilSilence []
        sayL $ "announcedNodes = " ++ show announcedNodes
        initMergeNodes announcedNodes
        syncNodes self nodesTV announcedNodes
    waitForNewNodesUntilSilence nodesSoFar =
        do
        maybeMsg <- expectTimeout 2000000 -- 2s
        case maybeMsg of
            Just (NewNode netId' nodeId') | netId' == netId ->
                waitForNewNodesUntilSilence (nodeId' : nodesSoFar)
            Just _ ->
                waitForNewNodesUntilSilence nodesSoFar
            Nothing ->
                return $ Set.fromList nodesSoFar
    initMergeNodes announcedNodes =
        do
        self <- getSelfNode
        if Set.findMin announcedNodes == self
            then 
                do
                sayL "initMergeNodes: first node"
                nsend "ERNetNodeInit" (NewNodes announcedNodes)
            else return ()
        
    syncNodes self nodesTV announcedNodes =
        forever $
            receiveWait [match newNodes, match allNodes]
        where
        newNodes (NewNodes nodes')
            | self == lastNode =
                do
                sayL "mergeNodes: AllNodes to first"
                nsendRemote firstNode "ERNetNodeInit" (AllNodes nodes)
            | otherwise =
                do
                sayL "mergeNodes: forwarding to next"
                nsendRemote nextNode "ERNetNodeInit" (NewNodes nodes)
            where
            nodes = announcedNodes `Set.union` nodes'
            firstNode = Set.findMin nodes
            lastNode = Set.findMax nodes
            nextNode = Set.elemAt (Set.findIndex self nodes + 1) nodes
        allNodes (AllNodes nodes) 
            | self /= lastNode = 
                do
                sayL "getAllNodes: forwarding to next"
                nsendRemote nextNode "ERNetNodeInit" (AllNodes nodes)
                signalInitDone
            | otherwise =
                signalInitDone
            where
            lastNode = Set.findMax nodes
            nextNode = Set.elemAt (Set.findIndex self nodes + 1) nodes
            signalInitDone =
                liftAtomically $ STM.writeTVar nodesTV (Just nodes)
            
data NewNode = NewNode NetId NodeId
    deriving (Typeable, Generic)
instance Binary NewNode

data NewNodes = NewNodes (Set.Set NodeId)
    deriving (Typeable, Generic)
instance Binary NewNodes

data AllNodes = AllNodes (Set.Set NodeId)
    deriving (Typeable, Generic)
instance Binary AllNodes

type NetId = String

netId :: NetId
netId = "testnet1"

liftAtomically :: (STM.STM a) -> Process a
liftAtomically = liftIO . STM.atomically
