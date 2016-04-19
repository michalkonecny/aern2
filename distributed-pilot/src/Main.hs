{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric, ExistentialQuantification, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, TypeOperators, FlexibleInstances, GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE ScopedTypeVariables, FunctionalDependencies, UndecidableInstances #-}
module Main where

import Prelude hiding ((.))

import System.Environment (getArgs)
import System.Random (randomRIO)

--import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Binary (Binary)
import Data.Typeable
import GHC.Generics
import Unsafe.Coerce

import Control.Category
import Control.Arrow
import Control.Monad.State -- mtl

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.STM as STM

--import Network.Transport.TCP (createTransport, defaultTCPParameters)

--import Control.Distributed.Process.Closure

import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet

main :: IO ()
main = do
    -- initialise distributed processing backend:
    [host, port] <- getArgs
    backend <- initializeBackend host port initRemoteTable
    node <- newLocalNode backend
    
    runProcess node $ 
        do
        say $ "Initialising network " ++ show netId
        nodes <- initialiseNodes backend
        say $ "Network initialised, found nodes:\n" ++ (unlines $ map show $ Set.toAscList nodes)
        
        let query = 30  
        answer <- runQAProcessArrow nodes CauchyRealP comput1A query -- will wait forever if not master
        say $ "query  = " ++ show query
        say $ "answer = " ++ show answer
        expect

comput1A :: 
    (ArrowQA to) => 
    () `to` (CauchyRealA to)
comput1A = piA

    
{-------- QA PROCESS ARROW --------}

type QAProcessArrow = WithNetInfo (WithNodeInfo (Kleisli Process))

type WithNodeInfo to = StateA NodeInfo to

data NodeInfo =
    NodeInfo
    {
        nodeInfo_initialising :: Bool,
        nodeInfo_myIx :: Integer,
        nodeInfo_nodes :: Set.Set NodeId,
        nodeInfo_netInfoTV :: STM.TVar (QANetInfo (WithNodeInfo (Kleisli Process)))
    }

liftProcessQA :: 
    (a -> Process b) -> (QAProcessArrow a b)
liftProcessQA = liftA . liftA . Kleisli

getNodeInfoA :: QAProcessArrow () NodeInfo
getNodeInfoA = liftA getA

data RemoteQuery p = 
    RemoteQuery p (SendPort (A p)) ComputId (Q p)
    deriving (Typeable, Generic)

instance (QAProtocol p) => Binary (RemoteQuery p)

instance (ArrowQA QAProcessArrow) where
    newQAComputation p =
        proc q2a ->
            do
            -- assign the computation an id: 
            computId <- newComputIdA -< ()
            () <- liftProcessQA say -< "newQAComputation: " ++ show computId
            -- construct the computation closure:
            nodeInfo <- getNodeInfoA -< ()
            let comp = QAComputation p $ useCorrectNode q2a computId nodeInfo
            -- store the computation in both QANetInfo objects:
            () <- registerComputationA p -< (computId, comp)
            () <- liftProcessQA say -< "newQAComputation: registered in netInfo " ++ show computId
            () <- liftProcessQA (registerComputationForIncoming) -< (nodeInfo, computId, comp)
            () <- liftProcessQA say -< "newQAComputation: registered in netInfoTV " ++ show computId
            -- return a "redirect to QANetInfo" computation 
            -- (to support shared evolving computations, eg when caching):
            returnA -< computLookupId p computId 
        where
        useCorrectNode q2a computId@(ComputId computId_i) nodeInfo 
             | responsibleNodeIx == nodeInfo_myIx nodeInfo = 
                 -- this node is responsible for this computation
                 proc query ->
                    do
                    () <- liftProcessQA say -< queryDescription query ++ " (local on node " ++ show responsibleNodeIx ++ ")"
                    q2a -< query
             | otherwise = -- another node is responsible for this computation
                           -- delegate...
                 proc query ->
                    do
                    () <- liftProcessQA say -< queryDescription query ++ " (sending to node " ++ show responsibleNodeIx ++ ")"
                    -- create a channel on which we will expect a response:
                    (sendPort, receivePort) <- liftProcessQA (const newChan) -< ()
                    -- forward the query to the "QAQuery" process on the other node:
                    liftProcessQA (nsendRemote responsibleNode "ERNetQueries") -< 
                        RemoteQuery p sendPort computId query
                    -- wait for a response:
                    answer <- liftProcessQA receiveChan -< receivePort
                    returnA -< answer
            where
            queryDescription query =
                "Computation " ++ show computId_i ++ " got query " ++ show query 
            responsibleNodeIx = computId_i `mod` (toInteger $ Set.size $ nodeInfo_nodes nodeInfo)
            responsibleNode = Set.elemAt (fromInteger responsibleNodeIx) $ nodeInfo_nodes nodeInfo
        registerComputationForIncoming (nodeInfo, computId, comp) =
            liftIO $ STM.atomically $
                do 
                ni <- STM.readTVar netInfoTV
                STM.writeTVar netInfoTV $ qaNetInfoAddComput computId comp ni
            where
            netInfoTV = nodeInfo_netInfoTV nodeInfo

runQAProcessArrow ::
    (QAProtocol p) =>
    Set.Set NodeId ->
    p ->
    QAProcessArrow () (QAComputation QAProcessArrow p) -> 
    (Q p) -> Process (A p)
runQAProcessArrow nodes p (StateA (StateA (Kleisli compM))) query =
    do
    say "runQAProcessArrow: starting"
    -- create a NodeInfo record, including netInfoTV:
    self <- getSelfNode
    let myIx = toInteger $ Set.findIndex self nodes
    netInfoTV <- liftIO $ STM.atomically $ STM.newTVar initQANetInfo
    let nodeInfo = NodeInfo False myIx nodes netInfoTV
    
    -- construct a computation and, if we are master, execute the query:
    let isMaster = myIx == 0

    -- work out the full computation network:
    (((QAComputation _p q2aA),netInfo),_) <- compM (((),initQANetInfo), nodeInfo)
    let (StateA (StateA (Kleisli q2aM))) = q2aA
    let netSize = Map.size $ net_id2comp netInfo
    say $ "netInfo has " ++ show netSize ++ " ER processes"
    netInfoTVnow <- liftIO $ STM.atomically $ STM.readTVar netInfoTV
    let netSizeTV = Map.size $ net_id2comp netInfoTVnow
    say $ "netInfoTV has " ++ show netSizeTV ++ " ER processes"

    -- start the "ERNetQueries" process which will deal with incoming queries using netInfoTV: 
    erNetQueriesProcess <- spawnLocal $ forever $ answerQueryWhenItComes netInfoTV netInfo nodeInfo
    register "ERNetQueries" erNetQueriesProcess
    

    -- TODO: test is the above is necessary to get "ERNetQueries" to be able to respond
    if True -- isMaster -- TODO 
        then
            do
            say "runQAProcessArrow: this is master, sending the initial query here"
            ((answer, _), _) <- q2aM ((query, netInfo), nodeInfo)
            return answer
        else
            do
            () <- expect -- listen on "ERNetQueries" forever
            say "error: a non-master received a () query"
            undefined
    where
    answerQueryWhenItComes netInfoTV netInfo nodeInfo =
        do
        say "ERNetQueries: waiting for a query"
        (RemoteQuery p' sendPort computId query') <- expect
        say $ "ERNetQueries: received query " ++ show query' ++ " for " ++ show computId
        let _ = [p,p'] -- TODO: support multiple protocols using receiveWait
        anyProtocolComput <- liftIO $ waitForComputId computId
        case anyProtocolComput of
            (AnyProtocolQAComputation (QAComputation p'' q2aA)) | sameProtocol p' p'' ->
                do
                -- TODO: spawn the answering in a separate process: 
                let (StateA (StateA (Kleisli q2aM))) = q2aA
                ((answer,_),_) <- q2aM (((unsafeCoerce query'), netInfo), nodeInfo)
                say $ "ERNetQueries: answering " ++ show answer
                sendChan sendPort (unsafeCoerce answer)
            _ ->
                do
                say "answerQueryWhenItComes: protocol mismatch"
                expect
        where
        waitForComputId computId =
            STM.atomically $
                do
                ni <- STM.readTVar netInfoTV
                case Map.lookup computId (net_id2comp ni) of
                    Nothing -> STM.retry
                    Just comput -> return comput


    
{-------- WithNetInfo Arrow transformer --------}

type WithNetInfo to = StateA (QANetInfo to) to 

data QANetInfo to =
    QANetInfo
    {
        net_id2comp :: Map.Map ComputId (AnyProtocolQAComputation (WithNetInfo to))
--        ,net_log :: QANetLog -- MOCKUP
    }

initQANetInfo :: QANetInfo to
initQANetInfo = QANetInfo Map.empty

qaNetInfoAddComput ::
    QAProtocol p =>
    ComputId -> 
    QAComputation (WithNetInfo to) p -> 
    QANetInfo to -> QANetInfo to
qaNetInfoAddComput computId comp ni =
    ni { net_id2comp = Map.insert computId (AnyProtocolQAComputation comp) (net_id2comp ni) }

newtype ComputId = ComputId Integer
    deriving (Show, Eq, Ord, Enum, Typeable, Generic)

instance Binary ComputId

newComputIdA :: (Arrow to) => WithNetInfo to () ComputId
newComputIdA =
    proc () ->
        do
        ni <- getA -< ()
        let computId = aux ni
        returnA -< computId
    where
    aux ni 
        | Map.null id2comput = ComputId 0
        | otherwise = succ (fst $ Map.findMax id2comput)
        where
        id2comput = net_id2comp ni

registerComputationA ::
    (QAProtocol p, Arrow to) => 
    p -> 
    WithNetInfo to (ComputId, QAComputation (WithNetInfo to) p) ()
registerComputationA _ =
    proc (computId, comp) ->
        do
        ni <- getA -< ()
        putA -< qaNetInfoAddComput computId comp ni

computLookupId ::
    (QAProtocol p, ArrowApply to, ArrowChoice to) =>
    p -> ComputId -> QAComputation (WithNetInfo to) p
computLookupId p computId =
    QAComputation p (getAnswerA p computId)

getAnswerA ::
    (QAProtocol p, ArrowApply to, ArrowChoice to) =>
    p -> ComputId -> (WithNetInfo to) (Q p) (A p)
getAnswerA p computId =
    proc q ->
        case monadicPrg q of
            (ArrowMonad arrowPrg) ->
                app -< (arrowPrg, ())
    where
    monadicPrg q =
        do
        netInfo <- ArrowMonad getA
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

netInfoLookupId :: QANetInfo to -> ComputId -> (AnyProtocolQAComputation (WithNetInfo to))
netInfoLookupId (QANetInfo id2comput) computId =
    case Map.lookup computId id2comput of
        Just comp -> comp

        Nothing -> error $ "netInfoLookupId: unknown " ++ show computId

{-------- CAUCHY REAL COMPUTATION --------}
    
type CauchyRealA to = QAComputation to CauchyRealP
type CauchyReal = CauchyRealA (->)
type CauchyRealWithNI to = CauchyRealA (WithNetInfo to)

piA :: (ArrowQA to) => () `to` CauchyRealA to
piA =
    proc () ->
        newQAComputation CauchyRealP -< answerQuery
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
            newQAComputation CauchyRealP -< answerQuery q2a1 q2a2
        where
        answerQuery q2a1 q2a2 =
            proc accuracy ->
                do
                a1 <- q2a1 -< accuracy+1
                a2 <- q2a2 -< accuracy+1 -- MOCKUP; can be more efficient
                returnA -< a1 + a2

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
    newQAComputation :: (QAProtocol p) => p -> ((Q p) `to` (A p)) `to` (QAComputation to p) 

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

initialiseNodes :: Backend -> Process (Set.Set NodeId)
initialiseNodes backend =
    do
    self <- getSelfNode
    -- create shared variable to signal when initialisation is finished: 
    nodesTV <- liftIO $ STM.atomically $ STM.newTVar Nothing

    say "Registering the ERNetNodeInit process..."
    erNetNodeInitProcess <- spawnLocal $ listenERNetNodeInit self nodesTV
    register "ERNetNodeInit" erNetNodeInitProcess
    
    say "Searching for peers..."
    peers <- liftIO delayAndFindPeers
    -- announce peers to myself:
    mapM_ (send erNetNodeInitProcess . NewNode netId) peers
    
    say $ "Sending NewNode message to peers " ++ show peers ++ "..."
    mapM_ (sendNewNode self) peers
    
    nodes <- waitUntilInitialised nodesTV
    return nodes
    where
    delayAndFindPeers = 
        do
        -- wait a random bit to avoid many nodes broadcasting at the same time:
        randomDelay <- randomRIO (1,100000)
        threadDelay randomDelay
        -- search for peers using broadcast:
        findPeers backend 1000000
    
    sendNewNode self peer =
        nsendRemote peer "ERNetNodeInit" (NewNode netId self)
    
    waitUntilInitialised nodesTV =
        do
        liftIO $ STM.atomically $ do
            maybeNodes <- STM.readTVar nodesTV
            case maybeNodes of
                Just nodes -> return nodes
                _ -> STM.retry
    
    listenERNetNodeInit self nodesTV =
        do
        announcedNodes <- waitForNewNodesUntilSilence []
        say $ "announcedNodes = " ++ show announcedNodes
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
                say "initMergeNodes: first node"
                nsend "ERNetNodeInit" (NewNodes announcedNodes)
            else return ()
        
    syncNodes self nodesTV announcedNodes =
        forever $
            receiveWait [match newNodes, match allNodes]
        where
        newNodes (NewNodes nodes')
            | self == lastNode =
                do
                say "mergeNodes: AllNodes to first"
                nsendRemote firstNode "ERNetNodeInit" (AllNodes nodes)
            | otherwise =
                do
                say "mergeNodes: forwarding to next"
                nsendRemote nextNode "ERNetNodeInit" (NewNodes nodes)
            where
            nodes = announcedNodes `Set.union` nodes'
            firstNode = Set.findMin nodes
            lastNode = Set.findMax nodes
            nextNode = Set.elemAt (Set.findIndex self nodes + 1) nodes
        allNodes (AllNodes nodes) 
            | self /= lastNode = 
                do
                say "getAllNodes: forwarding to next"
                nsendRemote nextNode "ERNetNodeInit" (AllNodes nodes)
                signalInitDone
            | otherwise =
                signalInitDone
            where
            lastNode = Set.findMax nodes
            nextNode = Set.elemAt (Set.findIndex self nodes + 1) nodes
            signalInitDone =
                liftIO $ STM.atomically $ do
                    STM.writeTVar nodesTV (Just nodes)
            
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

{- OLD EXPERIMENTS BASED ON CLOUD HASKELL TUTORIALS

data T = T Int String deriving (Generic, Typeable)

instance Binary T

sampleTask :: (Int, String) -> Process ()
sampleTask (t, s) = liftIO (threadDelay (t * 1000000)) >> say s

remotable ['sampleTask]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

replyBack :: (ProcessId, T) -> Process ()
replyBack (sender, t) = send sender t

logMessage :: T -> Process ()
logMessage (T n msg) = say $ "handling " ++ msg ++ " (" ++ show n ++ ")"

main1 :: IO ()
main1 =
    do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t myRemoteTable
    runProcess node $ do
        -- Spawn another worker on the local node
        echoPid <- spawnLocal $ forever $ do
          -- Test our matches in order against each message in the queue
          receiveWait [match logMessage, match replyBack]
    
        -- The `say` function sends a message to a process registered as "logger".
        -- By default, this process simply loops through its mailbox and sends
        -- any received log message strings it finds to stderr.
    
        say "send some messages!"
        send echoPid (T 1 "hello")
        self <- getSelfPid
        send echoPid (self, (T 2 "hello"))
    
        -- `expectTimeout` waits for a message or times out after "delay"
        m <- expectTimeout 1000000 -- 1 second
        case m of
          -- Die immediately - throws a ProcessExitException with the given reason.
          Nothing  -> die "nothing came back!"
          Just (T n s) -> say $ "got " ++ s ++ " back!" ++ " (" ++ (show n) ++ ")"
-}