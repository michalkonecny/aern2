{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric, ExistentialQuantification, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, TypeOperators, FlexibleInstances, GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE ScopedTypeVariables, FunctionalDependencies #-}
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
import Control.Monad (forever)
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
        
        channels <- initialiseChannels nodes
        
        expect -- TODO


initialiseChannels :: (Set.Set NodeId) -> Process [AnyQAProtocolSendPort]
initialiseChannels nodes =
    do
    
    return [] -- TODO
    where
    
{-------- QA PROCESS ARROW --------}

type QAProcessArrow = WithNetInfo (Kleisli Process)

data AnyQAProtocolSendPort =
    forall p . (QAProtocol p) => -- existentially quantified type
        AnyQASendPort (SendPort (p, Q p))

type AnyProtocolProcessQAComputation = AnyProtocolQAComputation QAProcessArrow

instance (ArrowQA QAProcessArrow) where
    -- TODO

{-------- WithNetInfo Arrow transformer --------}

type WithNetInfo to = StateA (QANetInfo to) to 

data QANetInfo to =
    QANetInfo
    {
        net_id2comp :: Map.Map ValueId (AnyProtocolQAComputation (WithNetInfo to))
--        ,net_log :: QANetLog
    }

newtype ValueId = ValueId Integer
    deriving (Show, Eq, Ord, Enum)

netInfoLookupId :: QANetInfo to -> ValueId -> (AnyProtocolQAComputation (WithNetInfo to))
netInfoLookupId (QANetInfo id2comp) vId =
    case Map.lookup vId id2comp of
        Just comp -> comp
        Nothing -> error "netInfoLookupId: unknown vId"

getAnswer ::
    (QAProtocol p, ArrowApply to, ArrowChoice to) =>
    p -> ValueId -> (WithNetInfo to) (Q p) (A p)
getAnswer p vId =
    proc q ->
        case monadicPrg q of
            (ArrowMonad arrowPrg) ->
                app -< (arrowPrg, ())
    where
    monadicPrg q =
        do
        netInfo <- ArrowMonad getA
        case netInfoLookupId netInfo vId of
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
        case netInfoLookupId netInfo vId of
            (AnyProtocolQAComputation (QAComputation (p'::p') (q2a))) | sameProtocol p p' ->
                do
                a <- app -< (q2a, (unsafeCoerce q) :: Q p')
                returnA -< unsafeCoerce (a :: A p')
-}

{-------- CAUCHY REAL COMPUTATION --------}
    
type CauchyRealA to = QAComputation to CauchyRealP
type CauchyReal = CauchyRealA (->)
type CauchyRealWithNI to = CauchyRealA (WithNetInfo to)

{-
    TODO: use a type class instance  
-}
piA :: (Arrow to) => CauchyRealA to
piA =
    QAComputation CauchyRealP $
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
                a1 <- q2a1 -< (accuracy+1)
                a2 <- q2a2 -< (accuracy+1) -- TODO: improve efficiency
                returnA -< a1 + a2

{-------- CAUCHY REAL PROTOCOL --------}

data CauchyRealP = CauchyRealP
    deriving (Show)

instance QAProtocol CauchyRealP
    where
    type Q CauchyRealP = Accuracy
    type A CauchyRealP = MPBall

type Accuracy = Integer

data MPBall = MPBall { centre :: MPNum, radius :: Double }
    deriving Show
type MPNum = Double -- MOCKUP

instance Num MPBall where
    (MPBall c1 r1) + (MPBall c2 r2) = MPBall (c1+c2) (r1+r2)

{-------- GENERAL QA PROTOCOLS --------}

class (Arrow to) => ArrowQA to where
    newQAComputation :: p -> ((Q p) `to` (A p)) `to` (QAComputation to p) 

class (Show (Q p), Show (A p), Show p) => QAProtocol p where
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
    nodeId <- getSelfNode
    -- create shared variable to signal when initialisation is finished: 
    nodesTV <- liftIO $ STM.atomically $ STM.newTVar Nothing

    say "Registering the ERNetNodeInit process..."
    listenerERNetNodeInit <- spawnLocal $ listenERNetNodeInit nodeId nodesTV
    register "ERNetNodeInit" listenerERNetNodeInit
    
    say "Searching for peers..."
    peers <- liftIO delayAndFindPeers
    -- announce peers to myself:
    mapM_ (send listenerERNetNodeInit . NewNode netId) peers
    
    say $ "Sending NewNode message to peers " ++ show peers ++ "..."
    mapM_ (sendNewNode nodeId) peers
    
    nodes <- waitUntilInitialised nodesTV
    return nodes
    where
    delayAndFindPeers = 
        do
        -- wait a random bit to avoid many nodes broadcasting at the same time:
        randomDelay <- randomRIO (1,10000)
        threadDelay randomDelay
        -- search for peers using broadcast:
        findPeers backend 1000000
    
    sendNewNode nodeId peer =
        nsendRemote peer "ERNetNodeInit" (NewNode netId nodeId)
    
    waitUntilInitialised nodesTV =
        do
        liftIO $ STM.atomically $ do
            maybeNodes <- STM.readTVar nodesTV
            case maybeNodes of
                Just nodes -> return nodes
                _ -> STM.retry
    
    listenERNetNodeInit nodeId nodesTV =
        do
        announcedNodes <- waitForNewNodesUntilSilence []
        say $ "announcedNodes = " ++ show announcedNodes
        initMergeNodes announcedNodes
        syncNodes nodeId nodesTV announcedNodes
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
        nodeId <- getSelfNode
        if Set.findMin announcedNodes == nodeId
            then 
                do
                say "initMergeNodes: first node"
                nsend "ERNetNodeInit" (NewNodes announcedNodes)
            else return ()
        
    syncNodes nodeId nodesTV announcedNodes =
        forever $
            receiveWait [match newNodes, match allNodes]
        where
        newNodes (NewNodes nodes')
            | nodeId == lastNode =
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
            nextNode = Set.elemAt (Set.findIndex nodeId nodes + 1) nodes
        allNodes (AllNodes nodes) 
            | nodeId /= lastNode = 
                do
                say "getAllNodes: forwarding to next"
                nsendRemote nextNode "ERNetNodeInit" (AllNodes nodes)
                signalInitDone
            | otherwise =
                signalInitDone
            where
            lastNode = Set.findMax nodes
            nextNode = Set.elemAt (Set.findIndex nodeId nodes + 1) nodes
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