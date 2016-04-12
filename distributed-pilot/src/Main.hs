{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Environment (getArgs)
import System.Random (randomRIO)

import qualified Data.List as List
import qualified Data.Set as Set

import Data.Binary
import Data.Typeable
import GHC.Generics

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
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

    -- create shared variable to signal when initialisation is finished: 
    nodesTV <- STM.atomically $ STM.newTVar Nothing
    
    runProcess node $ 
        do
        say $ "Initialising network " ++ show netId
        nodeId <- getSelfNode
        say "Registering the ERNetwork process..."
        listenerERNetwork <- spawnLocal $ listenERNetwork nodeId nodesTV
        register "ERNetwork" listenerERNetwork
        
        say "Searching for peers..."
        peers <- liftIO $ delayAndFindPeers backend
        -- announce peers to myself:
        mapM_ (send listenerERNetwork . NewNode netId) peers
        
        say $ "Sending NewNode message to peers " ++ show peers ++ "..."
        mapM_ (sendNewNode nodeId) peers
        
        nodes <- waitUntilInitialised nodesTV
        say $ "Network initialised, found nodes:\n" ++ (List.intercalate "\n" $ map show nodes)
        
        expect

    where
    delayAndFindPeers backend = 
        do
        -- wait a random bit to avoid many nodes broadcasting at the same time:
        randomDelay <- randomRIO (1,10000)
        threadDelay randomDelay
        -- search for peers using broadcast:
        findPeers backend 1000000
    
    sendNewNode nodeId peer =
        nsendRemote peer "ERNetwork" (NewNode netId nodeId)
    
    waitUntilInitialised nodesTV =
        do
        liftIO $ STM.atomically $ do
            maybeNodes <- STM.readTVar nodesTV
            case maybeNodes of
                Just nodes -> return nodes
                _ -> STM.retry
    
    listenERNetwork nodeId nodesTV =
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
                nsend "ERNetwork" (NewNodes announcedNodes)
            else return ()
        
    syncNodes nodeId nodesTV announcedNodes =
        forever $
            receiveWait [match newNodes, match allNodes]
        where
        newNodes (NewNodes nodes')
            | nodeId == lastNode =
                do
                say "mergeNodes: AllNodes to first"
                nsendRemote firstNode "ERNetwork" (AllNodes nodes)
            | otherwise =
                do
                say "mergeNodes: forwarding to next"
                nsendRemote nextNode "ERNetwork" (NewNodes nodes)
            where
            nodes = announcedNodes `Set.union` nodes'
            firstNode = Set.findMin nodes
            lastNode = Set.findMax nodes
            nextNode = Set.elemAt (Set.findIndex nodeId nodes + 1) nodes
        allNodes (AllNodes nodes) 
            | nodeId /= lastNode = 
                do
                say "getAllNodes: forwarding to next"
                nsendRemote nextNode "ERNetwork" (AllNodes nodes)
                signalInitDone
            | otherwise =
                signalInitDone
            where
            lastNode = Set.findMax nodes
            nextNode = Set.elemAt (Set.findIndex nodeId nodes + 1) nodes
            signalInitDone =
                liftIO $ STM.atomically $ do
                    STM.writeTVar nodesTV (Just $ Set.toAscList nodes)
            
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