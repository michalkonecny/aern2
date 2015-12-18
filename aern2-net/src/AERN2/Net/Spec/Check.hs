module AERN2.Net.Spec.Check where

import AERN2.Real
import Data.String 
import AERN2.Net.Spec.Types

import qualified Data.Graph.Inductive.Graph as G

import qualified Data.Map as Map

typeCheckNetwork :: NetworkSpec -> [String]
typeCheckNetwork (NetworkSpec processes connections) =
    -- TODO: check that all processes have all their sockets in the graph
    -- TODO: check that no process' socket appears twice in the graph
    foldl checkSocket [] $ map (G.context connections) $ G.nodes connections
    where
    checkSocket prevMessages socketContext =
        checkProcessIdAndContinue
        where
        (incomingEdges, _, socketSpec, outgoingEdges) = socketContext
        (SocketSpec processId socketId) = socketSpec
        checkProcessIdAndContinue =
            case Map.lookup processId processes of
                Just procSpec -> checkSocketWithProcSpec procSpec
                Nothing -> newMsg $ "Socket " ++ show socketSpec ++ " contains an invalid ProcessID."
        checkSocketWithProcSpec procSpec =
            case (socketId, incomingEdges, outgoingEdges) of
                (InputSocket _, _, (_:_)) ->
                    newMsg $ "Input socket " ++ show socketSpec ++ " used as an output socket."
                (InputSocket _, [], _) ->
                    newMsg $ "Input socket " ++ show socketSpec ++ " is not connected to any channel."
                (InputSocket sockName, _, []) ->
                    case Map.lookup sockName (procSpec_inputs procSpec) of
                        Just sockType ->
                            case filter (/= sockType) $ map fst incomingEdges of
                                [] -> prevMessages
                                badones ->
                                    newMsg $ 
                                        "Input socket " ++ show socketSpec ++ " of type " ++ show sockType 
                                        ++ " is connected to channels of other types: "
                                        ++ show badones ++ "."
                        _ -> 
                            newMsg $ 
                                "Input socket " ++ show socketSpec ++ " does not exist in process"
                                ++ show (procSpec_name procSpec) ++ "."
                (OutputSocket _, (_:_), _) ->
                    newMsg $ "Output socket " ++ show socketSpec ++ " used as an input socket."
                (OutputSocket sockName, [], _) ->
                    case Map.lookup sockName (procSpec_outputs procSpec) of
                        Just sockType ->
                            case filter (/= sockType) $ map fst outgoingEdges of
                                [] -> prevMessages
                                badones ->  
                                    newMsg $ 
                                        "Output socket " ++ show socketSpec ++ " of type " ++ show sockType 
                                        ++ " is connected to channels of other types: "
                                        ++ show badones ++ "."
                        _ -> 
                            newMsg $ 
                                "Output socket " ++ show sockName ++ " does not exist in process"
                                ++ show (procSpec_name procSpec) ++ "."
        newMsg msg = msg : prevMessages
    