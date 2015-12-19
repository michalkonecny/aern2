module AERN2.Net.Interpreted.SerialAccuracyQ where

import AERN2.Real
import Data.String (fromString)
import AERN2.Net.Spec
import AERN2.Net.Spec.Examples (_example1_NS)

import qualified Data.Graph.Inductive.Graph as G
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Map as Map


_exampleExecution :: MPBall
_exampleExecution =
    cauchyReal2ball (executeClosedNetwork _example1_NS) (bits 1000)

executeClosedNetwork :: NetworkSpec -> CauchyReal
executeClosedNetwork (NetworkSpec _process processes connections) =
{-
    * For each real input socket, define a corresponding CauchyReal value using its unique source.
    * (Other types than real not supported yet.)
    * A console interface allows the user to query any of the display nodes. 
-}
    node2value displayInputNode
    where
    displayInputNode =
        fromJust $
        do
        (ProcessInNetSpec _ inNodes _) <- Map.lookup "result" processes
        Map.lookup (InputSocket "sink") inNodes
    inputValues :: Map.Map SocketSpec (RealType, CauchyReal) 
        -- TODO: use an existential type to allow other types
    inputValues =
        Map.map computeValue inputSockets
        where
        computeValue (t, SocketSpec sourceProcId sourceSockId) = (t, value)
            where
            value =
                fromJust $
                do
                (ProcessInNetSpec sourceProcSpec inSockets _outSockets) <- Map.lookup sourceProcId processes
                let name = procSpec_name sourceProcSpec
                let inputs = Map.map node2value inSockets
                Map.lookup sourceSockId $ executeProcess name inputs
    node2value = socketSpec2value . node2SocketSpec
    node2SocketSpec = fromJust . (G.lab connections)
    socketSpec2value socketSpec = snd $ fromJust $ Map.lookup socketSpec inputValues 
    inputSockets :: Map.Map SocketSpec (RealType, SocketSpec)
    inputSockets =
        Map.fromList $
        catMaybes $ map getSocketInfo $ 
        map (G.context connections) $ G.nodes connections
        where
        getSocketInfo ([(t, sourceNode)], _, socketSpec, []) =
            do
            sourceSocketSpec <- G.lab connections sourceNode 
            Just (socketSpec, (t, sourceSocketSpec))
        getSocketInfo _ = Nothing
        
executeProcess :: ProcessName -> Map.Map ProcessSocketId CauchyReal -> Map.Map ProcessSocketId CauchyReal
executeProcess name inputs = 
    case Map.lookup name knownProcesses of
        Just (inSockets, [outSocket], function) ->
            Map.fromList $ [(OutputSocket outSocket, head $ function inputValues)]
            where
            inputValues = map lookupValue inSockets
            lookupValue inSocket =
                fromJust $ Map.lookup (InputSocket inSocket) inputs  
    
knownProcesses :: Map.Map ProcessName ([SocketName], [SocketName], [CauchyReal] -> [CauchyReal])
knownProcesses =
    Map.fromList
    [
        ("pi", ([], ["res"], \[] -> [pi])),
        ("+R", (["op1", "op2"], ["res"], \[x,y] -> [x+y])),
        ("*R", (["op1", "op2"], ["res"], \[x,y] -> [x*y])),
        ("sqrtR", (["op"], ["res"], \[x] -> [sqrt x]))
    ]
    