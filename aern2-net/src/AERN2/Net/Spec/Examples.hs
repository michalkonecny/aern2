module AERN2.Net.Spec.Examples 
(
    module AERN2.Net.Spec.Examples,
    module AERN2.Net.Spec.Check
)
where

import AERN2.Real
import Data.String (fromString)

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Map as Map

import AERN2.Net.Spec.Types
import AERN2.Net.Spec.Check

{- some example process specifications -}

_pi_PS :: ProcessSpec
_pi_PS = const_PS "pi" real_RT
    
_sqrtR_PS :: ProcessSpec
_sqrtR_PS = unary_PS "sqrtR" real_RT
    
_plusR_PS :: ProcessSpec
_plusR_PS = binary_PS "+R" real_RT
    
_timesR_PS :: ProcessSpec
_timesR_PS = binary_PS "*R" real_RT
    
_displayR_PS :: ProcessSpec
_displayR_PS = sink_PS "displayR" real_RT
    
empty_PS :: ProcessName -> ProcessSpec
empty_PS name =
    ProcessSpec 
    {
        procSpec_name = name,
        procSpec_inputs = Map.fromList [],
        procSpec_outputs = Map.fromList []
    }
    
const_PS :: ProcessName -> RealType -> ProcessSpec
const_PS name t =
    ProcessSpec 
    {
        procSpec_name = name,
        procSpec_inputs = Map.fromList [],
        procSpec_outputs = Map.fromList [("res", t)]
    }
    
unary_PS :: ProcessName -> RealType -> ProcessSpec
unary_PS name t =
    ProcessSpec 
    {
        procSpec_name = name,
        procSpec_inputs = Map.fromList [("op", t)],
        procSpec_outputs = Map.fromList [("res", t)]
    }
    
binary_PS :: ProcessName -> RealType -> ProcessSpec
binary_PS name t =
    ProcessSpec 
    {
        procSpec_name = name,
        procSpec_inputs = Map.fromList [("op1", t), ("op2", t)],
        procSpec_outputs = Map.fromList [("res", t)]
    }

sink_PS :: ProcessName -> RealType -> ProcessSpec
sink_PS name t =
    ProcessSpec 
    {
        procSpec_name = name,
        procSpec_inputs = Map.fromList [("sink", t)],
        procSpec_outputs = Map.fromList []
    }
    

{- some example network specifications -}

{-|
    A closed network computing and displaying the constant real number:
     
    @pi * sqrt(pi)@  
-}
_example1_NS :: NetworkSpec
_example1_NS =
    NetworkSpec
    {
        netSpec_process = empty_PS "Dpisqrtpi",
        netSpec_subprocesses = Map.fromList 
            [
--                ("pi0", _pi_PS),
                ("pi1", ProcessInNetSpec _pi_PS Map.empty (Map.fromList [(OutputSocket "res", piOut)])),
                ("sqrt1", ProcessInNetSpec _sqrtR_PS (Map.fromList [(InputSocket "op", sqrt1In)]) (Map.fromList [(OutputSocket "res", sqrt1Out)])),
                ("times1", ProcessInNetSpec _timesR_PS  (Map.fromList [(InputSocket "op1", times1In1), (InputSocket "op2", times1In2)]) (Map.fromList [(OutputSocket "res", times1Out)])),
                ("result", ProcessInNetSpec _displayR_PS (Map.fromList [(InputSocket "sink", uiIn)]) Map.empty)
            ],
         netSpec_connections = 
            G.mkGraph nodes edges
    }
    where
    nodes = 
        [
--            (aux, SocketSpec "pi1" (OutputSocket "res")),
            (piOut, SocketSpec "pi1" (OutputSocket "res")),
            
            (times1In1, SocketSpec "times1" (InputSocket "op1")),
            (times1In2, SocketSpec "times1" (InputSocket "op2")),
            (times1Out, SocketSpec "times1" (OutputSocket "res")),
            (sqrt1In, SocketSpec "sqrt1" (InputSocket "op")),
            (sqrt1Out, SocketSpec "sqrt1" (OutputSocket "res")),
            (uiIn, SocketSpec "result" (InputSocket "sink"))
        ]
    edges = 
        [
            (piOut, times1In1, real_RT),
            (piOut, sqrt1In, real_RT),
            (sqrt1Out, times1In2, real_RT),
            (times1Out, uiIn, real_RT)
        ]
    piOut:sqrt1In:sqrt1Out:times1In1:times1In2:times1Out:uiIn
--        :aux
        : _ = map toInt [1..]
    
    
{-| 
    An open network computing the unary function:
    
    @x * sqrt(x)@ 
-}
_example2_NS :: NetworkSpec
_example2_NS =
    NetworkSpec
    {
        netSpec_process = unary_PS "xsqrtx" real_RT,
        netSpec_subprocesses = Map.fromList 
            [
                ("sqrt1", ProcessInNetSpec _sqrtR_PS (Map.fromList [(InputSocket "op", sqrt1In)]) (Map.fromList [(OutputSocket "res", sqrt1Out)])),
                ("times1", ProcessInNetSpec _timesR_PS  (Map.fromList [(InputSocket "op1", times1In1), (InputSocket "op2", times1In2)]) (Map.fromList [(OutputSocket "res", times1Out)]))
            ],
         netSpec_connections = 
            G.mkGraph nodes edges
    }
    where
    nodes = 
        [
            (selfIn, SocketSpec "self" (OutputSocket "op")), -- input socket on the outside, ie output socket for its subprocesses
            (times1In1, SocketSpec "times1" (InputSocket "op1")),
            (times1In2, SocketSpec "times1" (InputSocket "op2")),
            (times1Out, SocketSpec "times1" (OutputSocket "res")),
            (sqrt1In, SocketSpec "sqrt1" (InputSocket "op")),
            (sqrt1Out, SocketSpec "sqrt1" (OutputSocket "res")),
            (selfOut, SocketSpec "self" (InputSocket "res")) -- output socket on the outside, ie input socket for its subprocesses
        ]
    edges = 
        [
            (selfIn, times1In1, real_RT),
            (selfIn, sqrt1In, real_RT),
            (sqrt1Out, times1In2, real_RT),
            (times1Out, selfOut, real_RT)
        ]
    sqrt1In:sqrt1Out:times1In1:times1In2:times1Out:selfIn:selfOut: _ = map toInt [1..]
    