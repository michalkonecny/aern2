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
    
_expR_PS :: ProcessSpec
_expR_PS = unary_PS "expR" real_RT
    
_plusR_PS :: ProcessSpec
_plusR_PS = binary_PS "+" real_RT
    
_timesR_PS :: ProcessSpec
_timesR_PS = binary_PS "*" real_RT
    
_displayR_PS :: ProcessSpec
_displayR_PS = sink_PS "display" real_RT
    
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
     
    @pi * exp(pi)@  
-}
_example1_NS :: NetworkSpec
_example1_NS =
    NetworkSpec
    {
        netSpec_process = empty_PS "Dpiexppi",
        netSpec_subprocesses = Map.fromList 
            [
--                ("pi0", _pi_PS),
                ("pi1", _pi_PS),
                ("exp1", _expR_PS),
                ("times1", _timesR_PS),
                ("ui1", _displayR_PS)
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
            (exp1In, SocketSpec "exp1" (InputSocket "op")),
            (exp1Out, SocketSpec "exp1" (OutputSocket "res")),
            (uiIn, SocketSpec "ui1" (InputSocket "sink"))
        ]
    edges = 
        [
            (piOut, times1In1, real_RT),
            (piOut, exp1In, real_RT),
            (exp1Out, times1In2, real_RT),
            (times1Out, uiIn, real_RT)
        ]
    piOut:exp1In:exp1Out:times1In1:times1In2:times1Out:uiIn
--        :aux
        : _ = map toInt [1..]
    
    
{-| 
    An open network computing the unary function:
    
    @x * exp(x)@ 
-}
_example2_NS :: NetworkSpec
_example2_NS =
    NetworkSpec
    {
        netSpec_process = unary_PS "xexpx" real_RT,
        netSpec_subprocesses = Map.fromList 
            [
                ("exp1", _expR_PS),
                ("times1", _timesR_PS)
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
            (exp1In, SocketSpec "exp1" (InputSocket "op")),
            (exp1Out, SocketSpec "exp1" (OutputSocket "res")),
            (selfOut, SocketSpec "self" (InputSocket "res")) -- output socket on the outside, ie input socket for its subprocesses
        ]
    edges = 
        [
            (selfIn, times1In1, real_RT),
            (selfIn, exp1In, real_RT),
            (exp1Out, times1In2, real_RT),
            (times1Out, selfOut, real_RT)
        ]
    exp1In:exp1Out:times1In1:times1In2:times1Out:selfIn:selfOut: _ = map toInt [1..]
    