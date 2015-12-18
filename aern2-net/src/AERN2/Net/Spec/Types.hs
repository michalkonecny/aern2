{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AERN2.Net.Spec.Types where

import AERN2.Real

import Data.String (IsString(..))

import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Map as Map

{- Types for channel specifications -}

data RealType 
    = RT_Basic BasicRealType
    | RT_Interval (BasicRealType, BasicRealType)
    | RT_RealFunction RT_Smoothness Integer
    | RT_Tuple [RealType]
    | RT_Function [RealType] RealType
    deriving (Show, Eq)

instance HasEq RealType RealType

data RT_Smoothness 
    = RT_Smoothness_C Integer 
    | RT_Smoothness_CInfinity 
    | RT_Smoothness_Analytic 
    deriving (Show, Eq)

data BasicRealType
    = BRT_Other String
    | BRT_ComplexNumber
    | BRT_RealNumber
    | BRT_Rational
    | BRT_Integer
    | BRT_Boolean
    deriving (Show, Eq)

    
real_RT :: RealType
real_RT = RT_Basic BRT_RealNumber

complex_RT :: RealType
complex_RT = RT_Basic BRT_ComplexNumber

{- Process specifications -}

data ProcessSpec
    = ProcessSpec
    {
        procSpec_name :: ProcessName,
        -- TODO: add also a URL of some authoritative definition
        procSpec_inputs :: Map.Map SocketName RealType,
        procSpec_outputs :: Map.Map SocketName RealType
    }
    deriving (Show)

newtype ProcessName = ProcessName String
    deriving (Show, Eq, Ord, IsString)
instance HasEq ProcessName ProcessName

newtype SocketName = SocketName String
    deriving (Show, Eq, Ord, IsString)
instance HasEq SocketName SocketName

newtype ProcessID = ProcessID String
    deriving (Show, Eq, Ord, IsString)
instance HasEq ProcessID ProcessID

processInvertInOut :: ProcessSpec -> ProcessSpec
processInvertInOut ps =
    ps
    { 
        procSpec_outputs = procSpec_inputs ps,  
        procSpec_inputs = procSpec_outputs ps  
    }

{- Dataflow network specifications -}

data NetworkSpec =
    NetworkSpec
    {
        netSpec_process :: ProcessSpec,
        netSpec_subprocesses :: Map.Map ProcessID ProcessSpec,
        netSpec_connections :: Gr SocketSpec RealType
    } 
    deriving (Show)

data SocketSpec =
    SocketSpec
    {
        sockSpec_process_id :: ProcessID,
        sockSpec_process_socket_id :: ProcessSocketId
    }
    deriving (Show)

data ProcessSocketId
    = InputSocket SocketName
    | OutputSocket SocketName
    deriving (Show)

socketInvertInOut :: ProcessSocketId -> ProcessSocketId
socketInvertInOut (InputSocket name) = OutputSocket name
socketInvertInOut (OutputSocket name) = InputSocket name

