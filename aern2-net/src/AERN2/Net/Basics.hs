module AERN2.Net.Basics where

import AERN2.Real

data ProcessSpec
    = ProcessSpec
    {
        procSpec_name :: String,
        -- TODO: add also a URL of some authoritative definition
        procSpec_inputs :: [RealType],
        procSpec_outputs :: [RealType]
    }

{- Types for channel specifications -}

data RealType 
    = RT_Basic BasicRealType
    | RT_RealFunction RT_Smoothness Integer
    | RT_Function [RealType] RealType
    | RT_Tuple [RealType]
    | RT_Interval (RealType, RealType)

data RT_Smoothness = RT_Smoothness_C Integer | RT_Smoothness_CInfinity | RT_Smoothness_Analytic 

data BasicRealType 
    = BRT_Other String
    | BRT_ComplexNumber
    | BRT_RealNumber -- TODO: Should we add also range types?
    | BRT_Rational
    | BRT_Integer
    | BRT_Boolean


