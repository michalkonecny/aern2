{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}
module AERN2.Net.Spec.Symbolic where

import AERN2.Real
import Data.String (IsString(..),fromString)

import AERN2.Net.Spec.Arrow
--import Control.Arrow
import qualified Data.Map as Map

{- TODO
    Provide a way to define simple networks using familiar arithmetic expressions, such as:
    
    > net :: (HasRealOps to r) => (r `to` r)
    > net = toArrow $ let x = var "x" in pi * sqrt(x) * x 
    
-}

data RealExpr' a
    = Var VarName
    | Const (Maybe String) CauchyReal 
    | UnaryOp String a 
    | BinaryOp String a a 

newtype VarName = VarName String
    deriving (Show, IsString)

data RealExpr = RealExpr (RealExpr' RealExpr)
-- Use Data.Fix from data-fix 0.0.1?

toArrow :: (HasRealOps to r) => RealExpr -> ((Map.Map VarName r) `to` r)
toArrow =
    undefined -- TODO