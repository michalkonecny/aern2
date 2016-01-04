{-# LANGUAGE Arrows, TemplateHaskell, ScopedTypeVariables, FlexibleContexts #-}
module AERN2.Num.SymbolicArrow where

import qualified Data.Map as Map
import Control.Arrow

import AERN2.Num.Operations
import AERN2.Num.Accuracy
import AERN2.Num.MPBall
import AERN2.Num.CauchyReal

import AERN2.Num.SymbolicArrow.Expression
import AERN2.Num.SymbolicArrow.TH


_expr1 ::  RealExpr
_expr1 = cauchyReal2exprNamed "pi" pi

_expr2 :: RealExpr
_expr2 = rational2expr 1.5

_expr3 :: RealExpr
_expr3 = _expr1 + (var "x") + (var "x")

_expr3Direct :: Rational -> Accuracy -> MPBall
_expr3Direct xO ac =
    cauchyReal2ball (_expr3Arrow (cauchyReal xO)) ac
    where
    _expr3Arrow =
        proc x -> realExpr2arrow _expr3 -< Map.fromList [("x", x)]

_rExprA1Direct :: Rational -> Rational -> Accuracy -> MPBall
_rExprA1Direct xO yO ac =
    cauchyReal2ball (theArrow (cauchyReal xO, cauchyReal yO)) ac
    where
    theArrow =
      $(exprA[|let [x,y]=vars in (x+y)|])
--        (arr (\(x,y) -> Map.fromList [("x",x), ("y",y)])) 
--        >>>
--        (realExpr2arrow (let [x,y] = [var "x", var "y"] in (x + y)))


_pred1 :: RealPred
_pred1 =
    (integer2expr 1) <= (var "x")  
    &&
    (var "x") <= (integer2expr 2) 

{- TODO


_pred1Direct :: Rational -> Bool
_pred1Direct x =
    (realPred2arrow _pred1 (Map.fromList [("x",cauchyReal x)]))
        
-}
