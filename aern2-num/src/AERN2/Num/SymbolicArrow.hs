{-# LANGUAGE Arrows, TemplateHaskell, ScopedTypeVariables, FlexibleContexts, TypeOperators #-}
module AERN2.Num.SymbolicArrow 
( 
    module AERN2.Num.SymbolicArrow.TH, 
    module AERN2.Num.SymbolicArrow.Expression,
    var
)
where

import AERN2.Num.Operations
import AERN2.Num.Accuracy
import AERN2.Num.MPBall
import AERN2.Num.CauchyReal

import AERN2.Num.SymbolicArrow.Expression hiding (var)
import qualified AERN2.Num.SymbolicArrow.Expression as E
import AERN2.Num.SymbolicArrow.TH

{- usage examples -}

var :: String -> RealExpr
var = E.var -- must be defined in this module because it is explicitly qualified by this module in the splices

_rExprA1Direct :: Rational -> Rational -> Accuracy -> MPBall
_rExprA1Direct xO yO ac =
    cauchyReal2ball (myExprA (cauchyReal xO, cauchyReal yO)) ac
    where
    myExprA =
      $(exprA[|let [x,y]=vars in (x+pi)*(y+1)|])

_rPredA1Direct :: Rational -> Bool
_rPredA1Direct xO =
    (myPredA (cauchyReal xO))
    where
    myPredA =
        $(predA[|let [x]=vars in 1 <= x+pi && x+pi <= 1.5 |])
    


    

