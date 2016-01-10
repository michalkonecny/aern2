module AERN2.Num
    (module AERN2.Num.Operations,
     module AERN2.Num.SymbolicArrow, exprAvar,
     module AERN2.Num.Norm,
     module AERN2.Num.MPBall,
     module AERN2.Num.Accuracy,
     module AERN2.Num.IntegerRational,
     module AERN2.Num.CauchyReal,
     module AERN2.Num.Complex)
where

import AERN2.Num.Operations
import AERN2.Num.SymbolicArrow
import AERN2.Num.Norm
import AERN2.Num.Accuracy
import AERN2.Num.IntegerRational
import AERN2.Num.MPBall
import AERN2.Num.CauchyReal
import AERN2.Num.Complex

{- example arrow-generic expressions -}

exprAvar :: String -> RealExpr
exprAvar = var -- must be defined in this module because it is explicitly qualified by this module in the splices

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
    
