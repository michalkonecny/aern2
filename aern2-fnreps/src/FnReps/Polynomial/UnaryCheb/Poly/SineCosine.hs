module FnReps.Polynomial.UnaryCheb.Poly.SineCosine where

import AERN2.RealFunction
import Control.Arrow

import FnReps.Polynomial.UnaryCheb.Poly.Basics
import FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding ()
import FnReps.Polynomial.UnaryCheb.Poly.DCTMultiplication ()

{-
    To compute sin(p+-pe):
    
    * compute (r+-re) = range(p)
    * compute k = round(r/(pi/2))
    * compute sin or cos of p-k*pi/2 using Taylor series
      * which degree to use?
        * keep trying higher and higher degrees until
            * the accuracy or the result worsens
            * OR the accuracy of the result is much better than pe
    * add pe to the error bound of the resulting polynomial
       
-}