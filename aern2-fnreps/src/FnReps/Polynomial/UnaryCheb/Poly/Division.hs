module FnReps.Polynomial.UnaryCheb.Poly.Division where

import FnReps.Polynomial.UnaryCheb.Poly.Basics
import FnReps.Polynomial.UnaryCheb.Poly.DCTMultiplication


divideDCT_terms :: Terms -> Terms -> Terms
divideDCT_terms = lift2_DCT (/)
    {- 
        TODO
        Add a bound on the interpolation error.
    -}


