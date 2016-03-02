module FnReps.Polynomial.UnaryCheb.Poly.Division where

import FnReps.Polynomial.UnaryCheb.Poly.Basics
import FnReps.Polynomial.UnaryCheb.Poly.DCTMultiplication


divideDCT_terms :: Degree -> Terms -> Terms -> Terms
divideDCT_terms d = lift2_DCT (const $ const $ d) (/)
    {- 
        TODO
        Add a bound on the interpolation error.
    -}


