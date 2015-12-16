module FnReps.Polynomial.UnaryChebSparse.EvaluationRootFinding where

import AERN2.Real
import FnReps.Polynomial.UnaryChebSparse.Basics

{-|
    Take a interval polynomial P that has admits(*) only polynomials 
    without non-simple roots and return a list of balls that contain all the roots
    and each ball contains at least one root.
    
    * An interval polynomial P admits a (non-interval) polynomial p if each coefficient
    of p is inside the corresponding interval coefficient of P.
-}
findAllRoots :: Accuracy -> UnaryChebSparse -> [MPBall]
findAllRoots = undefined
{-
    TODO:
    
    First segment the domain until for each segment S, 
    either P is clearly positive on S or clearly negative on S
    or P' (the nominal derivative of P) is clearly positive on S or clearly negative on S.
    
    Then apply Newton method until the improvement is negligible compared to the interval size
    or the given accuracy threshold is reached. 
    
-}
