module FnReps.Polynomial.UnaryChebSparse 
(
    UnaryChebSparse, fromList
)
where

import AERN2.Real

import FnReps.Polynomial.UnaryChebSparse.Basics
import FnReps.Polynomial.UnaryChebSparse.DCTMultiplication ()

_example1 :: UnaryChebSparse
_example1 = 
    fromList [(0, exactBall 1.0)]
    where
    exactBall x = rational2BallP p x
    p = prec 100

    

