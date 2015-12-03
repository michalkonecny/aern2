module FnReps.Polynomial.UnaryChebSparse 
(
    UnaryChebSparse
)
where

import qualified Data.HashMap.Strict as HM

import AERN2.Real

import FnReps.Polynomial.UnaryChebSparse.Basics
import FnReps.Polynomial.UnaryChebSparse.DCTMultiplication ()

_example1 :: UnaryChebSparse
_example1 = 
    UnaryChebSparse (HM.fromList [(0, exactBall 1.0)])
    where
    exactBall x = rational2BallP p x
    p = prec 100

    

