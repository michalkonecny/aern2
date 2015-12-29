module FnReps.FunctionAbstraction where

import AERN2.Num

type RA = MPBall

class RF fn where
    evalMI :: fn -> RA -> RA
    constFn :: RA -> fn
    idFn :: fn
    weierstrassFn :: fn
    primitiveFn :: fn -> fn
    


