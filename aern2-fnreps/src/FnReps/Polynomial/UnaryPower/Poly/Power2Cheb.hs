module FnReps.Polynomial.UnaryPower.Poly.Power2Cheb 
(
power2Cheb
)
where

import AERN2.Num

import qualified FnReps.Polynomial.UnaryPower.Poly.Basics as Pow
import qualified FnReps.Polynomial.UnaryCheb.Poly as Cheb

power2Cheb :: Pow.Poly -> Cheb.Poly
power2Cheb p@(Pow.Poly ts) = Cheb.normaliseCoeffs $ evalHornerAcc (Pow.degree p) $ Cheb.fromList [(0,mpBall 0)]
                             where
                             x = Cheb.fromList [(0, mpBall 0), (1, mpBall 1)]
                             evalHornerAcc 0 sm = x*sm + Pow.terms_lookupCoeff ts 0
                             evalHornerAcc k sm = evalHornerAcc (k - 1) $! x*sm + Pow.terms_lookupCoeff ts k