module FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Benchmarks where

import AERN2.Num

import FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Basics as PPB
import FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Evaluation as PPE
import FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Division as PPD
import FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Integration as PPI

import qualified FnReps.Polynomial.UnaryCheb.Poly.Basics as Cheb
import qualified FnReps.Polynomial.UnaryCheb.Poly.SineCosine as SinCos

rungeMax :: Rational -> Integer -> Integer -> MPBall
rungeMax threshold its rangeBits = m
  where
  Interval _ m = range (bits rangeBits) runge $ Interval (mpBall $ -1) (mpBall 1)
  runge = inverse (fromPoly $ 100*Cheb.fromList [(1, mpBall 1)] * Cheb.fromList [(1, mpBall 1)] + 1) threshold its
  
rungeIntegral :: Rational -> Integer -> MPBall
rungeIntegral threshold its = 
  integral runge (mpBall $ -1) (mpBall 1)
  where
  runge = inverse (fromPoly $ 100*Cheb.fromList [(1, mpBall 1)] * Cheb.fromList [(1, mpBall 1)] + 1) threshold its
  
fracSinMax :: Cheb.Degree -> Rational -> Integer -> Integer -> MPBall
fracSinMax deg threshold its rangeBits = m
  where
  Interval _ m = range (bits rangeBits) fracSin $ Interval (mpBall $ -1) (mpBall 1)
  fracSin = inverse (fromPoly $ 10*sinp*sinp + 1) threshold its
  sinp = (SinCos.sine_poly deg NormZero $ Cheb.fromList [(1,mpBall 7)])
  
fracSinIntegral :: Cheb.Degree -> Rational -> Integer -> MPBall
fracSinIntegral deg threshold its = integral fracSin (mpBall $ -1) (mpBall 1)
  where
  fracSin = inverse (fromPoly $ 10*sinp*sinp + 1) threshold its
  sinp = (SinCos.sine_poly deg NormZero $ Cheb.fromList [(1,mpBall 7)])
  
hatMax :: Integer -> MPBall
hatMax rangeBits = m
  where
  Interval _ m = range (bits rangeBits) hat $ Interval (mpBall $ -1) (mpBall 1)
  hat = linearPolygon [(-1.0, mpBall $ 1/3), (-1/3, mpBall 1), (1.0, mpBall $ -1/3)] 0.0   

hatIntegral :: MPBall
hatIntegral = integral hat (mpBall $ -1) (mpBall 1)
  where
  hat = linearPolygon [(-1.0, mpBall $ 1/3), (-1/3, mpBall 1), (1.0, mpBall $ -1/3)] 0.0   
  