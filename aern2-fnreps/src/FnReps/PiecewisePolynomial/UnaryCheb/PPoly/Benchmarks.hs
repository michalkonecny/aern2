module FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Benchmarks where

import AERN2.Num
import AERN2.RealFunction
import AERN2.Net

import FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Basics as PPB
import FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Evaluation as PPE
import FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Division as PPD
import FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Integration as PPI

import qualified FnReps.Polynomial.UnaryCheb.Poly.Basics as Cheb
import qualified FnReps.Polynomial.UnaryCheb.Poly.SineCosine as SinCos

import Data.PQueue.Max (MaxQueue)
import qualified Data.PQueue.Max as Q
import qualified Prelude as Prelude

rungeMax :: Rational -> Integer -> Precision -> Accuracy -> MPBall
rungeMax threshold its pr acc = m
  where
  Interval _ m = range acc runge $ Interval (mpBall $ -1) (mpBall 1)
  runge = inverse (setPrecision pr $ fromPoly $ 100*Cheb.fromList [(1, mpBall 1)] * Cheb.fromList [(1, mpBall 1)] + 1) threshold its acc
  
rungeIntegral :: Rational -> Integer -> Precision -> Accuracy -> MPBall
rungeIntegral threshold its pr acc = 
  integral runge (mpBall $ -1) (mpBall 1)
  where
  runge = inverse (setPrecision pr $ fromPoly $ 100*Cheb.fromList [(1, mpBall 1)] * Cheb.fromList [(1, mpBall 1)] + 1) threshold its acc
 
fracSinPoly :: Cheb.Degree -> Cheb.Poly
fracSinPoly deg = 
  10*sinp*sinp + 1
  where
  sinp = SinCos.sine_poly deg NormZero $ Cheb.fromList [(1,mpBall 7)]
  
fracSinMax :: Cheb.Degree -> Rational -> Integer -> Precision -> Accuracy -> MPBall
fracSinMax deg threshold its pr acc = m
  where
  Interval _ m = range acc fracSin $ Interval (mpBall $ -1) (mpBall 1)
  fracSin = inverse (setPrecision pr $ fromPoly $ 10*sinp*sinp + 1) threshold its acc
  sinp = (setPrecision pr $ SinCos.sine_poly deg NormZero $ Cheb.fromList [(1,mpBall 7)])   
  
fracSinIntegral :: Cheb.Degree -> Rational -> Integer -> Precision -> Accuracy -> MPBall
fracSinIntegral deg threshold its pr acc = 
  integral fracSin (mpBall $ -1) (mpBall 1)
  where
  fracSin = inverse (setPrecision pr $ fromPoly $ 10*sinp*sinp + 1) threshold its acc
  sinp = (setPrecision pr $ SinCos.sine_poly deg NormZero $ Cheb.fromList [(1,mpBall 7)])
  
hatMax :: Integer -> MPBall
hatMax rangeBits = m
  where
  Interval _ m = range (bits rangeBits) hat $ Interval (mpBall $ -1) (mpBall 1)
  hat = linearPolygon [(-1.0, mpBall $ 1/3), (-1/3, mpBall 1), (1.0, mpBall $ -1/3)] 0.0   

hatIntegral :: MPBall
hatIntegral = integral hat (mpBall $ -1) (mpBall 1)
  where
  hat = linearPolygon [(-1.0, mpBall $ 1/3), (-1/3, mpBall 1), (1.0, mpBall $ -1/3)] 0.0   
  
sinesineMax :: Cheb.Degree -> Cheb.Degree -> Accuracy -> Precision -> MPBall
sinesineMax degI degO rangeBits pr = mx
  where
  Interval _ mx = range rangeBits sinesine $ Interval (mpBall $ -1) (mpBall 1)
  sinesine = fromPoly (SinCos.sine_poly degO NormZero $ Cheb.fromList [(1,mpBall 10)] 
                     + (SinCos.sine_poly degI NormZero $ setPrecision pr (Cheb.fromList [(1, mpBall 1)]*Cheb.fromList [(1, mpBall 20)]))) 

sinesineXMax :: Cheb.Degree -> Cheb.Degree -> Accuracy -> Precision -> MPBall
sinesineXMax degI degO rangeBits pr = mx
  where
  Interval _ mx = range rangeBits sinesine $ Interval (mpBall $ -1) (mpBall 1)
  sinesine = fromPoly $ (Cheb.fromList [(1,mpBall 1)]) + 
                        (SinCos.sine_poly degO NormZero $ Cheb.fromList [(1,mpBall 10)] 
                        + (SinCos.sine_poly degI NormZero $ setPrecision pr (Cheb.fromList [(1, mpBall 1)]*Cheb.fromList [(1, mpBall 20)]))) 

sinesineCosMax :: Cheb.Degree -> Cheb.Degree -> Accuracy -> Precision -> MPBall
sinesineCosMax degI degO rangeBits pr = mx
  where
  Interval _ mx = range rangeBits sinesine $ Interval (mpBall $ -1) (mpBall 1)
  sinesine = fromPoly $ (SinCos.sine_poly degI NormZero $ Cheb.fromList [(1,mpBall 10)]) + 
                        (SinCos.sine_poly degO NormZero $ Cheb.fromList [(1,mpBall 10)] 
                        + (SinCos.sine_poly degI NormZero $ setPrecision pr (Cheb.fromList [(1, mpBall 1)]*Cheb.fromList [(1, mpBall 20)])))
              
sinesineMaxFun :: Accuracy -> MPBall
sinesineMaxFun rangeBits = cauchyReal2ball mx rangeBits
  where
  Interval _ mx = rangeOnIntervalUnaryFnA (sinesine, Interval (-1.0) (1.0))
  sinesine = UnaryFnMPBall (Interval (-1.0) 1.0) $ \x -> catchingExceptions $ sin(10*x + sin(20*x*x))                                   

sinesineCosMaxFun :: Accuracy -> MPBall
sinesineCosMaxFun rangeBits = cauchyReal2ball mx rangeBits
  where
  Interval _ mx = rangeOnIntervalUnaryFnA (sinesine, Interval (-1.0) (1.0))
  sinesine = UnaryFnMPBall (Interval (-1.0) 1.0) $ \x -> catchingExceptions $ sin(10*x + sin(20*x*x)) + sin(10*x)

sinesineCosMaxFun' :: Accuracy -> MPBall
sinesineCosMaxFun' rangeBits = 
  fnMax sinesine rangeBits
  where
  sinesine = \x -> sin(10*x + sin(20*x*x)) + sin(10*x)
       
sinesineXMaxFun :: Accuracy -> MPBall
sinesineXMaxFun rangeBits = cauchyReal2ball mx rangeBits
  where
  Interval _ mx = rangeOnIntervalUnaryFnA (sinesine, Interval (-1.0) (1.0))
  sinesine = UnaryFnMPBall (Interval (-1.0) 1.0) $ \x -> catchingExceptions $ sin(10*x + sin(20*x*x)) + x       
                     
sinesineIntegral :: Cheb.Degree -> MPBall
sinesineIntegral deg = integral sinesine (mpBall $ -1) (mpBall 1)
  where
  sinesine = fromPoly (SinCos.sine_poly deg NormZero $ Cheb.fromList [(1,mpBall 10)] 
                     + (SinCos.sine_poly deg NormZero $ Cheb.fromList [(1, mpBall 1)]*Cheb.fromList [(1, mpBall 20)]))  

{- testing range: -}

data SearchInterval = SearchInterval Rational Rational MPBall deriving Eq

evalFnOnInterval :: (MPBall -> MPBall) -> Rational -> Rational -> Accuracy -> MPBall
evalFnOnInterval f l r acc = f (ri2ball (Interval l r) acc)

instance Prelude.Ord SearchInterval where
  (<=) (SearchInterval _ _ a) (SearchInterval _ _ b) =
    toRationalUp a <= toRationalUp b
    
instance HasAccuracy SearchInterval where
  getAccuracy (SearchInterval _ _ b) = getAccuracy b    
                     
fnMax :: (MPBall -> MPBall) -> Accuracy -> MPBall
fnMax f acc = 
  aux $ Q.singleton (SearchInterval (-1.0) 1.0 (f (ri2ball (Interval (-1.0) 1.0) (max (bits 100) acc))))
  where
  aux sis = 
    let
      (h@(SearchInterval il ir a),r) = Q.deleteFindMax sis
    in
      if getAccuracy h >= acc
      || getAccuracy (ri2ball (Interval il ir) (acc + 2)) >= acc then
        a
      else
        aux $ (refine h) `Q.union` r
  refine (SearchInterval l r a) = 
    let m = (l + r)/2 in
      Q.fromList [SearchInterval l m (evalFnOnInterval f l m acc), SearchInterval m r (evalFnOnInterval f m r acc)]
      
{- enriched -}

sineSquaredMax :: Cheb.Degree -> Accuracy -> MPBall
sineSquaredMax deg rangeBits = 
  m
  where
  Interval _ m = range rangeBits sinsq $ Interval (mpBall $ -1) (mpBall 1)
  sinp =  Cheb.fromList [(1, mpBall 1)] + (SinCos.sine_poly deg NormZero $ Cheb.fromList [(1, mpBall 10)])
  sinsq = fromPoly $ sinp * sinp

sineSquaredMaxE :: Cheb.Degree -> Accuracy -> MPBall
sineSquaredMaxE deg rangeBits = 
  m
  where
  Interval _ m = rangeEnriched rangeBits sinsq (\x -> (sin(10*x) + x)^2) (\_ -> True) $ Interval (mpBall $ -1) (mpBall 1)
  sinp =  Cheb.fromList [(1, mpBall 1)] + (SinCos.sine_poly deg NormZero $ Cheb.fromList [(1, mpBall 10)])
  sinsq = fromPoly $ sinp * sinp 

sineSquaredMaxFn ::Accuracy -> MPBall
sineSquaredMaxFn rangeBits = 
  fnMax (\x -> (sin(10*x) + x)^2 ) rangeBits

sineSquaredMaxFn' ::Accuracy -> MPBall
sineSquaredMaxFn' rangeBits = 
  cauchyReal2ball m rangeBits
  where
  Interval _ m = (rangeOnIntervalUnaryFnA (fn, Interval (-1.0) (1.0)))
  fn = UnaryFnMPBall (Interval (-1.0) 1.0) $ \x -> catchingExceptions $ (sin(10*x) + x)^2

fracSinMaxE :: Cheb.Degree -> Rational -> Integer -> Precision -> Accuracy -> MPBall
fracSinMaxE deg threshold its pr acc = m
  where
  Interval _ m = rangeEnriched acc fracSin (\x -> 1/(10*(sin $ 7*x)*(sin $ 7*x) + 1)) (\_ -> True) $ Interval (mpBall $ -1) (mpBall 1)
  fracSin = inverse (setPrecision pr $ fromPoly $ 10*sinp*sinp + 1) threshold its acc
  sinp = (setPrecision pr $ SinCos.sine_poly deg NormZero $ Cheb.fromList [(1,mpBall 7)])
  
sinesineMaxE :: Cheb.Degree -> Cheb.Degree -> Accuracy -> Precision -> MPBall
sinesineMaxE degI degO rangeBits pr = mx
  where
  Interval _ mx = rangeEnriched rangeBits sinesine (\x -> sin(10*x + sin(20*x*x))) (\_ -> True) $ Interval (mpBall $ -1) (mpBall 1)
  sinesine = fromPoly (SinCos.sine_poly degO NormZero $ Cheb.fromList [(1,mpBall 10)] 
                     + (SinCos.sine_poly degI NormZero $ setPrecision pr (Cheb.fromList [(1, mpBall 1)]*Cheb.fromList [(1, mpBall 20)])))  
                      