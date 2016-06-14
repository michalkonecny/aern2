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

import qualified Data.PQueue.Max as Q
import qualified Prelude as Prelude


{- FnReps Benchmarks with parameters -}

{- poly/ppoly sin(10x + sin(20x^2)) -}

  {- maximum -}
  
runSineSineMax :: IO ()
runSineSineMax = 
  do
  let a = sinesineMax 100 100 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 2
  let a = sinesineMax 100 150 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 11
  let a = sinesineMax 100 200 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 21
  let a = sinesineMax 100 300 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 44
  let a = sinesineMax 100 400 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 69
  let a = sinesineMax 100 500 (bits 200) (prec 200) in print $ (getAccuracy a, a) -- Bits 96
  let a = sinesineMax 150 600 (bits 400) (prec 400) in print $ (getAccuracy a, a) -- Bits 124  
  
  {- integral -}

runSineSineIntegral :: IO ()
runSineSineIntegral = 
  do
  let a = sinesineIntegral 100 100 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 2
  let a = sinesineIntegral 100 150 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 11
  let a = sinesineIntegral 100 200 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 21
  let a = sinesineIntegral 100 300 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 44
  let a = sinesineIntegral 100 400 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 69
  let a = sinesineIntegral 100 500 (bits 200) (prec 200) in print $ (getAccuracy a, a) -- Bits 96
  let a = sinesineIntegral 150 600 (bits 400) (prec 400) in print $ (getAccuracy a, a) -- Bits 124
  
{- poly/ppoly sin(10x + sin(20x^2)) + sin(10x) -}

  {- maximum -}
  
runSineSineCosMax :: IO ()
runSineSineCosMax = 
  do
  let a = sinesineCosMax 100 100 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 2
  let a = sinesineCosMax 100 150 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 11
  let a = sinesineCosMax 100 200 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 21
  let a = sinesineCosMax 100 300 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 44
  let a = sinesineCosMax 100 400 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 69
  let a = sinesineCosMax 100 500 (bits 200) (prec 200) in print $ (getAccuracy a, a) -- Bits 96
  let a = sinesineCosMax 150 600 (bits 400) (prec 400) in print $ (getAccuracy a, a) -- Bits 124    
  
  {- integral -}
  
runSineSineCosIntegral :: IO ()
runSineSineCosIntegral = 
  do
  let a = sinesineCosIntegral 100 100 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 2
  let a = sinesineCosIntegral 100 150 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 11
  let a = sinesineCosIntegral 100 200 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 21
  let a = sinesineCosIntegral 100 300 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 44
  let a = sinesineCosIntegral 100 400 (bits 100) (prec 100) in print $ (getAccuracy a, a) -- Bits 69
  let a = sinesineCosIntegral 100 500 (bits 100) (prec 200) in print $ (getAccuracy a, a) -- Bits 96
  let a = sinesineCosIntegral 150 600 (bits 100) (prec 400) in print $ (getAccuracy a, a) -- Bits 124
  
{- ppoly 1/(1 + 100*x^2) -}
  
  {- maximum -}
  
runRungeMax :: IO ()
runRungeMax = 
  do
  let m = rungeMax 0.1 1 (prec 100) (bits 100)   in print $ (getAccuracy m, m) -- Bits 6
  let m = rungeMax 0.1 2 (prec 100) (bits 100)   in print $ (getAccuracy m, m) -- Bits 13
  let m = rungeMax 0.1 3 (prec 150) (bits 150)   in print $ (getAccuracy m, m) -- Bits 26
  let m = rungeMax 0.01 3 (prec 150) (bits 150)  in print $ (getAccuracy m, m) -- Bits 40
  let m = rungeMax 0.001 3 (prec 150) (bits 150) in print $ (getAccuracy m, m) -- Bits 48
  let m = rungeMax 0.05 4 (prec 300) (bits 300)  in print $ (getAccuracy m, m) -- bits 69
  let m = rungeMax 0.01 4 (prec 300) (bits 300)  in print $ (getAccuracy m, m) -- bits 100
  
  {- integral -}
  
runRungeIntegral :: IO ()
runRungeIntegral = 
  do
  let m = rungeIntegral 0.1 1 (prec 100) (bits 100)   in print $ (getAccuracy m, m) -- Bits 6
  let m = rungeIntegral 0.1 2 (prec 100) (bits 100)   in print $ (getAccuracy m, m) -- Bits 13
  let m = rungeIntegral 0.1 3 (prec 150) (bits 150)   in print $ (getAccuracy m, m) -- Bits 26
  let m = rungeIntegral 0.01 3 (prec 150) (bits 150)  in print $ (getAccuracy m, m) -- Bits 27
  let m = rungeIntegral 0.001 3 (prec 150) (bits 150) in print $ (getAccuracy m, m) -- Bits 26
  let m = rungeIntegral 0.05 4 (prec 300) (bits 300)  in print $ (getAccuracy m, m) -- bits 69
  let m = rungeIntegral 0.01 4 (prec 300) (bits 300)  in print $ (getAccuracy m, m) -- bits 74  
  
{- ppoly x/(1 + 100*x^2) -}

  {- maximum -}
  
runRungeXMax :: IO ()
runRungeXMax = 
  do
  let m = rungeXMax 0.1 1 (prec 100) (bits 100)   in print $ (getAccuracy m, m) -- Bits 6
  let m = rungeXMax 0.1 2 (prec 100) (bits 100)   in print $ (getAccuracy m, m) -- Bits 13
  let m = rungeXMax 0.1 3 (prec 150) (bits 150)   in print $ (getAccuracy m, m) -- Bits 26
  let m = rungeXMax 0.01 3 (prec 150) (bits 150)  in print $ (getAccuracy m, m) -- Bits 29
  let m = rungeXMax 0.001 3 (prec 150) (bits 150) in print $ (getAccuracy m, m) -- Bits 28
  let m = rungeXMax 0.05 4 (prec 300) (bits 300)  in print $ (getAccuracy m, m) -- bits 69
  let m = rungeXMax 0.01 4 (prec 300) (bits 300)  in print $ (getAccuracy m, m) -- bits 79  
  
  {- integral -}

runRungeXIntegral :: IO ()
runRungeXIntegral = 
  do
  let m = rungeXIntegral 0.1 1 (prec 100) (bits 100)   in print $ (getAccuracy m, m) -- Bits 6
  let m = rungeXIntegral 0.1 2 (prec 100) (bits 100)   in print $ (getAccuracy m, m) -- Bits 13
  let m = rungeXIntegral 0.1 3 (prec 150) (bits 150)   in print $ (getAccuracy m, m) -- Bits 26
  let m = rungeXIntegral 0.01 3 (prec 150) (bits 150)  in print $ (getAccuracy m, m) -- Bits 29
  let m = rungeXIntegral 0.001 3 (prec 150) (bits 150) in print $ (getAccuracy m, m) -- Bits 28
  let m = rungeXIntegral 0.05 4 (prec 300) (bits 300)  in print $ (getAccuracy m, m) -- bits 69
  let m = rungeXIntegral 0.01 4 (prec 300) (bits 300)  in print $ (getAccuracy m, m) -- bits 79  
  
{- ppoly 1/(10 (sin(7x))^2 + 1) -}

  {- maximum -}
  
runFracSinMax :: IO ()
runFracSinMax = 
  do 
  let a = fracSinMax 20 0.5 1 (prec 100) (bits 100) in print $ (getAccuracy a, a) -- bits 2
  let a = fracSinMax 20 0.1 1 (prec 100) (bits 100) in print $ (getAccuracy a, a) -- bits 5
  let a = fracSinMax 30 0.1 2 (prec 100) (bits 100) in print $ (getAccuracy a, a) -- bits 13
  let a = fracSinMax 40 0.1 3 (prec 800) (bits 200) in print $ (getAccuracy a, a) -- bits 25
  let a = fracSinMax 40 0.05 3 (prec 800) (bits 200) in print $ (getAccuracy a, a) -- bits 30  
  
  {- integral -}

runFracSinIntegral :: IO ()
runFracSinIntegral = 
  do 
  let a = fracSinIntegral 20 0.5 1 (prec 100) (bits 100)  in print $ (getAccuracy a, a) -- bits 2
  let a = fracSinIntegral 20 0.1 1 (prec 100) (bits 100)  in print $ (getAccuracy a, a) -- bits 4
  let a = fracSinIntegral 30 0.1 2 (prec 100) (bits 100)  in print $ (getAccuracy a, a) -- bits 13
  let a = fracSinIntegral 40 0.1 3 (prec 800) (bits 200)  in print $ (getAccuracy a, a) -- bits 16
  let a = fracSinIntegral 40 0.05 3 (prec 800) (bits 200) in print $ (getAccuracy a, a) -- bits 18  

{- ppoly 1 - |x + 1/3| -}

  {- maximum -}
  
runHatMax :: IO ()
runHatMax = 
  do
  let a = hatMax (prec 100) (bits 100) in print $ (getAccuracy a, a)  
  
  {- integral -}

runHatIntegral :: IO ()
runHatIntegral = 
  do
  let a = hatIntegral (prec 100) in print $ (getAccuracy a, a)

{- -------------------- -}

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
 
rungeXMax :: Rational -> Integer -> Precision -> Accuracy -> MPBall
rungeXMax threshold its pr acc = m
  where
  Interval _ m = range acc runge $ Interval (mpBall $ -1) (mpBall 1)
  runge = (setPrecision pr $ Cheb.fromList [(1, mpBall 1)]) * inverse (setPrecision pr $ fromPoly $ 100*Cheb.fromList [(1, mpBall 1)] * Cheb.fromList [(1, mpBall 1)] + 1) threshold its acc
   
rungeXIntegral :: Rational -> Integer -> Precision -> Accuracy -> MPBall
rungeXIntegral threshold its pr acc = 
  integral runge (mpBall $ -1) (mpBall 1)
  where
  runge = (setPrecision pr $ Cheb.fromList [(1, mpBall 1)]) * inverse (setPrecision pr $ fromPoly $ 100*Cheb.fromList [(1, mpBall 1)] * Cheb.fromList [(1, mpBall 1)] + 1) threshold its acc   

rungeXMaxFn :: Accuracy -> MPBall
rungeXMaxFn acc = 
  cauchyReal2ball mx acc 
  where
  Interval _ mx = 
    rangeOnIntervalUnaryFnA ((UnaryFnMPBall (Interval (-1.0) 1.0) $ \x -> (catchingExceptions x)/(catchingExceptions (1 + 100*x^2))), Interval (-1.0) 1.0)
  
fracSinMax :: Cheb.Degree -> Rational -> Integer -> Precision -> Accuracy -> MPBall
fracSinMax deg threshold its pr acc = m
  where
  Interval _ m = range acc fracSin $ Interval (mpBall $ -1) (mpBall 1)
  fracSin = inverse (setPrecision pr $ fromPoly $ 10*sinp*sinp + 1) threshold its acc
  sinp = (setPrecision pr $ SinCos.sine_poly deg NormZero $ Cheb.fromList [(1,mpBall 7)])
  
fracSinMaxFn :: Accuracy -> MPBall
fracSinMaxFn acc = 
  cauchyReal2ball mx acc 
  where
  Interval _ mx = 
    rangeOnIntervalUnaryFnA ((UnaryFnMPBall (Interval (-1.0) 1.0) $ \x -> 1/(catchingExceptions $ 10*(sin(7*x))^2 + 1)), Interval (-1.0) 1.0)    
  
fracSinIntegral :: Cheb.Degree -> Rational -> Integer -> Precision -> Accuracy -> MPBall
fracSinIntegral deg threshold its pr acc = 
  integral fracSin (mpBall $ -1) (mpBall 1)
  where
  fracSin = inverse (setPrecision pr $ fromPoly $ 10*sinp*sinp + 1) threshold its acc
  sinp = (setPrecision pr $ SinCos.sine_poly deg NormZero $ Cheb.fromList [(1,mpBall 7)])
  
runFracSinXMax :: IO ()
runFracSinXMax = 
  do 
  let a = fracSinXMax 20 0.5 1 (prec 100) (bits 100) in print $ (getAccuracy a, a) -- bits 1
  let a = fracSinXMax 20 0.1 1 (prec 100) (bits 100) in print $ (getAccuracy a, a) -- bits 4
  let a = fracSinXMax 30 0.1 2 (prec 100) (bits 100) in print $ (getAccuracy a, a) -- bits 12
  let a = fracSinXMax 40 0.1 3 (prec 800) (bits 200) in print $ (getAccuracy a, a) -- bits 19
  let a = fracSinXMax 40 0.05 3 (prec 800) (bits 200) in print $ (getAccuracy a, a) -- bits 19  
  
fracSinXMax :: Cheb.Degree -> Rational -> Integer -> Precision -> Accuracy -> MPBall
fracSinXMax deg threshold its pr acc = m
  where
  Interval _ m = range acc fracSin $ Interval (mpBall $ -1) (mpBall 1)
  fracSin = x * inverse (setPrecision pr $ fromPoly $ 10*sinp*sinp + 1) threshold its acc
  sinp = (setPrecision pr $ SinCos.sine_poly deg NormZero $ Cheb.fromList [(1,mpBall 7)])   
  x = (setPrecision pr $ Cheb.fromList [(1, mpBall 1)])   
  
fracSinXMaxFn :: Accuracy -> MPBall
fracSinXMaxFn acc = 
  cauchyReal2ball mx acc 
  where
  Interval _ mx = 
    rangeOnIntervalUnaryFnA ((UnaryFnMPBall (Interval (-1.0) 1.0) $ \x -> (catchingExceptions $ x)/(catchingExceptions $ 10*(sin(7*x))^2 + 1)), Interval (-1.0) 1.0)  
  
hatMax :: Precision -> Accuracy -> MPBall
hatMax pr rangeBits = m
  where
  Interval _ m = range rangeBits hat $ Interval (mpBall $ -1) (mpBall 1)
  hat = linearPolygon [(-1.0, rational2BallP pr (1/3)), (-1/3, mpBall 1), (1.0, rational2BallP pr (-1/3))] 0.0   

hatIntegral :: Precision -> MPBall
hatIntegral pr = integral hat (mpBall $ -1) (mpBall 1)
  where
  hat = linearPolygon [(-1.0, rational2BallP pr (1/3)), (-1/3, mpBall 1), (1.0, rational2BallP pr (-1/3))] 0.0
  
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
                     
sinesineIntegral :: Cheb.Degree -> Cheb.Degree -> Accuracy -> Precision -> MPBall
sinesineIntegral degI degO rangeBits pr = 
  integrateUnaryFnA (sinesine, (mpBall $ -1), (mpBall 1))
  where
  sinesine = (SinCos.sine_poly degO NormZero $ Cheb.fromList [(1,mpBall 10)] 
                     + (SinCos.sine_poly degI NormZero $ setPrecision pr (Cheb.fromList [(1, mpBall 1)]*Cheb.fromList [(1, mpBall 20)])))  

sinesineXIntegral :: Cheb.Degree -> Cheb.Degree -> Precision -> MPBall
sinesineXIntegral degI degO pr = 
  integral sinesine (mpBall $ -1) (mpBall 1)
  where
  sinesine = fromPoly $ (Cheb.fromList [(1,mpBall 1)]) + 
                        (SinCos.sine_poly degO NormZero $ Cheb.fromList [(1,mpBall 10)] 
                        + (SinCos.sine_poly degI NormZero $ setPrecision pr (Cheb.fromList [(1, mpBall 1)]*Cheb.fromList [(1, mpBall 20)])))

sinesineCosIntegral :: Cheb.Degree -> Cheb.Degree -> Accuracy -> Precision -> MPBall
sinesineCosIntegral degI degO acc pr = 
  integral sinesine (mpBall $ -1) (mpBall 1)
  where
  sinesine = fromPoly $ (SinCos.sine_poly degI NormZero $ Cheb.fromList [(1,mpBall 10)]) + 
                        (SinCos.sine_poly degO NormZero $ Cheb.fromList [(1,mpBall 10)] 
                        + (SinCos.sine_poly degI NormZero $ setPrecision pr (Cheb.fromList [(1, mpBall 1)]*Cheb.fromList [(1, mpBall 20)])))

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
                      