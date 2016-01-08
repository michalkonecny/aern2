module AERN2.Net.Examples.Mini 
    (module AERN2.Net.Examples.Mini,
     module AERN2.Num)
where

import AERN2.Num

import Control.Arrow

import AERN2.Net.Execution.QACached

{--- a simple complex expression, a part of FFT ---}

twiddle :: (Integer, Integer) -> Complex CauchyReal
twiddle(k,n) =  exp(-2*k*complex_i*pi/n)

twiddleA :: (RealPredA to r) => (Integer, Integer) -> () `to` (Complex r)
twiddleA(k,n) = $(exprA[| let [i]=vars in exp(-2*k*i*pi/n)|]) <<< complex_iA

{--- the logistic map ---}

logistic :: Rational -> Integer -> CauchyReal -> CauchyReal
logistic c n =
    foldl1 (.) (replicate (int n) step)
    where
    step x = c * x * (1 - x)

logisticA :: (RealExprA to r) => Rational -> Integer -> r `to` r
logisticA c n =
    foldl1 (<<<) (replicate (int n) step) 
    where
    step = $(exprA[|let [x]=vars in  c * x * (1 - x)|])
    
logisticCached :: Rational -> Integer -> CauchyReal -> CauchyReal
logisticCached c n x0 =
    newCRA ([], Nothing, ac2ball)
    where
    ac2ball ac =
        snd $ executeQACachedA (auxA ac)
    auxA ac =
        proc () ->
            do
            r <- logisticA c n <<< convertA -< x0
            getAnswerCRA -< (r :: QACached_CauchyReal,ac)
            
logisticIterate :: Rational -> Integer -> CauchyReal -> CauchyReal
logisticIterate c n x0 =
    newCRA ([], Nothing, ac2ball)
    where
    ac2ball ac =
        snd $ last $ iterateUntilAccurate 100 ac auxP  
    auxP p = 
        logisticA c n x0p
        where
        x0p = cauchyReal2ball x0 ac
        ac = bits $ prec2integer p
    
iterateUntilAccurate :: Integer -> Accuracy -> (Precision -> MPBall) -> [(Precision, MPBall)]
iterateUntilAccurate iterLimit accuracy fn =
    stopWhenAccurate $ 
        take (int iterLimit) $ 
            zip ps (map fn ps)
    where
    ps = standardPrecisions
    stopWhenAccurate [] = []
    stopWhenAccurate ((p, result) : rest)
        | getAccuracy result >= accuracy = [(p, result)]
        | otherwise = (p, result) : (stopWhenAccurate rest)
