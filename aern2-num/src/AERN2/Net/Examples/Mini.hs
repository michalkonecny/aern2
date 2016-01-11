{-# LANGUAGE FlexibleContexts #-}
module AERN2.Net.Examples.Mini 
    (module AERN2.Net.Examples.Mini,
     module AERN2.Num)
where

import AERN2.Num

import Control.Arrow

import AERN2.Net.Strategy.QACached

{--- a very simple real expression ---}

example0 :: CauchyReal -> CauchyReal
example0 x = sqrt(x) + x

example0exprA ::
    (RealExprA to r) => r `to` r
example0exprA =
    $(exprA[|let [x] = vars in sqrt(x) + x|]) 

example0procA ::
    (RealExprA to r) => r `to` r
example0procA = 
    proc x ->
        do
        temp1 <- sqrtA -< x
        addA -< (temp1, x)

example0directA ::
    (RealExprA to r) => r `to` r
example0directA =
    addA <<< first sqrtA <<< arr (\x -> (x,x))

example0directA_TestCached :: Accuracy -> IO ()
example0directA_TestCached ac =
    printQANetLogThenResult $
        executeQACachedA $
            proc () ->
                do
                x <- convertA -< 1/3
                r <- example0directA -< x :: QACached_CauchyReal
                getAnswerCRA -< (r,ac)

{--- a simple complex expression, a part of FFT ---}

twiddle :: (Integer, Integer) -> Complex CauchyReal
twiddle(k,n) =  exp(-2*k*complex_i*pi/n)

twiddleA :: (RealPredA to r) => (Integer, Integer) -> () `to` (Complex r)
twiddleA(k,n) = $(exprA[| let [i]=vars in exp(-2*k*i*pi/n)|]) <<< complex_iA

{--- the logistic map ---}

logisticNoA :: Rational -> Integer -> CauchyReal -> CauchyReal
logisticNoA c n x0 =
    foldl1 (.) (replicate (int n) step) x0
    where
    step x = c * x * (1 - x)

logisticA :: (RealExprA to r) => Rational -> Integer -> r `to` r
logisticA c n =
    (foldl1 (<<<) (replicate (int n) step)) 
    where
    step = $(exprA[|let [x]=vars in  c * x * (1 - x)|])
    
logisticQACached :: Rational -> Integer -> CauchyReal -> CauchyReal
logisticQACached c n x0 =
    newCRA ([], Nothing, ac2ball)
    where
    ac2ball ac =
        snd $ logisticQACachedMPBall c n x0 ac
            
logisticQACachedMPBall :: Rational -> Integer -> CauchyReal -> (Accuracy -> (QANetLog, MPBall))
logisticQACachedMPBall c n x0 ac =
    executeQACachedA auxA
    where
    auxA =
        proc () ->
            do
            r <- logisticA c n <<< convertA -< x0
            getAnswerCRA -< (r :: QACached_CauchyReal,ac)
    
logisticQACachedMPBallPrintLog :: Rational -> Integer -> CauchyReal -> Accuracy -> IO ()
logisticQACachedMPBallPrintLog c n x0 ac =
    printQANetLogThenResult (logisticQACachedMPBall c n x0 ac)
            
logisticMPBIterate :: Rational -> Integer -> CauchyReal -> CauchyReal
logisticMPBIterate c n x0 =
    newCRA ([], Nothing, ac2ball)
    where
    ac2ball ac =
        snd $ last $ iterateUntilAccurate ac auxP  
    auxP p = 
        logisticA c n x0p
        where
        x0p = cauchyReal2ball x0 ac
        ac = bits $ prec2integer p

{- Example: naive exponential function on [-1,1] -} 
                                    
expLim :: CauchyReal -> CauchyReal
expLim x = lim (\n -> (sum [(x^k)/(k!) | k <- [0..n]]) +- errorBound (x,n))
           where
           errorBound (y,n) = ((abs y)^(n + 1))*3/((n + 1)!)  --TODO error bound only valid on [-1,1]
                                                             -- more general error bound: 3^ceil(x)

{- Newton iteration -}

{- TODO
newtonTest1 =
    newton f f' (Interval (cauchyReal 0) (cauchyReal 0))
    where
    f x = x*x - 2
    f' x = 2*x 
-}
    
newton :: 
    (CanSelectFromIntervalA (->) r, CanDivSameTypeA (->) (Interval r),
     CanLimitA (->) (Interval r), CanNegSameTypeA (->) (Interval r)) 
     =>
    (Interval r -> Interval r) -> (Interval r -> Interval r) -> 
    Interval r -> LimitType (Interval r)
newton f f' iX_0 = 
    iterateLim iX_0 $ \ iX -> let x = singleton (pickAnyA iX) in - (f x)/(f' iX)
    
{-

newtonA :: 
    (CanSelectFromIntervalA (->) r, CanDivSameTypeA (->) (Interval r),
     CanLimitA (->) (Interval r), CanNegSameTypeA (->) (Interval r)) 
     =>
    (Interval r `to` Interval r) -> 
    (Interval r `to` Interval r) -> 
    Interval r `to` LimitType (Interval r)
newtonA f f' iX_0 = 
    iterateLim iX_0 $ \ iX -> let x = singleton (pickAnyA iX) in - (f x)/(f' iX)

-}
    