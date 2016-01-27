{-# LANGUAGE FlexibleContexts #-}
module AERN2.Net.Examples.Mini 
    (module AERN2.Net.Examples.Mini,
     module AERN2.Num)
where

import AERN2.Num

import Control.Arrow
--import Control.Exception

import AERN2.Net.Strategy.QACached

{--- a very simple real expression ---}

example0 :: CauchyReal -> CauchyReal
example0 x = sqrt(x) + x

example0directA ::
    (ArrowReal to r) => r `to` r
example0directA =
    addA <<< first sqrtA <<< arr (\x -> (x,x))

example0procA ::
    (ArrowReal to r) => r `to` r
example0procA = 
    proc x ->
        do
        temp1 <- sqrtA -< x
        addA -< (temp1, x)

--example0procA_BAD ::
--    (ArrowReal to r) => r `to` r
--example0procA_BAD = 
--    proc x ->
--        do
--        temp1 <- sqrtA x -< ()
--        addA -< (temp1, x)

example0exprA ::
    (ArrowReal to r) => r `to` r
example0exprA =
    $(exprA[|let [x] = vars in sqrt(x) + x|]) 

example0TestCached :: Accuracy -> (QANetLog, MPBall)
example0TestCached ac =
    executeQACachedA $
        proc () ->
            do
            x <- convertA -< 1/3
            r <- example0directA -< x :: QACached_CauchyReal
            getAnswerCRA -< (r,ac)

{--- a simple complex expression, a part of FFT ---}

twiddle :: (Integer, Integer) -> Complex CauchyReal
twiddle(k,n) =  exp(-2*k*complex_i*pi/n)

twiddleA :: (ArrowReal to r) => (Integer, Integer) -> () `to` (Complex r)
twiddleA(k,n) = $(exprA[| let [i]=vars in exp(-2*k*i*pi/n)|]) <<< complex_iA

{--- the logistic map ---}

logisticNoA :: Rational -> Integer -> CauchyReal -> CauchyReal
logisticNoA c n x0 =
    foldl1 (.) (replicate (int n) step) x0
    where
    step x = c * x * (1 - x)

logisticA :: (ArrowReal to r) => Rational -> Integer -> r `to` r
logisticA c n =
    (foldl1 (<<<) (replicate (int n) step)) 
    where
    step = $(exprA[|let [x]=vars in  c * x * (1 - x)|])
    
logisticWithHookA :: (ArrowReal to r) => (r `to` Maybe r) -> Rational -> Integer -> r `to` Maybe r
logisticWithHookA hook c n =
    (foldl1 (<<<) (replicate (int n) step)) <<< arr Just 
    where
    step = 
        proc maybeX ->
            case maybeX of
                Just x -> hook <<< $(exprA[|let [x]=vars in  c * x * (1 - x)|]) -< x
                Nothing -> returnA -< Nothing
    
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
            
logisticMPB :: Rational -> Integer -> MPBall -> MPBall
logisticMPB c n x0 =
    runWithPrecisionPolicy (logisticA c n) pp x0 
    where
    pp = PrecisionPolicy (getPrecision x0) PrecisionPolicyMode_UseCurrent

logisticMPBIterate :: Rational -> Integer -> CauchyReal -> CauchyReal
logisticMPBIterate c n x0 =
    newCRA ([], Nothing, ac2ball)
    where
    ac2ball ac =
        case last $ iterateUntilAccurate ac (auxP ac) of
            (_, Just ball) -> ball
            _ -> error "logisticMPBIterate: failed"  
    auxP ac p = 
        runWithPrecisionPolicy (logisticWithHookA (arr check) c n) pp x0p
        where
        pp = PrecisionPolicy p PrecisionPolicyMode_UseCurrent
        x0p = cauchyReal2ball x0 pA
        pA = bits $ prec2integer p
        check ball 
            | getAccuracy ball < ac = Nothing -- throw LossOfPrecision 
            | otherwise = Just ball 

--logisticMPBIteratePrintProgress :: Rational -> Integer -> CauchyReal -> Accuracy -> IO ()
--logisticMPBIteratePrintProgress c n x0 ac =
--    mapM_ printInfo (iterInfos)
--    where
--    iterInfos = iterateUntilAccurate ac auxP
--    auxP p = 
--        logisticWithHookA check c n x0p
--        where
--        x0p = cauchyReal2ball x0 pA
--        pA = bits $ prec2integer p
--        check ball 
--            | getAccuracy ball < ac = Nothing -- throw LossOfPrecision 
--            | otherwise = Just ball 
--    printInfo (p, maybeBall) =
--        do
--        putStrLn $ show p ++ ": result = " ++ show maybeBall

{- Example: naive exponential function on [-1,1] -} 
                                    
myExp :: CauchyReal -> CauchyReal
myExp x = lim (\n -> (sum [(x^k)/(k!) | k <- [0..n]]) +- errorBound (x,n))
           where
           errorBound (y,n) = ((abs y)^(n + 1))*3/((n + 1)!)  
            -- Error bound only valid on [-1,1].
            -- A more general error bound: 3^ceil(x)

myExpA :: 
    (ArrowReal to r, CanAbsSameTypeA to r,
     CanPlusMinusA to r r, PlusMinusTypeA to r r ~ Interval r, 
     CanLimitA to (Interval r)) 
    => 
    r `to` LimitTypeA to (Interval r)
myExpA = 
    limA $ \n -> 
        proc x -> do
            terms <- mapA termA -< [(x,k) | k <- [0..n]]
            s <- sumA -< terms
            absx <- absA -< x
            eb <- errorBoundA n -< absx
            plusMinusA -< (s,eb)
    where
    errorBoundA n = 
        $(exprA[| let [absx] = vars in (absx^(n + 1))*3/((n + 1)!)|])
        where
        _ = n :: Integer
    termA = proc (x,k) ->
        do
        temp1 <- powA -< (x,k)
        divA -< (temp1, (k!))

myExpTestDirect :: Rational -> Accuracy -> MPBall
myExpTestDirect x =
    cauchyReal2ball $ myExpA (cauchyReal x)

myExpTestCached :: Rational -> Accuracy -> (QANetLog, MPBall)
myExpTestCached xRat ac =
    executeQACachedA $
        proc () -> do
            x <- convertA -< xRat
            r <- myExpA -< x :: QACached_CauchyReal
            b <- getAnswerCRA -< (r,ac)
            returnA -< b

{- Newton iteration -}

newtonTest :: CauchyReal
newtonTest =
    newton f f' (Interval (cauchyReal 1) (cauchyReal 2))
    where
    f x = x*x - 2
    f' x = 2*x 

newtonTestADirect :: Accuracy -> MPBall
newtonTestADirect acc =
    cauchyReal2ball (newtonA f f' (Interval (cauchyReal 1) (cauchyReal 2))) acc
    where
    f = proc(x) -> 
        do
        sq <- mulA -< (x,x)
        diff <- subA -< (sq,2)
        returnA -< diff
    f' = proc(x) -> 
        do
        tx <- mulA -< (2,x)
        returnA -< tx

newtonTestACached :: Accuracy -> (QANetLog, MPBall)
newtonTestACached acc =
    executeQACachedA $ proc () -> do
        l <- convertA -< 1
        r <- convertA -< 2
        x <- newtonA f f' -< (Interval l r)
        b <- getAnswerCRA -< (x,acc)
        let _ = [l,r,x :: QACached_CauchyReal]
        returnA -< b
    where
    f = proc(x) -> 
        do
        sq <- mulA -< (x,x)
        diff <- subA -< (sq,2)
        returnA -< diff
    f' = proc(x) -> 
        do
        tx <- mulA -< (2,x)
        returnA -< tx
    
newton :: 
    (CanSelectFromIntervalA (->) r, CanDivSameTypeA (->) (Interval r),
     CanLimitA (->) (Interval r), CanSubSameTypeA (->) (Interval r)) 
     =>
    (r -> r) -> 
    (Interval r -> Interval r) -> 
    Interval r -> LimitType (Interval r)
newton f f' iX_0 = 
    iterateLim iX_0 $ \ iX -> let x = pickAnyA iX in (singleton x) - (singleton $ f x)/(f' iX)

newtonA ::
     (ArrowReal to r, CanSelectFromIntervalA to r, 
      CanDivSameTypeA to (Interval r),
      CanLimitA to (Interval r)) 
     =>
    (r `to` r) -> 
    (Interval r `to` Interval r) -> 
    Interval r `to` LimitTypeA to (Interval r)
newtonA f f' = 
    iterateLimA $
        proc iX ->
            do
            x <- pickAnyA -< iX
            fx <- f -< x
            f'iX <- f' -< iX
            temp1 <- divA -< (singleton fx,f'iX)
            subA -< (singleton x,temp1)

newtonIterateTest :: [Interval CauchyReal]
newtonIterateTest =
    newtonIterate f f' (Interval (cauchyReal 1) (cauchyReal 2))
    where
    f x = x*x - 2
    f' x = 2*x 

newtonIterate ::
    (CanSelectFromIntervalA (->) r, CanDivSameTypeA (->) (Interval r),
     CanLimitA (->) (Interval r), CanSubSameTypeA (->) (Interval r)) 
     =>
    (r -> r) -> 
    (Interval r -> Interval r) -> 
    Interval r -> 
    [(Interval r)]
newtonIterate f f' iX_0 =
    iterate (\ iX -> let x = pickAnyA iX in (singleton x) - (singleton $ f x)/(f' iX)) iX_0
    
{- TODO

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
    
