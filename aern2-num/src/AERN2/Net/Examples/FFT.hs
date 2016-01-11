{-# LANGUAGE Arrows, TypeOperators, TemplateHaskell #-}
module AERN2.Net.Examples.FFT 
(
    fftTestDirectCR,
    fftTestDirectMPB,
    fftTestCached,
    dftCooleyTukey
)
where

import AERN2.Num

--import AERN2.Net.Spec.Arrow
import Control.Arrow

import AERN2.Net.Strategy.Direct ()
import AERN2.Net.Strategy.QACached

{-
import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id
-}

fftTestDirectCR :: Integer -> Accuracy -> [(MPBall, MPBall)]
fftTestDirectCR nN ac =
    map (\ c -> complexCR2balls c ac) $ fftWithInput ()
    where
    fftWithInput =
        proc () ->
            do
            x <- complexListNamedA "input" -< input
            dftCooleyTukey nN -< x
    input = map rational [1..nN] 

fftTestDirectMPB :: Integer -> Precision -> [(Complex MPBall)]
fftTestDirectMPB nN p =
    fftWithInput ()
    where
    fftWithInput =
        proc () ->
            do
            x <- complexListNamedA "input" -< input
            dftCooleyTukey nN -< x
    input = map (integer2BallP p) [1..nN] 

fftTestCached :: Integer -> Accuracy -> (QANetLog, [(Complex MPBall)])
fftTestCached nN ac =
    executeQACachedA $ proc () ->
        do
        rs <- (fftWithInput :: QACachedA () [QACached_Complex]) -< ()
        mapA getComplexAnswer -< rs
    where
    getComplexAnswer = proc (r :+ i) ->
        do
        rA <- getAnswerCRA -< (r, ac)
        iA <- getAnswerCRA -< (i, ac)
        returnA -< (rA :+ iA)
    fftWithInput =
        proc () ->
            do
            x <- complexListNamedA "input" -< input
            dftCooleyTukey nN -< x
    input = map rational [1..nN] 

{-|
    Discrete Fourier Transform using the Cooley and Tukey Radix-2 algorithm.
    
    Preconditions:
    
    * @N > 0@ is a power of 2.
    
    Arrow preconditions:
    
    * The input list has exactly @N@ elements.
-}
dftCooleyTukey :: 
    (RealExprA to r)
    =>
    Integer {-^ @N@ -} -> 
    [Complex r] `to` [Complex r]
dftCooleyTukey nN = ditfft2 nN 1

{-|
    Radix-2 Cooley-Tukey as described in:
    
    https://en.wikipedia.org/wiki/Cooley-Tukey_FFT_algorithm#The_radix-2_DIT_case
    
    Preconditions:
    
    * @N > 0@ is a power of 2.
    * @s > 0@
    
    Arrow preconditions:
    
    * The input list has at least @s*(N-1) + 1@ elements.
-}
ditfft2 :: 
    (RealExprA to r)
    =>
    Integer {-^ @N@ -} -> 
    Integer {-^ @s@ -} ->
    [Complex r] `to` [Complex r]
ditfft2 nN s
    | nN == 1 =
        proc (x0:_) -> 
            returnA -< [x0]
    | otherwise =
        proc x ->
            do
            vTX0 <- ditfft2 nNhalf (2 * s) -< x 
            vTXNhalf <- ditfft2 nNhalf (2 * s) -< drop (int s) x
            vTXNhalfTwiddled <- mapAwithPos twiddle -< vTXNhalf
            vX0 <- zipWithA addA -< (vTX0, vTXNhalfTwiddled)
            vXNhalf <- zipWithA subA -< (vTX0, vTXNhalfTwiddled)
            returnA -< vX0 ++ vXNhalf
    where
    nNhalf = round (nN / 2)
    twiddle k = 
        proc x_k_plus_NHalf ->
            do
--            c <- $(exprA[|let [i] = vars in exp(-2*pi*i*k/nN)|]) <<< complex_iA -< ()
--            let _ = [c,x_k_plus_NHalf]
--            mulA -< (x_k_plus_NHalf, c)
            mulA -< (x_k_plus_NHalf, exp(-2*pi*i*k/nN))
        where
        i = complex_i :: Complex CauchyReal                