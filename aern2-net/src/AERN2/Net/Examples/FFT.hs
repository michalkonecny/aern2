{-# LANGUAGE Arrows, TypeOperators #-}
module AERN2.Net.Examples.FFT 
(
    dftCooleyTukey
)
where

import AERN2.Num
import Data.String (fromString)

import AERN2.Net.Spec.Arrow
import Control.Arrow

--import AERN2.Net.Execution.Direct
--
--import Debug.Trace (trace)
--
--shouldTrace :: Bool
--shouldTrace = False
----shouldTrace = True
--
--maybeTrace :: String -> a -> a
--maybeTrace 
--    | shouldTrace = trace
--    | otherwise = const id


{-|
    Discrete Furier Transform using the Cooley and Tukey Radix-2 algorithm.
    
    Preconditions:
    
    * @N > 0@ is a power of 2.
    
    Arrow preconditions:
    
    * The input list has exactly @N@ elements.
-}
dftCooleyTukey :: 
    (ArrowComplex to r)
    =>
    Integer {-^ @N@ -} -> 
    [r] `to` [r]
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
    (ArrowComplex to r)
    =>
    Integer {-^ @N@ -} -> 
    Integer {-^ @s@ -} ->
    [r] `to` [r]
ditfft2 nN s
    | nN == 1 =
        proc (x0:_) -> 
            returnA -< [x0]
    | otherwise =
        proc x ->
            do
            vTX0 <- ditfft2 nNhalf (2 * s) -< x 
            vTXNhalf <- ditfft2 nNhalf (2 * s) -< (drop (toInt s) x)
            vTXNhalfTwiddled <- mapA twiddle -< vTXNhalf
            vX0 <- zipWithA (const addA) -< (vTX0, vTXNhalfTwiddled)
            vXNhalf <- zipWithA (const subA) -< (vTX0, vTXNhalfTwiddled)
            returnA -< vX0 ++ vXNhalf
    where
    nNhalf = round (nN / 2)
    twiddle k = 
        proc x_k_plus_NHalf ->
            mulComplexA "exp(-2*pi*i*k/nN)*" (exp(-2*pi*i*k/nN)) -< x_k_plus_NHalf
    i = complexI
    
                