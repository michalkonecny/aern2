{-# LANGUAGE Arrows, TypeOperators #-}
module AERN2.Net.Examples.FFT where

import AERN2.Real
import Data.String (fromString)

import AERN2.Net.Spec.Arrow
import Control.Arrow

import AERN2.Net.Execution.Direct

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id


{-|
    Radix-2 Cooley-Tukey as described in:
    
    https://en.wikipedia.org/wiki/Cooley-Tukey_FFT_algorithm#The_radix-2_DIT_case
-}
ditfft2 :: 
    (ArrowReal to r)
    =>
    Integer -> Integer ->
    [r] `to` [r]
ditfft2 nN s
    | nN == 1 =
        proc (x0:_) -> 
            returnA -< [x0]
    | otherwise =
        proc x ->
            do
            vX0 <- ditfft2 nNhalf (2 * s) -< x 
            vXNhalf <- ditfft2 nNhalf (2 * s) -< (drop (toInt s) x)
            vXNhalfTwiddled <- mapA twiddle -< zip [0..] vXNhalf
            
            returnA -< x -- TODO
    where
    nNhalf = round (nN / 2)
    twiddle = 
        undefined -- TODO
--        proc (k, x_k_plus_NHalf) ->
--            mulComplexA (exp(-2*pi*i*k/nN)) -< x_k_plus_NHalf
    i = 
--        complexI -- TODO
        (integer 0) :+ (integer 1)
    
                