{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, TypeSynonymInstances #-}

module AERN2.Real.MPFloat 
    (MPFloat, Precision, prec, maximumPrecision, setPrecisionUp,
     toRational, toDoubleUp, toDoubleDown,
     zero, rationalUp, rationalDown, integerUp, integerDown,
     neg, abs, addUp, addDown, subUp, subDown, 
     mulUp, mulDown, divUp, divDown, recipUp, recipDown, 
     piUp, piDown,
     cosUp, cosDown, sinUp, sinDown)
where

import Prelude hiding (fromInteger, fromRational, toRational, abs)
import qualified Prelude as P

import qualified Data.Approximate.MPFRLowLevel as MPLow

import AERN2.Real.Operations (fromInteger, fromRational)

type MPFloat = MPLow.Rounded
newtype Precision = Precision Integer
    deriving (P.Eq, P.Ord, P.Show, P.Enum, P.Num, P.Real, P.Integral)

setPrecisionUp :: Precision -> MPFloat -> MPFloat
setPrecisionUp (Precision p) = MPLow.set MPLow.Up (P.fromInteger p)

prec :: Integer -> Precision
prec p 
    | p < 2 = error errmsg  
    | p > maximumPrecision = error errmsg
    | otherwise = Precision p
    where
    errmsg =
        "Precision must be between 2 and " ++ show maximumPrecision ++ " (given: p=" ++ show p ++ ")."

maximumPrecision :: Integer
maximumPrecision = 1000000

{- conversions -}

toRational :: MPFloat -> Rational
toRational = MPLow.toRationalA

toDoubleUp :: MPFloat -> Double
toDoubleUp = MPLow.toDoubleA MPLow.Up
    
toDoubleDown :: MPFloat -> Double
toDoubleDown = MPLow.toDoubleA MPLow.Down
    
{- constants -}

zero :: MPFloat
zero = MPLow.zero
    
one :: MPFloat
one = rationalUp (Precision 20) 1.0
    
integerUp :: Precision -> Integer -> MPFloat
integerUp p i = rationalUp p (P.fromInteger i)
    
integerDown :: Precision -> Integer -> MPFloat
integerDown p i = rationalDown p (P.fromInteger i)
    
rationalUp :: Precision -> Rational -> MPFloat
rationalUp (Precision p) x =
    MPLow.fromRationalA MPLow.Up (P.fromInteger p) x
    
rationalDown :: Precision -> Rational -> MPFloat
rationalDown (Precision p) x =
    MPLow.fromRationalA MPLow.Down (P.fromInteger p) x
    
{- common functions -}

neg :: MPFloat -> MPFloat
neg = unaryUp MPLow.neg

abs :: MPLow.Rounded -> MPFloat
abs x 
    | x < MPLow.zero = neg x
    | otherwise = x

addUp, addDown :: MPFloat -> MPFloat -> MPFloat
addUp = binaryUp MPLow.add
addDown = binaryDown MPLow.add

subUp, subDown :: MPFloat -> MPFloat -> MPFloat
subUp = binaryUp MPLow.sub
subDown = binaryDown MPLow.sub

mulUp, mulDown :: MPFloat -> MPFloat -> MPFloat
mulUp = binaryUp MPLow.mul
mulDown = binaryDown MPLow.mul

divUp,divDown :: MPFloat -> MPFloat -> MPFloat
divUp = binaryUp MPLow.div
divDown = binaryDown MPLow.div

recipUp :: MPFloat -> MPFloat
recipUp x = divUp one x

recipDown :: MPFloat -> MPFloat
recipDown x = divDown one x

{- special constants and functions -}

piUp :: Precision -> MPFloat
piUp (Precision p) =
    MPLow.pi MPLow.Up (P.fromInteger p)
    
piDown :: Precision -> MPFloat
piDown (Precision p) =
    MPLow.pi MPLow.Down (P.fromInteger p)
    
cosUp :: MPFloat -> MPFloat
cosUp = unaryUp MPLow.cos

cosDown :: MPFloat -> MPFloat
cosDown = unaryDown MPLow.cos

sinUp :: MPFloat -> MPFloat
sinUp = unaryUp MPLow.sin

sinDown :: MPFloat -> MPFloat
sinDown = unaryDown MPLow.sin

{- auxiliary functions to automatically determine result precision from operand precisions -}

unaryUp :: 
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat)
unaryUp opRP x = opRP MPLow.Up p x
    where
    p = MPLow.getPrec x

unaryDown :: 
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat)
unaryDown opRP x = opRP MPLow.Down p x
    where
    p = MPLow.getPrec x

binaryUp :: 
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat -> MPFloat)
binaryUp opRP x y = opRP MPLow.Up p x y
    where
    p = (MPLow.getPrec x) `max` (MPLow.getPrec y)

binaryDown :: 
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat -> MPFloat)
binaryDown opRP x y = opRP MPLow.Down p x y
    where
    p = (MPLow.getPrec x) `max` (MPLow.getPrec y)
    