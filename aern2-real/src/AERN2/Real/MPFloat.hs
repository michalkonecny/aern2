{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, TypeSynonymInstances #-}

module AERN2.Real.MPFloat 
    (MPFloat, Precision(..), 
     toRational, toDoubleUp, toDoubleDown, getDoubleBound,
     zero, rationalUp, rationalDown, 
     neg, abs, absDB, addUp, addDown, subUp, subDown, subDB, mulUp, mulDown, divUp, divDown, 
     piUp, piDown,
     cosUp, cosDown, sinUp, sinDown)
where

import Prelude hiding (fromInteger, toRational, abs)
import qualified Prelude as P

import qualified Data.Approximate.MPFRLowLevel as MPLow

import AERN2.Real.DoubleBound

type MPFloat = MPLow.Rounded
newtype Precision = Precision Integer
    deriving (P.Eq, P.Ord, P.Show, P.Enum, P.Num, P.Real, P.Integral)

{- conversions -}

toRational :: MPFloat -> Rational
toRational = MPLow.toRationalA

toDoubleUp :: MPFloat -> Double
toDoubleUp = MPLow.toDoubleA MPLow.Up
    
toDoubleDown :: MPFloat -> Double
toDoubleDown = MPLow.toDoubleA MPLow.Down
    
getDoubleBound :: MPFloat -> DoubleBound
getDoubleBound = DoubleBound . toDoubleUp 
    
{- constants -}

zero :: MPFloat
zero = MPLow.zero
    
rationalUp :: Precision -> Rational -> MPFloat
rationalUp (Precision p) r =
    MPLow.fromRationalA MPLow.Up (P.fromInteger p) r
    
rationalDown :: Precision -> Rational -> MPFloat
rationalDown (Precision p) r =
    MPLow.fromRationalA MPLow.Down (P.fromInteger p) r
    
piUp :: Precision -> MPFloat
piUp (Precision p) =
    MPLow.pi MPLow.Up (P.fromInteger p)
    
piDown :: Precision -> MPFloat
piDown (Precision p) =
    MPLow.pi MPLow.Down (P.fromInteger p)
    

neg :: MPFloat -> MPFloat
neg = unaryUp MPLow.neg

abs :: MPLow.Rounded -> MPFloat
abs x 
    | x < MPLow.zero = neg x
    | otherwise = x

absDB :: MPFloat -> DoubleBound
absDB = getDoubleBound . abs

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

subDB :: MPFloat -> MPFloat -> DoubleBound
a `subDB` b = getDoubleBound $ a `subUp` b 


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
    