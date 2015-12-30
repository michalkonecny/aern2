{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, TypeSynonymInstances #-}

module AERN2.Num.MPFloat 
    (MPFloat, Precision, prec, prec2integer, maximumPrecision, standardPrecisions, 
     getPrecision, setPrecisionUp,
     toRational, toDoubleUp, toDoubleDown,
     zero, one, two, integer, integerUp, integerDown, rationalUp, rationalDown,
     neg, abs, addUp, addDown, subUp, subDown, 
     distUp, distDown, avgUp, avgDown, 
     mulUp, mulDown, divUp, divDown, recipUp, recipDown,
     piUp, piDown,
     cosUp, cosDown, sinUp, sinDown, sqrtUp, sqrtDown, expUp, expDown)
where

import AERN2.Num.Operations hiding (abs,neg,toRational)
import qualified Prelude as P

import AERN2.Num.IntegerRational ()

import qualified Data.Approximate.MPFRLowLevel as MPLow


type MPFloat = MPLow.Rounded
newtype Precision = Precision Integer
    deriving (P.Eq, P.Ord, P.Show, P.Enum, P.Num, P.Real, P.Integral)

prec2integer :: Precision -> Integer
prec2integer (Precision p) = p

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

getPrecision :: MPFloat -> Precision
getPrecision x = Precision (P.toInteger $ MPLow.getPrec x)

{- conversions -}

toRational :: MPFloat -> Rational
toRational = MPLow.toRationalA

toDoubleUp :: MPFloat -> Double
toDoubleUp = MPLow.toDoubleA MPLow.Up
    
toDoubleDown :: MPFloat -> Double
toDoubleDown = MPLow.toDoubleA MPLow.Down
    
instance HasEq MPFloat MPFloat where
    equalTo = (P.==)

instance HasOrder MPFloat MPFloat where
    lessThan = (P.<)
    leq = (P.<=)

{- constants -}

zero :: MPFloat
zero = MPLow.zero
    
one :: MPFloat
one = integer 1
    
two :: MPFloat
two = integer 2
    
integerUp :: Precision -> Integer -> MPFloat
integerUp p i = rationalUp p (P.fromInteger i)
    
integerDown :: Precision -> Integer -> MPFloat
integerDown p i = rationalDown p (P.fromInteger i)
    
    
instance HasIntegers MPFloat where
    integer n =
        findExact $ map upDown $ drop (toInt 4) standardPrecisions
        where
        upDown p = (integerDown p n, integerUp p n)
        findExact [] = 
            error $ "integer too high to represent exactly: " ++ show n
        findExact ((nDown, nUp) : rest)
            | nDown == nUp = nUp
            | otherwise = findExact rest

standardPrecisions :: [Precision]
standardPrecisions =
    map Precision $ aux 8 13
    where
    aux j j' 
        | j <= maximumPrecision = j : (aux j' (j+j'))
        | otherwise = []
    
rationalUp :: Precision -> Rational -> MPFloat
rationalUp (Precision p) x =
    MPLow.fromRationalA MPLow.Up (P.fromInteger p) x
    
rationalDown :: Precision -> Rational -> MPFloat
rationalDown (Precision p) x =
    MPLow.fromRationalA MPLow.Down (P.fromInteger p) x

-- | Computes an upper bound to the distance @|x - y|@ of @x@ and @y@.
distUp :: MPFloat -> MPFloat -> MPFloat
distUp x y = if x >= y then subUp x y else subUp y x

-- | Computes a lower bound to the distance @|x - y|@ of @x@ and @y@.
distDown :: MPFloat -> MPFloat -> MPFloat
distDown x y = if x >= y then subDown x y else subDown y x
    
avgUp :: MPFloat -> MPFloat -> MPFloat
avgUp x y = (x `addUp` y) `divUp` two

avgDown :: MPFloat -> MPFloat -> MPFloat
avgDown x y = (x `addDown` y) `divDown` two

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

sqrtUp :: MPFloat -> MPFloat
sqrtUp = unaryUp MPLow.sqrt

sqrtDown :: MPFloat -> MPFloat
sqrtDown = unaryDown MPLow.sqrt

expUp :: MPFloat -> MPFloat
expUp = unaryUp MPLow.exp

expDown :: MPFloat -> MPFloat
expDown = unaryDown MPLow.exp

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
    p = (MPLow.getPrec x) `P.max` (MPLow.getPrec y)

binaryDown :: 
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat -> MPFloat)
binaryDown opRP x y = opRP MPLow.Down p x y
    where
    p = (MPLow.getPrec x) `P.max` (MPLow.getPrec y)
    