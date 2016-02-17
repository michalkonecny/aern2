{-# LANGUAGE TypeSynonymInstances #-}
module AERN2.Num.MPFloat 
    (
     -- * Precision operations
     module AERN2.Num.Precision,
     -- * MPFR numbers and their basic operations
     MPFloat, setPrecisionUp,
     toRational, toDoubleUp, toDoubleDown,
     -- * MPFR number constructors
     zero, one, two, fromIntegerUp, fromIntegerDown, fromRationalUp, fromRationalDown,
     piUp, piDown,
     -- * MPFR number basic arithmetic
     neg, abs, addUp, addDown, subUp, subDown, 
     distUp, distDown, avgUp, avgDown, 
     mulUp, mulDown, divUp, divDown, recipUp, recipDown,
     -- * MPFR number selected operations
     cosUp, cosDown, sinUp, sinDown, sqrtUp, sqrtDown, expUp, expDown)
where

import AERN2.Num.Operations hiding (abs,neg,toRational)
import qualified Prelude as P

import AERN2.Num.IntegerRational ()
import AERN2.Num.Precision

import qualified Data.Approximate.MPFRLowLevel as MPLow


type MPFloat = MPLow.Rounded

instance HasPrecision MPFloat where
    getPrecision x = prec (P.toInteger $ MPLow.getPrec x)

setPrecisionUp :: Precision -> MPFloat -> MPFloat
setPrecisionUp p = MPLow.set MPLow.Up (p2mpfrPrec p)

p2mpfrPrec :: Precision -> MPLow.Precision
p2mpfrPrec = P.fromInteger . prec2integer

{- conversions -}

toRational :: MPFloat -> Rational
toRational = MPLow.toRationalA

toDoubleUp :: MPFloat -> Double
toDoubleUp = MPLow.toDoubleA MPLow.Up
    
toDoubleDown :: MPFloat -> Double
toDoubleDown = MPLow.toDoubleA MPLow.Down
    
instance HasEq MPFloat MPFloat where

instance HasOrder MPFloat MPFloat where

{- constants -}

zero :: MPFloat
zero = MPLow.zero
    
one :: MPFloat
one = convert 1
    
two :: MPFloat
two = convert 2
    
fromIntegerUp :: Precision -> Integer -> MPFloat
fromIntegerUp p i = fromRationalUp p (P.fromInteger i)
    
fromIntegerDown :: Precision -> Integer -> MPFloat
fromIntegerDown p i = fromRationalDown p (P.fromInteger i)
    

instance ConvertibleA (->) Integer MPFloat where
    convertA n =
        findExact $ map upDown $ drop (int 4) standardPrecisions
        where
        upDown p = (fromIntegerDown p n, fromIntegerUp p n)
        findExact [] = 
            error $ "integer too high to represent exactly: " ++ show n
        findExact ((nDown, nUp) : rest)
            | nDown == nUp = nUp
            | otherwise = findExact rest

fromRationalUp :: Precision -> Rational -> MPFloat
fromRationalUp p x =
    MPLow.fromRationalA MPLow.Up (p2mpfrPrec p) x
    
fromRationalDown :: Precision -> Rational -> MPFloat
fromRationalDown p x =
    MPLow.fromRationalA MPLow.Down (p2mpfrPrec p) x

-- | Computes an upper bound to the distance @|x - y|@ of @x@ and @y@.
distUp :: PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat
distUp pp x y = if x >= y then subUp pp x y else subUp pp y x

-- | Computes a lower bound to the distance @|x - y|@ of @x@ and @y@.
distDown :: PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat
distDown pp x y = if x >= y then subDown pp x y else subDown pp y x
    
avgUp :: PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat
avgUp pp x y = divUp pp (addUp pp x y) two

avgDown :: PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat
avgDown pp x y = divDown pp two (addDown pp x y)

{- common functions -}

neg :: MPFloat -> MPFloat
neg = unaryUp MPLow.neg

abs :: MPLow.Rounded -> MPFloat
abs x 
    | x < MPLow.zero = neg x
    | otherwise = x

addUp, addDown :: PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat
addUp = binaryUp True MPLow.add
addDown = binaryDown True MPLow.add

subUp, subDown :: PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat
subUp = binaryUp True MPLow.sub
subDown = binaryDown True MPLow.sub

mulUp, mulDown :: PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat
mulUp = binaryUp True MPLow.mul
mulDown = binaryDown True MPLow.mul

divUp,divDown :: PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat
divUp = binaryUp False MPLow.div
divDown = binaryDown False MPLow.div

recipUp :: PrecisionPolicy -> MPFloat -> MPFloat
recipUp pp x = divUp pp one x

recipDown :: PrecisionPolicy -> MPFloat -> MPFloat
recipDown pp x = divDown pp one x

{- special constants and functions -}

piUp :: Precision -> MPFloat
piUp p =
    MPLow.pi MPLow.Up (p2mpfrPrec p)
    
piDown :: Precision -> MPFloat
piDown p =
    MPLow.pi MPLow.Down (p2mpfrPrec p)
    
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
    Bool ->
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat -> MPFloat) ->
    (PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat)
binaryUp = binaryApprox True

binaryDown :: 
    Bool ->
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat -> MPFloat) ->
    (PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat)
binaryDown = binaryApprox False
    
binaryApprox :: 
    Bool -> Bool ->
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat -> MPFloat) ->
    (PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat)
binaryApprox isUp canBeExact opRP pp x y = 
    case precPolicy_mode pp of
        PrecisionPolicyMode_UseCurrent ->
            withPrec pCurr
        PrecisionPolicyMode_KeepExactDyadic | canBeExact ->
            getExact pMax
        _ ->
            withPrec pMax
    where
    pMax = pCurr `P.max` (getPrecision x) `P.max` (getPrecision y)
    pCurr = precPolicy_precision pp
    withPrec p
        | isUp = opRP MPLow.Up (p2mpfrPrec p) x y
        | otherwise = opRP MPLow.Down (p2mpfrPrec p) x y
    getExact p
        | rUp P.== rDown = rUp
        | otherwise = getExact (precisionTimes2 p)
        where
        rUp = opRP MPLow.Up (p2mpfrPrec p) x y
        rDown = opRP MPLow.Down (p2mpfrPrec p) x y
        
        
    