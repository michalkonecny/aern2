{-|
    Module      :  AERN2.MP.Float.Operators
    Description :  Infix operators for up/down-rounded floating-point numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Infix operators for up/down-rounded floating-point numbers
-}

module AERN2.MP.Float.Operators 
(
    -- upwards and downwards rounded operations
    (+^), (+.)
    , (-^), (-.)
    , (*^), (*.)
    , (/^), (/.)
    -- upwards and downwards rounded conversions
    , fromIntegerUp, fromIntegerDown
    , fromRationalUp, fromRationalDown
    -- upwards and downwards rounded selected elementary functions
    , cosUp, cosDown, sinUp, sinDown
    , sqrtUp, sqrtDown, expUp, expDown, logUp, logDown
)
where

import MixedTypesNumPrelude

import AERN2.MP.Precision
import AERN2.MP.Float.Auxi

import AERN2.MP.Float.Type
import AERN2.MP.Float.Arithmetic
import AERN2.MP.Float.Conversions

infixl 6  +^, -^, +., -.
infixl 7  *^, *., /^, /.

(+^) :: MPFloat -> MPFloat -> MPFloat
(+^) = up2 addCEDU
(-^) :: MPFloat -> MPFloat -> MPFloat
(-^) = up2 subCEDU
(*^) :: MPFloat -> MPFloat -> MPFloat
(*^) = up2 mulCEDU
(/^) :: MPFloat -> MPFloat -> MPFloat
(/^) = up2 divCEDU

fromIntegerUp :: Precision -> Integer -> MPFloat
fromIntegerUp p = up1 (fromIntegerCEDU p)
fromRationalUp :: Precision -> Rational -> MPFloat
fromRationalUp p = up1 (fromRationalCEDU p)

cosUp :: MPFloat -> MPFloat
cosUp = up1 cosCEDU
sinUp :: MPFloat -> MPFloat
sinUp = up1 sinCEDU
sqrtUp :: MPFloat -> MPFloat
sqrtUp = up1 sqrtCEDU
expUp :: MPFloat -> MPFloat
expUp = up1 expCEDU
logUp :: MPFloat -> MPFloat
logUp = up1 logCEDU


(+.) :: MPFloat -> MPFloat -> MPFloat
(+.) = down2 addCEDU
(-.) :: MPFloat -> MPFloat -> MPFloat
(-.) = down2 subCEDU
(*.) :: MPFloat -> MPFloat -> MPFloat
(*.) = down2 mulCEDU
(/.) :: MPFloat -> MPFloat -> MPFloat
(/.) = down2 divCEDU

fromIntegerDown :: Precision -> Integer -> MPFloat
fromIntegerDown p = down1 (fromIntegerCEDU p)
fromRationalDown :: Precision -> Rational -> MPFloat
fromRationalDown p = down1 (fromRationalCEDU p)

cosDown :: MPFloat -> MPFloat
cosDown = down1 cosCEDU
sinDown :: MPFloat -> MPFloat
sinDown = down1 sinCEDU
sqrtDown :: MPFloat -> MPFloat
sqrtDown = down1 sqrtCEDU
expDown :: MPFloat -> MPFloat
expDown = down1 expCEDU
logDown :: MPFloat -> MPFloat
logDown = down1 logCEDU


up1, down1 :: 
    (t -> BoundsCEDU MPFloat) -> 
    (t -> MPFloat)
up1 op x = ceduUp $ op x
down1 op x = ceduDown $ op x

up2, down2 :: 
    (t1 -> t2 -> BoundsCEDU MPFloat) -> 
    (t1 -> t2 -> MPFloat)
up2 op x y = ceduUp $ op x y
down2 op x y = ceduDown $ op x y


