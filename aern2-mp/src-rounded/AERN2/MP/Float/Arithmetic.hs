{-|
    Module      :  AERN2.MP.Float.Arithmetic
    Description :  Arbitrary precision floating point numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision floating-point numbers with up/down-rounded operations.
-}

module AERN2.MP.Float.Arithmetic
  (
   -- * MPFloat basic arithmetic
     addCEDU, subCEDU
   , mulCEDU, divCEDU, recipCEDU
   -- * MPFloat selected constants and operations
   , piCEDU
   , cosCEDU, sinCEDU
   , sqrtCEDU, expCEDU, logCEDU
   -- * auxiliary functions
   , constCEDU, unaryCEDU, binaryCEDU
   )
where

import MixedTypesNumPrelude
import qualified Prelude as P

import AERN2.MP.Precision

import qualified AERN2.MP.Float.RoundedAdaptor as MPLow

import AERN2.MP.Float.Aux
import AERN2.MP.Float.Type

one :: MPFloat
one = MPLow.one

{- common functions -}

instance CanNeg MPFloat where
  negate = ceduUp . unaryCEDU MPLow.neg

instance CanAbs MPFloat where
  abs x
    | x P.< MPLow.zero = negate x
    | otherwise = x


addCEDU :: MPFloat -> MPFloat -> BoundsCEDU MPFloat
addCEDU = binaryCEDU MPLow.add

subCEDU :: MPFloat -> MPFloat -> BoundsCEDU MPFloat
subCEDU = binaryCEDU MPLow.sub

mulCEDU :: MPFloat -> MPFloat -> BoundsCEDU MPFloat
mulCEDU = binaryCEDU MPLow.mul

divCEDU :: MPFloat -> MPFloat -> BoundsCEDU MPFloat
divCEDU = binaryCEDU MPLow.div

recipCEDU :: MPFloat -> BoundsCEDU MPFloat
recipCEDU x = divCEDU one x

{- special constants and functions -}

piCEDU :: Precision -> BoundsCEDU MPFloat
piCEDU pp = 
    constCEDU MPLow.pi (p2mpfrPrec pp)

cosCEDU :: MPFloat -> BoundsCEDU MPFloat
cosCEDU = unaryCEDU MPLow.cos

sinCEDU :: MPFloat -> BoundsCEDU MPFloat
sinCEDU = unaryCEDU MPLow.sin
            
sqrtCEDU :: MPFloat -> BoundsCEDU MPFloat
sqrtCEDU = unaryCEDU MPLow.sqrt
            
expCEDU :: MPFloat -> BoundsCEDU MPFloat
expCEDU = unaryCEDU MPLow.exp
            
logCEDU :: MPFloat -> BoundsCEDU MPFloat
logCEDU = unaryCEDU MPLow.log

{- auxiliary functions to automatically determine result precision from operand precisions -}

binaryCEDU :: 
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat -> MPFloat) -> 
    MPFloat -> MPFloat -> BoundsCEDU MPFloat
binaryCEDU op x y =
    getCEDU d u
    where
    d = op MPLow.Down p x y
    u = op MPLow.Up p x y
    p = p2mpfrPrec $ (getPrecision x) `max` (getPrecision y)

unaryCEDU :: 
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat) -> 
    MPFloat -> BoundsCEDU MPFloat
unaryCEDU op x =
    getCEDU d u
    where
    d = op MPLow.Down p x
    u = op MPLow.Up p x
    p = p2mpfrPrec $ getPrecision x

constCEDU :: 
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat) -> 
    MPLow.Precision -> BoundsCEDU MPFloat
constCEDU op p =
    getCEDU d u
    where
    d = op MPLow.Down p
    u = op MPLow.Up p
