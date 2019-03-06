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
   )
where

import MixedTypesNumPrelude
import qualified Prelude as P

import AERN2.MP.Precision

import qualified Data.CDAR as MPLow

import AERN2.MP.Float.Aux
import AERN2.MP.Float.Type

{- common functions -}

instance CanNeg MPFloat where
  negate = P.negate

instance CanAbs MPFloat where
  abs = P.abs

addCEDU :: MPFloat -> MPFloat -> BoundsCEDU MPFloat
addCEDU = binaryCEDU (P.+)

subCEDU :: MPFloat -> MPFloat -> BoundsCEDU MPFloat
subCEDU = binaryCEDU (P.-)

mulCEDU :: MPFloat -> MPFloat -> BoundsCEDU MPFloat
mulCEDU = binaryCEDU (P.*)

divCEDU :: MPFloat -> MPFloat -> BoundsCEDU MPFloat
divCEDU x y 
    | y P.== (P.fromInteger 0) = getBoundsCEDU MPLow.Bottom
    | otherwise = binaryCEDU (P./) x y

recipCEDU :: MPFloat -> BoundsCEDU MPFloat
recipCEDU = unaryCEDU P.recip

{- special constants and functions -}

piCEDU :: Precision -> BoundsCEDU MPFloat
piCEDU pp = 
    getBoundsCEDU $ MPLow.piA (p2mpfrPrec pp)

cosCEDU :: MPFloat -> BoundsCEDU MPFloat
cosCEDU = unaryPrecCEDU 0 MPLow.cosA

sinCEDU :: MPFloat -> BoundsCEDU MPFloat
sinCEDU = unaryPrecCEDU 0 MPLow.sinA
            
sqrtCEDU :: MPFloat -> BoundsCEDU MPFloat
sqrtCEDU = unaryCEDU MPLow.sqrtA
            
expCEDU :: MPFloat -> BoundsCEDU MPFloat
expCEDU = unaryCEDU MPLow.expA

logCEDU :: MPFloat -> BoundsCEDU MPFloat
logCEDU = unaryCEDU MPLow.logA

{- auxiliary functions to automatically determine result precision from operand precisions -}

binaryCEDU ::
    (MPFloat -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat -> BoundsCEDU MPFloat)
binaryCEDU op x y =
    getBoundsCEDU $ op x y

unaryCEDU ::
    (MPFloat -> MPFloat) ->
    (MPFloat -> BoundsCEDU MPFloat)
unaryCEDU op x =
    getBoundsCEDU $ op x

unaryPrecCEDU ::
    Integer ->
    (MPLow.Precision -> MPFloat -> MPFloat) ->
    (MPFloat -> BoundsCEDU MPFloat)
unaryPrecCEDU addPrec op x@(MPLow.Approx mb _ _ s) =
    getBoundsCEDU $ op ((-s P.+ mb) P.+ (int addPrec)) x
unaryPrecCEDU addPrec op MPLow.Bottom =
    getBoundsCEDU $ op ((int $ integer defaultPrecision) P.+ (int addPrec)) MPLow.Bottom
    
