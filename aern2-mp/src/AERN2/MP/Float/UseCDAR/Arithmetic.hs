{-|
    Module      :  AERN2.MP.Float.UseCDAR.Arithmetic
    Description :  Arbitrary precision floating point numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision floating-point numbers with up/down-rounded operations.
-}

module AERN2.MP.Float.UseCDAR.Arithmetic
  (
   -- * MPFloat basic arithmetic
     addCentreErr, subCentreErr
   , mulCentreErr, divCentreErr, recipCentreErr
   -- * MPFloat selected constants and operations
   , piCentreErr
   , cosCentreErr, sinCentreErr
   , sqrtCentreErr, expCentreErr, logCentreErr
   )
where

import MixedTypesNumPrelude
import qualified Prelude as P

import AERN2.MP.Precision

import qualified Data.CDAR as MPLow
import AERN2.MP.Float.UseCDAR.Type

{- common functions -}

instance CanNeg MPFloat where
  negate = P.negate

instance CanAbs MPFloat where
  abs = P.abs

addCentreErr :: MPFloat -> MPFloat -> (MPFloat, MPFloat)
addCentreErr = binaryCentreErr (P.+)

subCentreErr :: MPFloat -> MPFloat -> (MPFloat, MPFloat)
subCentreErr= binaryCentreErr (P.-)

mulCentreErr :: MPFloat -> MPFloat -> (MPFloat, MPFloat)
mulCentreErr= binaryCentreErr (P.*)

divCentreErr :: MPFloat -> MPFloat -> (MPFloat, MPFloat)
divCentreErr= binaryCentreErr (P./)

recipCentreErr :: MPFloat -> (MPFloat, MPFloat)
recipCentreErr = unaryCentreErr P.recip

{- special constants and functions -}

piCentreErr :: Precision -> (MPFloat, MPFloat)
piCentreErr pp = 
    getCentreErr $ MPLow.piA (p2mpfrPrec pp)

cosCentreErr :: MPFloat -> (MPFloat, MPFloat)
cosCentreErr = unaryPrecCentreErr 0 MPLow.cosA

sinCentreErr :: MPFloat -> (MPFloat, MPFloat)
sinCentreErr = unaryPrecCentreErr 0 MPLow.sinA
            
sqrtCentreErr :: MPFloat -> (MPFloat, MPFloat)
sqrtCentreErr = unaryPrecCentreErr 0 MPLow.sqrtA
            
expCentreErr :: MPFloat -> (MPFloat, MPFloat)
expCentreErr = unaryPrecCentreErr 0 MPLow.expA
            
logCentreErr :: MPFloat -> (MPFloat, MPFloat)
logCentreErr = unaryPrecCentreErr 0 MPLow.logA
            
{- auxiliary functions to automatically determine result precision from operand precisions -}

binaryCentreErr ::
    (MPFloat -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat -> (MPFloat, MPFloat))
binaryCentreErr op x y =
    getCentreErr $ op x y

unaryCentreErr ::
    (MPFloat -> MPFloat) ->
    (MPFloat -> (MPFloat, MPFloat))
unaryCentreErr op x =
    getCentreErr $ op x

unaryPrecCentreErr ::
    Integer ->
    (MPLow.Precision -> MPFloat -> MPFloat) ->
    (MPFloat -> (MPFloat, MPFloat))
unaryPrecCentreErr addPrec op x@(MPLow.Approx _ _ p) =
    getCentreErr $ op (p P.+ (int addPrec)) x
unaryPrecCentreErr _ _ MPLow.Bottom =
    error "unaryPrecCentreErr: Bottom"


getCentreErr :: MPFloat -> (MPFloat, MPFloat)
getCentreErr (MPLow.Approx m e s) = 
    (MPLow.Approx m 0 s, MPLow.Approx e 0 s)
getCentreErr MPLow.Bottom =
    error "getCentreErr: Bottom"
    
