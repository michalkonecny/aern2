{-# LANGUAGE CPP #-}
{-|
    Module      :  AERN2.MP.Float.Arithmetic
    Description :  Arbitrary precision floating point numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision floating-point numbers with up/down-rounded operations.

    Currently, we use hmpfr when compiling with ghc 7.10 and higher
    and haskell-mpfr when compiling with ghc 7.8.
-}

module AERN2.MP.Float.Arithmetic
  (
   piUp, piDown
   -- * MPFloat basic arithmetic
   , addUp, addDown, subUp, subDown
   , mulUp, mulDown, divUp, divDown, recipUp, recipDown
   -- * MPFloat selected operations
   , cosUp, cosDown, sinUp, sinDown
   , sqrtUp, sqrtDown, expUp, expDown, logUp, logDown
   )
where

import Numeric.MixedTypes
import qualified Prelude as P

import AERN2.MP.Precision
import AERN2.MP.Float.Type

#ifdef HaskellMPFR
import qualified Data.Approximate.MPFRLowLevel as MPLow

one = MPLow.fromInt MPLow.Up (P.fromInteger 10) (int 1)

#endif
#ifdef HMPFR
import qualified Data.Number.MPFR as MPLow

one = MPLow.one
#endif

{- common functions -}

instance CanNeg MPFloat where
  negate = unaryUp MPLow.neg

instance CanAbs MPFloat where
  abs x
    | x P.< MPLow.zero = negate x
    | otherwise = x

addUp, addDown :: MPFloat -> MPFloat -> MPFloat
addUp = binaryUp True MPLow.add
addDown = binaryDown True MPLow.add

subUp, subDown :: MPFloat -> MPFloat -> MPFloat
subUp = binaryUp True MPLow.sub
subDown = binaryDown True MPLow.sub

mulUp, mulDown :: MPFloat -> MPFloat -> MPFloat
mulUp = binaryUp True MPLow.mul
mulDown = binaryDown True MPLow.mul

divUp,divDown :: MPFloat -> MPFloat -> MPFloat
divUp = binaryUp False MPLow.div
divDown = binaryDown False MPLow.div

recipUp :: MPFloat -> MPFloat
recipUp x = divUp one x

recipDown :: MPFloat -> MPFloat
recipDown x = divDown one x


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

logUp :: MPFloat -> MPFloat
logUp = unaryUp MPLow.log

logDown :: MPFloat -> MPFloat
logDown = unaryDown MPLow.log


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
    (MPFloat -> MPFloat -> MPFloat)
binaryUp = binaryApprox True

binaryDown ::
    Bool ->
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat -> MPFloat)
binaryDown = binaryApprox False

binaryApprox ::
    Bool -> Bool ->
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat -> MPFloat)
binaryApprox isUp _canBeExact opRP x y =
    withPrec pMax
    where
    pMax = (getPrecision x) `max` (getPrecision y)
    withPrec p
        | isUp = opRP MPLow.Up (p2mpfrPrec p) x y
        | otherwise = opRP MPLow.Down (p2mpfrPrec p) x y
