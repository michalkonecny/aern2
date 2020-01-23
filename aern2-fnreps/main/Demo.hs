{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
module Demo where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import AERN2.MP
-- import qualified AERN2.MP.Ball as MPBall

import AERN2.Real
-- import AERN2.Limit

import AERN2.Interval

import AERN2.RealFun.Operations

import AERN2.Poly.Cheb (ChPoly)

import qualified AERN2.PPoly as PPoly
import AERN2.PPoly (PPoly)

import qualified AERN2.Local as Local ()
import qualified AERN2.Local.PPoly as LPPoly
import qualified AERN2.Local.Poly as LPoly

---------------------------------------------------------

type LPoly = LPoly.LocalPoly MPBall
type LPPoly = LPPoly.LocalPPoly

x_Poly :: Accuracy -> ChPoly MPBall
x_Poly acG =
  setAccuracyGuide acG $ varFn (unaryIntervalDom, bits 10) ()

unaryIntervalDom :: DyadicInterval
unaryIntervalDom = dyadicInterval (-1,1)


---------------------------------------------------------------------
-- THE IDENTITY FUNCTION (USING VARIOUS TYPES)
x_PPoly :: Accuracy -> PPoly
x_PPoly = PPoly.fromPoly . x_Poly

x_LPPoly :: LPPoly
x_LPPoly = LPPoly.fromPoly LPoly.variable

---------------------------------------------------------------------
-- REAL FUNCTION DEFINITION (GENERIC IN TYPE)
bumpy :: (CanSinCosSameType t, CanMulBy t Integer, CanMinMaxSameType t) => t -> t
-- bumpy :: _ => t -> t
bumpy x = sin (10*x) `max` cos (11 * x)

---------------------------------------------------------------------
-- EVALUATING AND INTEGRATING THE FUNCTION USING PIECEWISE POLYNOMIAL APPROXIMATIONS
bumpy_I_PPoly :: Integer -> MPBall
bumpy_I_PPoly n = integrateOverDom (bumpy $ x_PPoly (bits n)) unaryIntervalDom

bumpy_I_LPPoly :: Integer -> MPBall
bumpy_I_LPPoly n = integrateOverDom (bumpy x_LPPoly) unaryIntervalDom (bits n)

---------------------------------------------------------------------
-- UNIVARIATE GLOBAL OPTIMISATION
-- |sin x - (x - x^3/6 + x^5/120 - x^7/5040)| <= eps

sinT7 :: 
  (CanAddSubMulBy t t, CanPowCNBy t Integer
  , CanDivCNBy t Integer) => 
  t -> t
sinT7 x = x - x^!3/!6 + x^!5/!120 - x^!7/!5040

sinT7horner :: 
  (CanAddSubMulBy t t, CanPowCNBy t Integer
  , CanSub Integer t, SubType Integer t ~ t
  , CanDivCNBy t Integer) => 
  t -> t
sinT7horner x = 
  x * (1 - x^!2/!6 * (1 - x^!2/!20*(1 - x^!2/!42)))

sinT7fp :: 
  (CanAddSubMulBy t t, CanPowCNBy t Integer, CanMulBy t MPBall
  , CanSub Integer t, SubType Integer t ~ t
  , CanDivCNBy t Integer) => 
  Integer -> t -> t
sinT7fp prec x = 
  x *. (1 -. x2/!.6 *. (1 -. x2/!.20*.(1 -. x2/!.42)))
  where
  x2 = (x^!2)*onePMe
  a -. b = (a-b)*onePMe
  a *. b = (a*b)*onePMe
  a /!. b = (a/!b)*onePMe
  infixl 6 -.
  infixl 7 *.
  infixl 7 /!.
  onePMe = mpBall (1,0.5^!(prec))

domSP :: DyadicInterval
domSP = dyadicInterval (fpAvoidDenorm, 0.75)

fpAvoidDenorm :: Rational
fpAvoidDenorm = 0.5^!(23) + 0.5^!(126)

xP8 :: Accuracy -> ChPoly MPBall
xP8 acG = setAccuracyGuide acG $ varFn (domSP, acG) ()

sinT7fpErr :: Integer -> Accuracy -> MPBall
sinT7fpErr prec acG = 
  max (abs diffMin) (abs diffMax)
  where
  diffMax = maximumOverDom (sinT7fp prec x - sinT7horner x) domSP
  diffMin = minimumOverDom (sinT7fp prec x - sinT7horner x) domSP
  x = xP8 acG
