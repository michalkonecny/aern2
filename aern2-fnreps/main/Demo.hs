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

-- import AERN2.Real
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

x_PPoly :: Accuracy -> PPoly
x_PPoly = PPoly.fromPoly . x_Poly

unaryIntervalDom :: DyadicInterval
unaryIntervalDom = dyadicInterval (-1,1)

x_LPoly :: LPoly
x_LPoly = LPoly.variable

x_LPPoly :: LPPoly
x_LPPoly = LPPoly.fromPoly LPoly.variable

bumpy :: (CanSinCosSameType t, CanMulBy t Integer, CanMinMaxSameType t) => t -> t
-- bumpy :: _ => t -> t
bumpy x = sin (10*x) `max` cos (11 * x)

bumpy_I_Poly :: MPBall
bumpy_I_Poly = integrateOverDom (bumpy $ x_Poly (bits 5)) unaryIntervalDom

bumpy_I_PPoly :: MPBall
bumpy_I_PPoly = integrateOverDom (bumpy $ x_PPoly (bits 25)) unaryIntervalDom

bumpy_I_LPoly :: MPBall
bumpy_I_LPoly = integrateOverDom (bumpy x_LPoly) unaryIntervalDom (bits 20)

bumpy_I_LPPoly :: MPBall
bumpy_I_LPPoly = integrateOverDom (bumpy x_LPPoly) unaryIntervalDom (bits 20)
