module AERN2.MPoly.Power where

import MixedTypesNumPrelude
import AERN2.MP.Accuracy
import AERN2.MP.Ball

import AERN2.MPoly.Power.Type
import qualified AERN2.Poly.Cheb as Cheb

mSine :: Integer -> Integer -> Accuracy -> PowMPoly MPBall
mSine n i ac =
  fromCheb (sin(setAccuracyGuide ac Cheb._chPolyX)) n i
