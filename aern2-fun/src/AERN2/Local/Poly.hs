module AERN2.Local.Poly where

import Numeric.MixedTypes
import AERN2.Interval
import AERN2.MP.Ball
import AERN2.Poly.Cheb
import AERN2.RealFun.Operations
import AERN2.Local.Basics

type LocalPoly a = Local (ChPoly a)

variable :: LocalPoly MPBall
variable l r _ac =
  x
  where
  x :: ChPoly MPBall
  x = varFn sampleFn ()
  sampleFn = constFn (dom, 1)
  dom = Interval l r
