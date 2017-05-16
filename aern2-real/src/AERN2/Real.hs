{-|
    Module      :  AERN2.Real
    Description :  Cauchy reals
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Cauchy reals
-}
module AERN2.Real
(
  module AERN2.MP
  , module AERN2.AccuracySG
  -- * The type of real numbers
  , CauchyRealP, pCR
  , realName, realId, realSources, realRename
  , realWithAccuracy, (?), realWithAccuracyA, realsWithAccuracyA
  , CauchyRealA, CauchyReal, newCR
  , (-:-)
  , convergentList2CauchyRealA
  , seqByPrecision2CauchyRealA
  , CanBeReal, real, CanBeRealA, realA
  , CanBeComplex, complex, CanBeComplexA, complexA
  -- * constants
  , pi, piA
  -- * arrow-generic versions of some operations
  , expA, logA, sqrtA, sinA, cosA
  -- * auxiliary functions for making new real operations
  , unaryOp, binaryOp, binaryOpWithPureArg
  , getCRFnNormLog
  , getInitQ1FromSimple, getInitQ1TFromSimple, getInitQ1Q2FromSimple
  , binaryWithBall
)
where

import Numeric.MixedTypes hiding (id)
-- import qualified Prelude as P

import Control.Arrow

import AERN2.MP

import AERN2.QA.Protocol
import AERN2.AccuracySG
import AERN2.Real.Type
import AERN2.Real.Helpers
import AERN2.Real.Comparison ()
import AERN2.Real.Ring ()
import AERN2.Real.Field ()
import AERN2.Real.Elementary
import AERN2.Real.Tests ()

import AERN2.QA.NetLog
import AERN2.QA.Strategy.Cached
import AERN2.QA.Strategy.Parallel

{-|
  Example arrow-generic real number computation
-}
_addA :: (QAArrow to) => (CauchyRealA to, CauchyRealA to) `to` CauchyRealA to
_addA =
  -- using -XArrows syntax:
  proc (x,y) ->
    (-:-)-< x + y
      -- -:- is a shorcut for qaRegister

_CRone :: (QAArrow to) => CauchyRealA to
_CRone = realA 1

_addApure :: CauchyReal
_addApure =
  _addA (_CRone, pi)
  -- equivalent to @_CRonePure + _CRonePure@ since registration does nothing

_addAcached :: QACachedA () (CauchyRealA QACachedA)
_addAcached =
  proc () ->
    do
    xReg <-(-:-)-< _CRone
    _addA -< (xReg,piA)
    -- equivalent to @(-:-)-< xReg + piA //.. [xReg,xReg]@
    -- not equivalent to @returnA -< xR + xR@ since there is no registration

_addAcachedPrint :: IO ()
_addAcachedPrint =
  printQANetLogThenResult $ executeQACachedA $
    proc () ->
      do
      x <-_addAcached -< ()
      (-?-) -< (x, bitsS 10)

_addslA :: (QAArrow to) => (CauchyRealA to) `to` (CauchyRealA to)
_addslA =
  proc x ->
    do
    xReg <-(-:-)-< x
    lx <-(-:-)-< log xReg
    sx <-(-:-)-< sqrt xReg
    a1 <- (-:-)-< sx + lx
    a2 <- (-:-)-< sx - lx
    (-:-) -< a1 * a2

_addslACachedPrint :: IO ()
_addslACachedPrint =
  printQANetLogThenResult $ executeQACachedA $
    proc () ->
      do
      x <-_addslA -< realA 2
      (-?-) -< (x, bitsS 100)

_addslAParPrint :: IO ()
_addslAParPrint =
  do
  r <- executeQAParA $
    proc () ->
      do
      x <-_addslA -< realA 2
      (-?-) -< (x, bitsS 100)
  printQANetLogThenResult r
