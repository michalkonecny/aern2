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
  module AERN2.AccuracySG
  -- * The type of real numbers
  , CauchyRealP, pCR
  , realName, realId, realSources, realRename
  , realWithAccuracy, (?), realWithAccuracyA
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

import AERN2.MP.Ball

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

{-|
  Example arrow-generic real number computation
-}
_addA :: (QAArrow to) => (CauchyRealA to, CauchyRealA to) `to` CauchyRealA to
_addA =
  -- using -XArrows syntax:
  proc (x,y) -> do
    s <-(-:-)-< x + y
      -- -:- is a shorcut for qaRegister
    returnA -< s

_CRonePure :: CauchyReal
_CRonePure = real 1

_addApure :: CauchyReal
_addApure =
  _addA (_CRonePure, pi)
  -- equivalent to @_CRonePure + _CRonePure@ since registration does nothing

_CRoneCached :: CauchyRealA QACachedA
_CRoneCached = realA 1

_addAcached :: QACachedA () (CauchyRealA QACachedA)
_addAcached =
  proc () ->
    do
    xReg <-(-:-)-< _CRoneCached
    _addA -< (xReg,piA)
    -- equivalent to @(-:-)-< xReg + piA //.. [xReg,xReg]@
    -- not equivalent to @returnA -< xR + xR@ since there is no registration

_addAcachedPrint :: IO ()
_addAcachedPrint =
  printQANetLogThenResult $ executeQACachedA $
    proc () ->
      do
      x <-_addAcached -< ()
      realWithAccuracyA -< (x, accuracySG $ bits 10)
