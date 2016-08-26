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
  QA(..)
  , CauchyRealP, pCR
  , CauchyRealA, CauchyReal, newCR
  , real, realA
  , pi, piA, sqrtA, expA, logA, sinA, cosA
)
where

import Numeric.MixedTypes hiding (id)
-- import qualified Prelude as P

import Control.Arrow

import AERN2.MP.Ball

import AERN2.QA
import AERN2.Real.Type
-- import AERN2.Real.Aux ()
import AERN2.Real.Ring ()
import AERN2.Real.Field ()
import AERN2.Real.Elementary

{-|
  Example arrow-generic real number computation
-}
_addA :: (QAArrow to) => (CauchyRealA to, CauchyRealA to) `to` CauchyRealA to
_addA =
  -- using -XArrows syntax:
  proc (x,y) -> do
    s <-(-:-)-< x + y //.. [x,y] -- listing source values for better tracing messages
      -- -:- and //.. are shorcuts for qaRegister and (,) respectively
    returnA -< s

_CRonePure :: CauchyReal
_CRonePure = real 1

_addApure :: CauchyReal
_addApure =
  _addA (_CRonePure, _CRonePure)
  -- equivalent to @_CRonePure + _CRonePure@ since registration does nothing

_CRoneCached :: CauchyRealA QACachedA
_CRoneCached = realA 1

_addAcached :: QACachedA () (CauchyRealA QACachedA)
_addAcached =
  proc () ->
    do
    xReg <-(-:-)-< _CRoneCached //.. []
    _addA -< (xReg,xReg)
    -- equivalent to @(-:-)-< xReg + xReg //.. [xReg,xReg]@
    -- not equivalent to @returnA -< xR + xR@ since there is no registration

_addAcachedPrint :: IO ()
_addAcachedPrint =
  printQANetLogThenResult $ executeQACachedA $
    proc () ->
      do
      x <-_addAcached -< ()
      qaMakeQueryA -< (x, bits 10)
