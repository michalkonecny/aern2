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
  -- * mini tests
  , _addslACachedPrint
  , _addslAParPrint
  , _example_pif
  , _trisection
)
where

import AERN2.MP
import AERN2.QA.Protocol
import AERN2.AccuracySG
import AERN2.Sequence ()
import AERN2.Real.Type
import AERN2.Real.Arithmetic (pi, piA)
import AERN2.Real.Tests ()

-- imports used in examples below:

import MixedTypesNumPrelude
-- import qualified Prelude as P

import Control.Arrow

import AERN2.QA.NetLog
import AERN2.QA.Strategy.Cached
import AERN2.QA.Strategy.Parallel

import Text.Printf
import AERN2.MP.Dyadic
import AERN2.Sequence


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

_addslA :: (QAArrow to) => (CauchyRealA to) `to` (CauchyRealCNA to)
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
  print r

{- parallel branching -}

_example_pif :: CauchyReal -> CauchyReal
_example_pif r =
  if r < 0 then -r else r -- abs via parallel if

_trisection ::
  (Dyadic -> CauchyReal) ->
  (Dyadic,Dyadic) ->
  CauchyRealCN
_trisection f (l,r) =
  newSeqSimple (cn $ mpBall 0) $ fromSegment l r
  where
  fromSegment :: Dyadic -> Dyadic -> AccuracySG -> CN MPBall
  fromSegment a b ac
    | getAccuracy ab >= ac  = cn ab
    | otherwise             = pick [tryM m1, tryM m2]
    where
    ab = fromEndpoints (mpBall a) (mpBall b) :: MPBall
    m1 = (5*a + 3*b)*(dyadic ((1/8)⚡))
    m2 = (3*a + 5*b)*(dyadic ((1/8)⚡))
    tryM :: Dyadic -> Sequence (Maybe (CN MPBall))
    tryM m = newSeqSimple Nothing withAC
      where
      withAC :: AccuracySG -> Maybe (CN MPBall)
      withAC acF
        | fa * fm !<! 0 = Just $ fromSegment a m ac
        | fm * fb !<! 0 = Just $ fromSegment m b ac
        | fa * fb !>=! 0 = Just $ err
        | otherwise = Nothing
        where
        fa = (f a) ? acF
        fm = (f m) ? acF
        fb = (f b) ? acF
    err :: CN MPBall
    err =
      noValueNumErrorCertainCN $
        NumError $
          printf "trisection: function does not have opposite signs on points %s %s" (show a) (show b)
