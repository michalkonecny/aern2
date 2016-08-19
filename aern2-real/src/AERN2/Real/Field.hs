{-|
    Module      :  AERN2.Real.Field
    Description :  field operations on CR
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Field operations on Cauchy Real numbers.
-}
module AERN2.Real.Field
(
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

import Control.Arrow

import AERN2.MP.Ball
-- import AERN2.MP.Precision
-- import AERN2.MP.Accuracy

import AERN2.QA
import AERN2.Real.Type
import AERN2.Real.Aux

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace
    | shouldTrace = trace
    | otherwise = const id

_dummy :: ()
_dummy = maybeTrace "dummy" ()


instance (QAArrow to) => CanAddAsymmetric (CauchyRealA to) (CauchyRealA to) where
  type AddType (CauchyRealA to) (CauchyRealA to) = (CauchyRealA to)
  add = binaryOp "+" add (getInitQ1Q2FromSimple $ proc q -> returnA -< (q,q))

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
_CRonePure = newCR "one" (\ _ac -> mpBall 1)

_addApure :: CauchyReal
_addApure = _addA (_CRonePure, _CRonePure)

_CRoneCached :: CauchyRealA QACachedA
_CRoneCached = newCR "one" (Kleisli $ \ _ac -> return $ mpBall 1)

_addAcached :: QACachedA () (CauchyRealA QACachedA)
_addAcached =
  proc () ->
    do
    xReg <-(-:-)-< _CRoneCached //.. []
    _addA -< (xReg,xReg)
    -- returnA -< xR + xR

_addAcachedPrint :: IO ()
_addAcachedPrint =
  printQANetLogThenResult $ executeQACachedA $
    proc () ->
      do
      x <-_addAcached -< ()
      qaMakeQueryA -< (x, bits 10)
