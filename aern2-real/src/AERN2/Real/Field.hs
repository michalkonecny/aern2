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

import Numeric.MixedTypes hiding (id)
-- import qualified Prelude as P

import Control.Category (id)
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

{- negation -}

instance (ArrowChoice to) => CanNeg (CauchyRealA to) where
  negate = unaryOp "-" negate (getInitQ1FromSimple $ proc q -> returnA -< q)

{- addition -}

instance (ArrowChoice to) => CanAddAsymmetric (CauchyRealA to) (CauchyRealA to) where
  add = binaryOp "+" add (getInitQ1Q2FromSimple $ proc q -> returnA -< (q,q))

instance (ArrowChoice to) => CanAddAsymmetric (CauchyRealA to) Integer where
  type AddType (CauchyRealA to) Integer = CauchyRealA to
  add = binaryOpWithPureArg "+" add (getInitQ1TFromSimple id)

instance (ArrowChoice to) => CanAddAsymmetric Integer (CauchyRealA to) where
  type AddType Integer (CauchyRealA to) = CauchyRealA to
  add = flip add

instance (ArrowChoice to) => CanAddAsymmetric (CauchyRealA to) Int where
  type AddType (CauchyRealA to) Int = CauchyRealA to
  add = binaryOpWithPureArg "+" add (getInitQ1TFromSimple id)

instance (ArrowChoice to) => CanAddAsymmetric Int (CauchyRealA to) where
  type AddType Int (CauchyRealA to) = CauchyRealA to
  add = flip add

instance (ArrowChoice to) => CanAddAsymmetric (CauchyRealA to) Rational where
  type AddType (CauchyRealA to) Rational = CauchyRealA to
  add = binaryOpWithPureArg "+" add (getInitQ1TFromSimple id)

instance (ArrowChoice to) => CanAddAsymmetric Rational (CauchyRealA to) where
  type AddType Rational (CauchyRealA to) = CauchyRealA to
  add = flip add

{- MPBall + CauchyReal = MPBall, only allowed in the (->) arrow  -}

mpBallSimilarTo :: MPBall -> CauchyReal -> MPBall
mpBallSimilarTo b r =
  qaMakeQuery r $ getAccuracyIfExactUsePrec b

getAccuracyIfExactUsePrec :: MPBall -> Accuracy
getAccuracyIfExactUsePrec ball =
  case getAccuracy ball of
    Exact -> bits (getPrecision ball)
    result -> result

instance CanAddAsymmetric CauchyReal MPBall where
  type AddType CauchyReal MPBall = MPBall
  add r b = add (mpBallSimilarTo b r) b

instance CanAddAsymmetric MPBall CauchyReal where
  type AddType MPBall CauchyReal = MPBall
  add = flip add

{- subtraction -}

instance (ArrowChoice to) => CanSub (CauchyRealA to) (CauchyRealA to) where
  sub = binaryOp "-" sub (getInitQ1Q2FromSimple $ proc q -> returnA -< (q,q))

instance (ArrowChoice to) => CanSub (CauchyRealA to) Integer
instance (ArrowChoice to) => CanSub Integer (CauchyRealA to)
instance (ArrowChoice to) => CanSub (CauchyRealA to) Int
instance (ArrowChoice to) => CanSub Int (CauchyRealA to)
instance (ArrowChoice to) => CanSub (CauchyRealA to) Rational
instance (ArrowChoice to) => CanSub Rational (CauchyRealA to)
instance CanSub CauchyReal MPBall
instance CanSub MPBall CauchyReal
