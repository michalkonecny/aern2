{-|
    Module      :  AERN2.Real.Type
    Description :  Cauchy real numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Cauchy real numbers
-}
module AERN2.Real.Type
(
  QAProtocol(..), QA(..)
  , QAArrow(..)
  , CauchyRealP, pCR
  , CauchyRealA, CauchyReal
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

import Control.Arrow

import AERN2.MP.Ball
-- import AERN2.MP.Precision
-- import AERN2.MP.Accuracy

import AERN2.QA

-- import Debug.Trace (trace)
--
-- shouldTrace :: Bool
-- shouldTrace = False
-- --shouldTrace = True
--
-- maybeTrace :: String -> a -> a
-- maybeTrace
--     | shouldTrace = trace
--     | otherwise = const id
--
-- _dummy :: ()
-- _dummy = maybeTrace "dummy" ()

data CauchyRealP = CauchyRealP deriving (Show)

instance QAProtocol CauchyRealP where
  type Q CauchyRealP = Accuracy
  type A CauchyRealP = MPBall
  sampleQ _ = NoInformation

pCR :: CauchyRealP
pCR = CauchyRealP

instance QAProtocolCacheable CauchyRealP where
  type QACache CauchyRealP = Maybe MPBall
  newQACache _ = Nothing
  lookupQACache _ cache ac =
    case cache of
      Just b | getAccuracy b >= ac -> Just b
      _ -> Nothing
  updateQACache _ Nothing _ b = Just b
  updateQACache _ (Just b1) _ b2 = Just (b1 `intersect` b2)

type CauchyRealA to = QA to CauchyRealP

type CauchyReal = CauchyRealA (->)

instance (QAArrow to) => CanAddAsymmetric (CauchyRealA to) (CauchyRealA to) where
  type AddType (CauchyRealA to) (CauchyRealA to) = (CauchyRealA to)
  add x y =
    QA p Nothing name makeQ
    where
      name = "+" -- "(" ++ (qaName x) ++ "+" ++ (qaName y) ++ ")"
      p = qaProtocol x
      makeQ =
          proc ac -> do
            xB <- qaMakeQuery x -< ac
            yB <- qaMakeQuery y -< ac
            returnA -< xB + yB -- TODO: ensure accuracy ac

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
_CRonePure = QA pCR Nothing "one" (\ _ac -> mpBall 1)

_addApure :: CauchyReal
_addApure = _addA (_CRonePure, _CRonePure)

_CRoneCached :: CauchyRealA QACachedA
_CRoneCached = QA pCR Nothing "one" (Kleisli $ \ _ac -> return $ mpBall 1)

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
