{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  AERN2.MPBallWithGlobalPrec
    Description :  MPBall parametrised by a global precision
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    This type is useful for iRRAM-style arrow-generic computation
    over MPBall with a global precision.
-}
module AERN2.MPBallWithGlobalPrec
(
  -- * The protocol and type of objects depending on a global parameter
  MPBallWithGlobalPrecP, pMPBallWGPrec
  , wgprmName, wgprmId, wgprmSources, wgprmRename
  , wgprmQuery, (?), wgprmQueryA, wgprmListQueryA
  , MPBallWithGlobalPrecA, MPBallWithGlobalPrec
  , newMPBallWGPrec, newMPBallWGPrecSimple
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

import Control.Arrow

import AERN2.MP
import AERN2.MP.Dyadic

import AERN2.QA.Protocol
import AERN2.WithGlobalParam

type MPBallWithGlobalPrecP = WithGlobalParamP Precision MPBall

pMPBallWGPrec :: MPBallWithGlobalPrecP
pMPBallWGPrec = pWGParam Nothing (mpBall 0)

type MPBallWithGlobalPrecA to = WithGlobalParamA to Precision MPBall
type MPBallWithGlobalPrec = MPBallWithGlobalPrecA (->)

newMPBallWGPrec ::
  (QAArrow to)
  =>
  String -> [AnyProtocolQA to] -> ((Maybe (QAId to), Maybe (QAId to)) -> Precision `to` MPBall) -> MPBallWithGlobalPrecA to
newMPBallWGPrec = newWGParam Nothing (mpBall 0)

newMPBallWGPrecSimple ::
  (QAArrow to)
  =>
  ((Maybe (QAId to), Maybe (QAId to)) -> Precision `to` MPBall) -> MPBallWithGlobalPrecA to
newMPBallWGPrecSimple = newMPBallWGPrec "simple" []


$(declForTypes
  [[t| Integer |], [t| Int |], [t| Dyadic |], [t| Rational |]]
  (\ t -> [d|

    instance
      (QAArrow to)
      =>
      ConvertibleExactly $t (MPBallWithGlobalPrecA to)
      where
      safeConvertExactly x =
        Right $ newMPBallWGPrec (show x) [] (\_src -> arr $ p2a)
        where
        p2a p = convertP p x

  |]))
