{-|
    Module      :  AERN2.Real.Type
    Description :  The type of Cauchy real numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    The type of Cauchy real numbers
-}
module AERN2.Real.Type
(
  CauchyRealP, pCR
  , CauchyRealA, CauchyReal, newCR
  , real
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

import Control.Arrow

import AERN2.MP.Dyadic
import AERN2.MP.Ball

import AERN2.QA

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

data CauchyRealP = CauchyRealP deriving (Show)

instance QAProtocol CauchyRealP where
  type Q CauchyRealP = Accuracy
  type A CauchyRealP = MPBall
  -- sampleQ _ = NoInformation

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

newCR :: (QAArrow to) => String -> Accuracy `to` MPBall -> CauchyRealA to
newCR name makeQ = newQA name pCR NoInformation makeQ

instance Show CauchyReal where
  show r = show $ qaMakeQuery r (bits 100)

{- conversions -}

type CanBeRealA to t = ConvertibleExactly t (CauchyRealA to)

real :: (CanBeRealA to t) => t -> CauchyRealA to
real = convertExactly

instance (QAArrow to) => ConvertibleExactly Integer (CauchyRealA to) where
  safeConvertExactly x =
    Right $ newCR (show x) (arr $ const $ mpBall x)

instance (QAArrow to) => ConvertibleExactly Int (CauchyRealA to) where
  safeConvertExactly x =
    Right $ newCR (show x) (arr $ const $ mpBall x)

instance (QAArrow to) => ConvertibleExactly Dyadic (CauchyRealA to) where
  safeConvertExactly x =
    Right $ newCR (show x) (arr $ const $ mpBall x)

instance (QAArrow to) => ConvertibleExactly Rational (CauchyRealA to) where
  safeConvertExactly x =
    Right $ newCR (show x) (arr makeQ)
    where
    makeQ = seqByPrecision2CauchySeq (flip mpBallP x)

instance ConvertibleWithPrecision CauchyReal MPBall where
  safeConvertP p r =
    Right $ setPrecision p $ qaMakeQuery r (bits p + 10)
