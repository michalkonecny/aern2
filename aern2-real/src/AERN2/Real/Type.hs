{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-|
    Module      :  AERN2.Real.Type
    Description :  The type of constructive real numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    The type of constructive real numbers using convergent sequences of intervals.
-}
module AERN2.Real.Type where

import MixedTypesNumPrelude
-- import qualified Prelude as P

import qualified Numeric.CollectErrors as CN

import qualified Data.List as List

import AERN2.MP
import AERN2.MP.Dyadic

import AERN2.MP.WithCurrentPrec
import GHC.TypeNats

-- import AERN2.MP.Accuracy

{- Convergent partial sequences -}

newtype CSequence t = CSequence [CN t]

instance Show t => Show (CSequence t) where
  show (CSequence s) = 
    "{?(prec " <> (show $ integer p) <> "): " 
    <> (show $ s !! cseqShowDefaultIndex) <> "}"
    where
    p = cseqPrecisions !! cseqShowDefaultIndex

cseqShowDefaultIndex :: Integer
cseqShowDefaultIndex = 7

lift1 :: (CN t1 -> CN t2) -> CSequence t1 -> CSequence t2
lift1 f (CSequence a1) = CSequence (map f a1)

lift2 :: (CN t1 -> CN t2 -> CN t3) -> CSequence t1 -> CSequence t2 -> CSequence t3
lift2 f (CSequence a1) (CSequence a2) = CSequence (zipWith f a1 a2)

lift1T :: (CN t1 -> t2 -> CN t3) -> CSequence t1 -> t2 -> CSequence t3
lift1T f (CSequence a1) a2 = CSequence (map (flip f a2) a1)

liftT1 :: (t1 -> CN t2 -> CN t3) -> t1 -> CSequence t2 -> CSequence t3
liftT1 f a1 (CSequence a2) = CSequence (map (f a1) a2)

cseqPrecisions :: [Precision]
cseqPrecisions = standardPrecisions (prec 10)

cseqIndexForPrecision :: Precision -> Integer
cseqIndexForPrecision p =
  case List.findIndex (>= p) cseqPrecisions of
    Nothing -> error $ "unable to find index for precision " ++ show p
    Just i -> integer i

cseqFromPrecFunction :: (Precision -> CN b) -> CSequence b
cseqFromPrecFunction withP = CSequence $ map withP cseqPrecisions

cseqFromWithCurrentPrec :: (forall p. (KnownNat p) => WithCurrentPrec (CN b) p) -> CSequence b
cseqFromWithCurrentPrec (withCurrentP :: (forall p. (KnownNat p) => WithCurrentPrec (CN b) p)) = 
  CSequence $ map withP cseqPrecisions
  where
  withP p = runWithPrec p withCurrentP :: CN b

crealFromWithCurrentPrec :: (forall p. (KnownNat p) => WithCurrentPrec (CN MPBall) p) -> CReal
crealFromWithCurrentPrec = cseqFromWithCurrentPrec

{- Cauchy real numbers -}

type CReal = CSequence MPBall

type HasCReals t = ConvertibleExactly CReal t

type CanBeCReal t = ConvertibleExactly t CReal

creal :: (CanBeCReal t) => t -> CReal
creal = convertExactly

crealFromPrecFunction :: (Precision -> CN MPBall) -> CReal
crealFromPrecFunction = cseqFromPrecFunction

{- Extracting approximations -}

class CanExtractApproximation e q where
  type ExtractedApproximation e q
  {-| Get an approximation of an exact value using the given query -}
  extractApproximation :: e {-^ exact value -} -> q {-^ query -} -> ExtractedApproximation e q

infix 1 ?

(?) :: CanExtractApproximation e q => e -> q -> ExtractedApproximation e q
(?) = extractApproximation

instance (HasAccuracy t) => CanExtractApproximation (CSequence t) Accuracy where
  type ExtractedApproximation (CSequence t) Accuracy = CN t
  extractApproximation (CSequence s) ac = aux s
    where
    aux (bCN : rest) 
      | CN.hasCertainError bCN = bCN
      | getAccuracy bCN >= ac = bCN
      | otherwise = aux rest
    aux [] =
        CN.noValueNumErrorPotential $ 
          CN.NumError "failed to find an approximation with sufficient accuracy"
  
{-| Get a ball approximation of the real number with at least the specified accuracy -}
realWithAccuracy :: CReal -> Accuracy -> CN MPBall
realWithAccuracy = extractApproximation

instance CanExtractApproximation (CSequence t) Precision where
  type ExtractedApproximation (CSequence t) Precision = CN t
  extractApproximation (CSequence s) p =
    s !! (cseqIndexForPrecision p)

instance ConvertibleWithPrecision CReal (CN MPBall) where
  safeConvertP p r = Right $ r ? p

-- {- exact conversions -}

instance ConvertibleExactly CReal CReal where
  safeConvertExactly = Right

instance ConvertibleExactly Rational CReal where
  safeConvertExactly x =
    Right $ crealFromPrecFunction (cn . flip mpBallP x)

instance ConvertibleExactly Integer CReal where
  safeConvertExactly = safeConvertExactly . rational

instance ConvertibleExactly Int CReal where
  safeConvertExactly = safeConvertExactly . rational

instance ConvertibleExactly Dyadic CReal where
  safeConvertExactly = safeConvertExactly . rational

instance ConvertibleExactly (WithAnyPrec (CN MPBall)) CReal where
  safeConvertExactly (WithAnyPrec wcp) = Right $ crealFromWithCurrentPrec wcp

_example1 :: CReal
_example1 = creal 1.0

_example2 :: CN MPBall
_example2 = (creal $ 1/3) ? (bits 100)

_example3 :: CN MPBall
_example3 = convertP (prec 100) (creal $ 1/3)
