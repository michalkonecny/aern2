{-# LANGUAGE OverloadedRecordDot #-}
module AERN2.MP.Affine.Conversions
  ( CanBeMPAffine,
    mpAffineWithSample,
    mpAffineFromBall
  )
where

import AERN2.MP.Affine.Type
import AERN2.MP (MPBall (MPBall), errorBound, mpBallP, prec)
import AERN2.MP.Float (mpFloat)
import Data.Hashable
import qualified Data.Map as Map
import GHC.Records
import MixedTypesNumPrelude
import Test.QuickCheck ()

instance ConvertibleExactly MPAffine MPBall where
  safeConvertExactly :: MPAffine -> ConvertResult MPBall
  safeConvertExactly aff = Right $ MPBall centre e
    where
      mpAffineFlattened = mpAffNormalise $ aff {config = aff.config {maxTerms = int 1}}
      (MPAffine {centre, errTerms}) = mpAffineFlattened
      e = case Map.toList errTerms of
          [] -> errorBound 0
          ((_, coeff):_) -> errorBound (abs coeff) -- should have at most one term

type CanBeMPAffine t = ConvertibleExactly (WithSample MPAffine t) MPAffine

mpAffineWithSample :: (CanBeMPAffine t) => MPAffine -> t -> MPAffine
mpAffineWithSample sample t = convertExactly (WithSample sample t)

instance ConvertibleExactly (WithSample MPAffine (ErrorTermId, MPBall)) MPAffine where
  safeConvertExactly :: (WithSample MPAffine (ErrorTermId, MPBall)) -> ConvertResult MPAffine
  safeConvertExactly (WithSample sample (key, MPBall c e))
    | e == 0 =
        Right $ MPAffine {config, centre = c, errTerms = Map.empty}
    | otherwise =
        Right $ MPAffine {config, centre = c, errTerms = Map.singleton key (mpFloat e)}
    where
      config = sample.config

mpAffineFromBall :: (Hashable errIdItem) => MPAffine -> errIdItem -> MPBall -> MPAffine
mpAffineFromBall sample errIdItem b =
  mpAffineWithSample sample (ErrorTermId (hash errIdItem), b)

instance ConvertibleExactly (WithSample MPAffine Integer) MPAffine where
  safeConvertExactly :: (WithSample MPAffine Integer) -> ConvertResult MPAffine
  safeConvertExactly (WithSample sample n) =
    Right $ mpAffineFromBall sample n (mpBallP p n)
    where
      p = prec sample.config.precision

instance ConvertibleExactly (WithSample MPAffine Rational) MPAffine where
  safeConvertExactly :: (WithSample MPAffine Rational) -> ConvertResult MPAffine
  safeConvertExactly (WithSample sample q) =
    Right $ mpAffineFromBall sample q (mpBallP p q)
    where
      p = prec sample.config.precision