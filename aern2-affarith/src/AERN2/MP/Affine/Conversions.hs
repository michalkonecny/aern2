module AERN2.MP.Affine.Conversions
  ( CanBeMPAffine,
    mpAffine,
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

instance ConvertibleExactly MPAffine MPBall where
  safeConvertExactly :: MPAffine -> ConvertResult MPBall
  safeConvertExactly aff = Right $ MPBall centre e
    where
      mpAffineFlattened = mpAffNormalise $ aff {config = aff.config {maxTerms = int 1}}
      (MPAffine {centre, errTerms}) = mpAffineFlattened
      e = errorBound $ abs $ snd $ head (Map.toList errTerms) -- should have one term only

type CanBeMPAffine t = ConvertibleExactly (MPAffineConfig, t) MPAffine

mpAffine :: (CanBeMPAffine t) => MPAffineConfig -> t -> MPAffine
mpAffine config t = convertExactly (config, t)

instance ConvertibleExactly (MPAffineConfig, (ErrorTermId, MPBall)) MPAffine where
  safeConvertExactly :: (MPAffineConfig, (ErrorTermId, MPBall)) -> ConvertResult MPAffine
  safeConvertExactly (config, (key, MPBall c e))
    | e == 0 =
        Right $ MPAffine {config, centre = c, errTerms = Map.empty}
    | otherwise =
        Right $ MPAffine {config, centre = c, errTerms = Map.singleton key (mpFloat e)}

mpAffineFromBall :: (Hashable errIdItem) => MPAffineConfig -> errIdItem -> MPBall -> MPAffine
mpAffineFromBall config errIdItem b =
  mpAffine config (ErrorTermId (hash errIdItem), b)

instance ConvertibleExactly (MPAffineConfig, Integer) MPAffine where
  safeConvertExactly :: (MPAffineConfig, Integer) -> ConvertResult MPAffine
  safeConvertExactly (config, n) =
    Right $ mpAffineFromBall config n (mpBallP p n)
    where
      p = prec config.precision

instance ConvertibleExactly (MPAffineConfig, Rational) MPAffine where
  safeConvertExactly :: (MPAffineConfig, Rational) -> ConvertResult MPAffine
  safeConvertExactly (config, q) =
    Right $ mpAffineFromBall config q (mpBallP p q)
    where
      p = prec config.precision