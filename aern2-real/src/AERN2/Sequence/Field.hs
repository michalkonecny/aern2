{-|
    Module      :  AERN2.Sequence.Field
    Description :  field operations on sequences
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Field operations on convergent sequences.
-}
module AERN2.Sequence.Field
(
)
where

import Numeric.MixedTypes hiding (id)
-- import qualified Prelude as P

import Control.Category (id)
import Control.Arrow

import qualified Control.CollectErrors as CE

import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.QA.Protocol
import AERN2.AccuracySG
import AERN2.Sequence.Type
import AERN2.Sequence.Helpers
import AERN2.Sequence.Ring (mulGetInitAC)

{- division -}

instance
  (QAArrow to, CanDiv a b, HasNorm (WithoutCN a), HasNorm (WithoutCN b)
  , SuitableForSeq a, SuitableForSeq b, SuitableForSeq (DivType a b))
  =>
  CanDiv (SequenceA to a) (SequenceA to b)
  where
  type DivType (SequenceA to a) (SequenceA to b) = SequenceA to (DivType a b)
  divide =
    binaryOp "/" divide getInitQ1Q2
    where
    getInitQ1Q2 a1 a2 =
      proc q ->
        do
        -- In a Fractional instance, optimising 3/x and not optimising x/3 etc.
        -- In a Fractional instance, x/3 should be replaced by (1/3)*x etc.
        b1 <- seqWithAccuracy a1 -< q
        let jPre2 = mulGetInitAC b1 q
        b2 <- seqWithAccuracy a2 -< jPre2
        let jInit1 = divGetInitAC1 b2 q
        let jInit2 = divGetInitAC2 b1 b2 q
        returnA -< ((jInit1, Just b1), (jInit2, Just b2))

divGetInitAC1 ::
  (HasNorm (WithoutCN denom), CanEnsureCN denom)
  =>
  denom -> AccuracySG -> AccuracySG
divGetInitAC1 denom acSG =
  case CE.getMaybeValue (ensureCN denom) of
    Nothing -> acSG0
    Just denomNoCN ->
      case getNormLog denomNoCN of
        NormBits denomNL -> max acSG0 (acSG - denomNL)
        NormZero -> acSG0 -- denominator == 0, we have no chance...

divGetInitAC2 ::
  (HasNorm (WithoutCN numer), CanEnsureCN numer
  , HasNorm (WithoutCN denom), CanEnsureCN denom)
  =>
  numer -> denom -> AccuracySG -> AccuracySG
divGetInitAC2 numer denom acSG =
  case (CE.getMaybeValue (ensureCN numer), CE.getMaybeValue (ensureCN denom)) of
    (Just numerNoCN, Just denomNoCN) ->
      case (getNormLog numerNoCN, getNormLog denomNoCN) of
        (_, NormZero) -> acSG0 -- denominator == 0, we have no chance...
        (NormZero, _) -> acSG0 -- numerator == 0, it does not matter
        (NormBits numerNL, NormBits denomNL) -> max acSG0 (acSG + numerNL - 2 * denomNL)
    _ -> acSG0


instance
  (CanDiv a MPBall, SuitableForSeq a
  , CanSetPrecision (DivType a MPBall))
  =>
  CanDiv (Sequence a) MPBall
  where
  type DivType (Sequence a) MPBall = DivType a MPBall
  divide = binaryWithEnclTranslateAC divGetInitAC1 divide

instance
  (CanDiv MPBall b, SuitableForSeq b
  , CanSetPrecision (DivType MPBall b))
  =>
  CanDiv MPBall (Sequence b)
  where
  type DivType MPBall (Sequence b) = DivType MPBall b
  divide numer =
    flip (binaryWithEnclTranslateAC (divGetInitAC2 numer) (flip divide)) numer

divGetInitQ1T ::
  (Arrow to, HasNorm (WithoutCN denom), CanEnsureCN denom)
  =>
  SequenceA to numer -> denom -> AccuracySG `to` (AccuracySG, Maybe numer)
divGetInitQ1T _numerSeq denom =
  arr $ \q -> (divGetInitAC1 denom q, Nothing)

divGetInitQ2T ::
  (QAArrow to
  , HasNorm (WithoutCN numer), CanEnsureCN numer
  , HasNorm (WithoutCN denom), CanEnsureCN denom)
  =>
  numer -> SequenceA to denom -> AccuracySG `to` (AccuracySG, Maybe denom)
divGetInitQ2T numer denomSeq =
  proc q ->
    do
    denom <- seqWithAccuracy denomSeq -< q
    returnA -< (divGetInitAC2 numer denom q, Just denom)

-- instance (QAArrow to) => CanDiv (CauchyRealA to) Integer where
--   type DivType (CauchyRealA to) Integer = CauchyRealA to
--   divide = binaryOpWithPureArg "/" divide divGetInitQ1T
--
-- instance (QAArrow to) => CanDiv Integer (CauchyRealA to) where
--   type DivType Integer (CauchyRealA to) = CauchyRealA to
--   divide = flip $ binaryOpWithPureArg "/" (flip divide) divGetInitQ1TL
--
-- instance (QAArrow to) => CanDiv (CauchyRealA to) Int where
--   type DivType (CauchyRealA to) Int = CauchyRealA to
--   divide = binaryOpWithPureArg "/" divide divGetInitQ1T
--
-- instance (QAArrow to) => CanDiv Int (CauchyRealA to) where
--   type DivType Int (CauchyRealA to) = CauchyRealA to
--   divide = flip $ binaryOpWithPureArg "/" (flip divide) divGetInitQ1TL
--
-- instance (QAArrow to) => CanDiv (CauchyRealA to) Dyadic where
--   type DivType (CauchyRealA to) Dyadic = CauchyRealA to
--   divide = binaryOpWithPureArg "/" divide divGetInitQ1T
--
-- instance (QAArrow to) => CanDiv Dyadic (CauchyRealA to) where
--   type DivType Dyadic (CauchyRealA to) = CauchyRealA to
--   divide = flip $ binaryOpWithPureArg "/" (flip divide) divGetInitQ1TL
--
-- instance (QAArrow to) => CanDiv (CauchyRealA to) Rational where
--   type DivType (CauchyRealA to) Rational = CauchyRealA to
--   divide = binaryOpWithPureArg "/" divide divGetInitQ1T
--
-- instance (QAArrow to) => CanDiv Rational (CauchyRealA to) where
--   type DivType Rational (CauchyRealA to) = CauchyRealA to
--   divide = flip $ binaryOpWithPureArg "/" (flip divide) divGetInitQ1TL
--
{- integer power -}

instance
  (QAArrow to
  , CanEnsureCN a
  , Field (WithoutCN a)
  )
  =>
  CanPow (SequenceA to a) Integer
  where
  type PowType (SequenceA to a) Integer = SequenceA to (EnsureCN a)
  -- pow = powUsingMulRecip
