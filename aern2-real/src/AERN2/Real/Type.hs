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
  CauchyRealP, pCR, CauchyRealCNP, pCRCN
  , CauchyRealA, CauchyReal, newCR
  , CauchyRealCNA, CauchyRealCN --, newCRCN
  , realName, realId, realSources, realRename
  , realWithAccuracy, realWithAccuracyA, realsWithAccuracyA
  , convergentList2CauchyRealA
  , seqByPrecision2CauchyRealA
  , CanBeReal, real, CanBeRealA, realA
  , CanBeComplex, complex, CanBeComplexA, complexA
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

-- import qualified Control.CollectErrors as CE
import Control.Arrow
import Text.Printf

import Data.Complex

import AERN2.MP
import AERN2.MP.Dyadic

import AERN2.QA.Protocol
import AERN2.QA.Strategy.CachedUnsafe ()

import AERN2.AccuracySG

import AERN2.Sequence.Type
import AERN2.Sequence.Comparison ()
import AERN2.Sequence.Branching

{- Cauchy real numbers -}

type CauchyRealP = SequenceP MPBall
type CauchyRealCNP = SequenceP (CollectNumErrors MPBall)

pCR :: CauchyRealP
pCR = SequenceP (mpBall 0)

pCRCN :: CauchyRealCNP
pCRCN = SequenceP (cn $ mpBall 0)

type CauchyRealA to = SequenceA to MPBall
type CauchyReal = CauchyRealA (->)

type CauchyRealCNA to = SequenceA to (CollectNumErrors MPBall)
type CauchyRealCN = CauchyRealCNA (->)

realName :: SequenceA to a -> String
realName = seqName

realRename :: (String -> String) -> SequenceA to a -> SequenceA to a
realRename = seqRename

realId :: QA to p -> Maybe (QAId to)
realId = qaId

realSources :: QA to p -> [QAId to]
realSources = qaSources

{-| Get a ball approximation of the real number with at least the specified accuracy.
   (A specialisation of 'qaMakeQuery' for Cauchy reals.) -}
realWithAccuracy :: (QAArrow to) => CauchyRealA to -> AccuracySG `to` MPBall
realWithAccuracy = (?)

realWithAccuracyA :: (QAArrow to) => (CauchyRealA to, AccuracySG) `to` MPBall
realWithAccuracyA = qaMakeQueryA

realsWithAccuracyA :: (QAArrow to) => ([CauchyRealA to], AccuracySG) `to` [MPBall]
realsWithAccuracyA = qaMakeQueryOnManyA

{- constructions -}

newCR :: (QAArrow to) => String -> [AnyProtocolQA to] -> AccuracySG `to` MPBall -> CauchyRealA to
newCR = newSeq (mpBall 0)

convergentList2CauchyRealA :: (QAArrow to) => String -> [MPBall] -> (CauchyRealA to)
convergentList2CauchyRealA = convergentList2SequenceA

seqByPrecision2CauchyRealA :: (QAArrow to) => String -> (Precision -> MPBall) -> (CauchyRealA to)
seqByPrecision2CauchyRealA = seqByPrecision2SequenceA

{- conversions -}

type CanBeRealA to t = ConvertibleExactly t (CauchyRealA to)
type CanBeReal t = CanBeRealA (->) t

real :: (CanBeRealA (->) t) => t -> CauchyReal
real = convertExactly

realA :: (CanBeRealA to t) => t -> CauchyRealA to
realA = convertExactly

type CanBeComplexA to t = ConvertibleExactly t (Complex (CauchyRealA to))
type CanBeComplex t = CanBeComplexA (->) t

complex :: (CanBeComplexA (->) t) => t -> Complex CauchyReal
complex = convertExactly

complexA :: (CanBeComplexA to t) => t -> Complex (CauchyRealA to)
complexA = convertExactly

instance (QAArrow to) => ConvertibleExactly Rational (CauchyRealA to) where
  safeConvertExactly x =
    Right $ newCR (show x) [] (arr makeQ)
    where
    makeQ = seqByPrecision2CauchySeq (flip mpBallP x) . bits

instance ConvertibleWithPrecision CauchyReal MPBall where
  safeConvertP p r =
    Right $ setPrecision p $ r ? (accuracySG $ bits p + 10)

{- examples -}

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
    ab = fromEndpoints (mpBall a) (mpBall b)
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
      noValueNumErrorCertain $
        NumError $
          printf "trisection: function does not have opposite signs on points %s %s" (show a) (show b)
