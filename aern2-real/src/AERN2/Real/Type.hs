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
  , realName, realId, realSources, realRename
  , realWithAccuracy, realWithAccuracyA, realsWithAccuracyA
  , CauchyRealA, CauchyReal, newCR
  , convergentList2CauchyRealA
  , seqByPrecision2CauchyRealA
  , CanBeReal, real, CanBeRealA, realA
  , CanBeComplex, complex, CanBeComplexA, complexA
  , pickNonZeroRealA
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

import Numeric.CatchingExceptions

import Control.Arrow

import Text.Printf

import Data.Complex

import AERN2.MP.Accuracy
import AERN2.MP.Dyadic
import AERN2.MP.Ball

import AERN2.QA.Protocol

import AERN2.AccuracySG

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

{- Cauchy real numbers -}

data CauchyRealP = CauchyRealP deriving (Show)

instance QAProtocol CauchyRealP where
  type Q CauchyRealP = AccuracySG
  type A CauchyRealP = MPBall
  -- sampleQ _ = NoInformation

pCR :: CauchyRealP
pCR = CauchyRealP

instance QAProtocolCacheable CauchyRealP where
  type QACache CauchyRealP = (Maybe (MPBall, AccuracySG))
  newQACache _ = Nothing
  lookupQACache _ cache acSG@(AccuracySG acS acG) =
    case cache of
      Just (b, AccuracySG _ bAG) | getAccuracy b >= acS && (getAccuracy b >= acG || bAG >= acG - tol) ->
        (Just (setPrecisionAtLeastAccuracy acS $ reduceSizeUsingAccuracyGuide acG b),
         Just (logMsg b))
      Just (b, _) -> (Nothing, Just (logMsg b))
      Nothing -> (Nothing, Just ("cache empty"))
    where
    tol = accuracySGdefaultTolerance
    logMsg b = printf "query: %s; cache: (ac=%s) %s" (show acSG) (show (getAccuracy b))  (show cache)
  updateQACache _ q b Nothing = Just (b,q)
  updateQACache _ q2 b2 (Just (b1,q1)) = Just (b1 `intersect` b2, q1 `max` q2)

type CauchyRealA to = QA to CauchyRealP

realName :: CauchyRealA to -> String
realName = qaName

realRename :: (String -> String) -> CauchyRealA to -> CauchyRealA to
realRename f r = r {  qaName = f (qaName r)  }

realId :: CauchyRealA to -> Maybe (QAId to)
realId = qaId

realSources :: CauchyRealA to -> [QAId to]
realSources = qaSources

{-| Get a ball approximation of the real number with at least the specified accuracy.
   (A specialisation of 'qaMakeQuery' for Cauchy reals.) -}
realWithAccuracy :: (QAArrow to) => CauchyRealA to -> AccuracySG `to` MPBall
realWithAccuracy = (?)

realWithAccuracyA :: (QAArrow to) => (CauchyRealA to, AccuracySG) `to` MPBall
realWithAccuracyA = qaMakeQueryA

realsWithAccuracyA :: (QAArrow to) => ([CauchyRealA to], AccuracySG) `to` [MPBall]
realsWithAccuracyA = qaMakeQueryOnManyA

type CauchyReal = CauchyRealA (->)

instance Show CauchyReal where
  show r = show $ r ? (accuracySG (bits 100))

instance CanTestValid CauchyReal where
  isValid _ = True

{- constructions -}

newCR :: (QAArrow to) => String -> [AnyProtocolQA to] -> AccuracySG `to` MPBall -> CauchyRealA to
newCR name sources makeQ = newQA name sources pCR (AccuracySG NoInformation NoInformation) makeQ

convergentList2CauchyRealA :: (QAArrow to) => String -> [MPBall] -> (CauchyRealA to)
convergentList2CauchyRealA name balls =
  newCR name [] (arr $ convergentList2CauchySeq balls . bits)

seqByPrecision2CauchyRealA :: (QAArrow to) => String -> (Precision -> MPBall) -> (CauchyRealA to)
seqByPrecision2CauchyRealA name byPrec =
  newCR name [] (arr $ seqByPrecision2CauchySeq byPrec . bits)

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


instance (QAArrow to) => ConvertibleExactly Integer (CauchyRealA to) where
  safeConvertExactly x =
    Right $ newCR (show x) [] (arr $ flip setPrecisionAtLeastAccuracy (mpBall x) . bits)

instance (QAArrow to) => ConvertibleExactly Int (CauchyRealA to) where
  safeConvertExactly x =
    Right $ newCR (show x) [] (arr $ flip setPrecisionAtLeastAccuracy (mpBall x) . bits)

instance (QAArrow to) => ConvertibleExactly Dyadic (CauchyRealA to) where
  safeConvertExactly x =
    Right $ newCR (show x) [] (arr $ flip setPrecisionAtLeastAccuracy (mpBall x) . bits)

instance (QAArrow to) => ConvertibleExactly Rational (CauchyRealA to) where
  safeConvertExactly x =
    Right $ newCR (show x) [] (arr makeQ)
    where
    makeQ = seqByPrecision2CauchySeq (flip mpBallP x) . bits

instance ConvertibleWithPrecision CauchyReal MPBall where
  safeConvertP p r =
    Right $ setPrecision p $ r ? (accuracySG $ bits p + 10)

{- non-zero picking -}

{-|
  Given a list @[(a1,b1),(a2,b2),...]@ and assuming that
  at least one of @a1,a2,...@ is non-zero, pick one of them
  and return the corresponding pair @(ai,bi)@.

  If none of @a1,a2,...@ is zero, either throw an exception
  or loop forever.
 -}
pickNonZeroRealA :: (QAArrow to) => [(CauchyRealA to, s)] `to` (CauchyRealA to, s)
pickNonZeroRealA =
  startFromAccuracy (bits 0)
  where
  startFromAccuracy ac =
    proc realsAndS -> do
      balls <- realsWithAccuracyA -< (map fst realsAndS, accuracySG ac)
      let maybeNonZero = pickNonZeroBall $ zip balls realsAndS
      case maybeNonZero of
        Just result -> returnA -< result
        _ -> startFromAccuracy (ac + 1) -< realsAndS
    where
    pickNonZeroBall :: [(MPBall, s)] -> Maybe s
    pickNonZeroBall [] = Nothing
    pickNonZeroBall ((b, r) : rest)
      | isNonZero b = Just r
      | otherwise = pickNonZeroBall rest

instance CanPickNonZero CauchyReal where
  pickNonZero = pickNonZeroRealA
