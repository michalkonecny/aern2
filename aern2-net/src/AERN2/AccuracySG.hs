{-# LANGUAGE Arrows #-}
{-|
    Module      :  AERN2.AccuracySG
    Description :  strict and guide accuracy pairs
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    strict and guide accuracy pairs
-}
module AERN2.AccuracySG
(
  AccuracySG(..), acSG0, default_acSG, accuracySG, bitsS, bitsSG
, accuracySGdefaultTolerance
, CanAdjustToAccuracySG(..)
)
where

import MixedTypesNumPrelude
import qualified Prelude as P

import Control.Arrow

-- import qualified Control.CollectErrors as CE
import Control.CollectErrors (CollectErrors) --, EnsureCE, CanEnsureCE, ensureCE)

import AERN2.MP.Accuracy
import AERN2.MP.Ball

{-| An accuracy specification which includes a soft target "guide" accuracy
    in addition to the usual string accuracy requirement. -}
data AccuracySG =
  AccuracySG { _acStrict :: Accuracy, _acGuide :: Accuracy }
  deriving (P.Eq)

instance Show AccuracySG where
  show (AccuracySG acS acG) =
    "bitsSG " ++ (show $ fromAccuracy acS) ++ " " ++ (show $ fromAccuracy acG)

instance ConvertibleExactly AccuracySG Accuracy where
  safeConvertExactly (AccuracySG acS acG) = Right $ acS `max` acG

accuracySGdefaultTolerance :: Integer
accuracySGdefaultTolerance = 20

accuracySG :: Accuracy -> AccuracySG
accuracySG ac = AccuracySG ac (ac + accuracySGdefaultTolerance)

bitsSG :: Integer -> Integer -> AccuracySG
bitsSG acS acG = AccuracySG (bits acS) (bits acG)

bitsS :: Integer -> AccuracySG
bitsS = accuracySG . bits

acSG0 :: AccuracySG
acSG0 = bitsS 0

default_acSG :: AccuracySG
default_acSG = bitsS 100

instance HasEqAsymmetric AccuracySG AccuracySG
instance HasOrderAsymmetric AccuracySG AccuracySG where
  geq (AccuracySG acS1 acG1) (AccuracySG acS2 acG2) =
    acS1 >= acS2 && acG1 >= acG2
  greaterThan acSG1 acSG2 =
    acSG1 >= acSG2 && acSG1 /= acSG2
  leq = flip geq
  lessThan = flip greaterThan

instance HasOrderAsymmetric Accuracy AccuracySG where
  greaterThan ac (AccuracySG acS acG) =
    ac > acS && ac > acG - accuracySGdefaultTolerance
  geq ac (AccuracySG acS acG) =
    ac >= acS && ac >= acG - accuracySGdefaultTolerance
  leq ac (AccuracySG acS _acG) =
    ac <= acS
  lessThan ac (AccuracySG acS _acG) =
    ac < acS

instance HasOrderAsymmetric AccuracySG Accuracy where
  greaterThan = flip lessThan
  lessThan = flip greaterThan
  leq = flip leq
  geq = flip geq

instance CanMinMaxAsymmetric AccuracySG AccuracySG where
  min = lift2 min
  max = lift2 max

lift2 ::
  (Accuracy -> Accuracy -> Accuracy)
  -> (AccuracySG -> AccuracySG -> AccuracySG)
lift2 op (AccuracySG acS1 acG1) (AccuracySG acS2 acG2) =
   AccuracySG (acS1 `op` acS2) (acG1 `op` acG2)

instance CanAddAsymmetric AccuracySG Integer where
  add (AccuracySG acS acG) n = AccuracySG (acS + n) (acG + n)
instance CanAddAsymmetric Integer AccuracySG where
  type AddType Integer AccuracySG = AccuracySG
  add = flip add
instance CanSub AccuracySG Integer where


class CanAdjustToAccuracySG t where
  adjustToAccuracySG :: AccuracySG -> t -> t

instance CanAdjustToAccuracySG MPBall where
  adjustToAccuracySG (AccuracySG acS acG) =
    setPrecisionAtLeastAccuracy acS . reduceSizeUsingAccuracyGuide acG

instance CanAdjustToAccuracySG Bool where
  adjustToAccuracySG _ = id

instance CanAdjustToAccuracySG Kleenean where
  adjustToAccuracySG _ = id

instance CanAdjustToAccuracySG t => CanAdjustToAccuracySG (Maybe t) where
  adjustToAccuracySG acSG = fmap (adjustToAccuracySG acSG)

instance CanAdjustToAccuracySG t => CanAdjustToAccuracySG (CollectErrors es t) where
  adjustToAccuracySG acSG = fmap (adjustToAccuracySG acSG)

instance
  (Arrow to, CanUnionAsymmetric e1 e2)
  =>
  CanUnionAsymmetric (to AccuracySG e1) (to AccuracySG e2)
  -- this instance is important for "parallel if"
  where
  type UnionType (to AccuracySG e1) (to AccuracySG e2) =
    to AccuracySG (UnionType e1 e2)
  union xA yA =
    proc ac ->
      do
      x <- xA -< ac
      y <- yA -< ac
      returnA -< union x y
