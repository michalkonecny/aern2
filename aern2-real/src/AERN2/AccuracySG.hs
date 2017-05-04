{-|
    Module      :  AERN2.AccuracySG
    Description :  The type of Cauchy real numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    The type of Cauchy real numbers
-}
module AERN2.AccuracySG
(
  AccuracySG(..), acSG0, accuracySG, bitsSG
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

import AERN2.MP.Accuracy

{-| An accuracy specification which includes a soft target "guide" accuracy
    in addition to the usual string accuracy requirement. -}
data AccuracySG =
  AccuracySG { _acStrict :: Accuracy, _acGuide :: Accuracy }
  deriving (Show)

accuracySGdefaultTolerance :: Integer
accuracySGdefaultTolerance = 2

accuracySG :: Accuracy -> AccuracySG
accuracySG ac = AccuracySG ac (ac + accuracySGdefaultTolerance)

bitsSG :: Integer -> AccuracySG
bitsSG = accuracySG . bits

acSG0 :: AccuracySG
acSG0 = bitsSG 0

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
