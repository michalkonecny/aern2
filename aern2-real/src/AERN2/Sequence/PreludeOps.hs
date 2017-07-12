{-# LANGUAGE CPP #-}
{-|
    Module      :  AERN2.Sequence.PreludeOps
    Description :  Instances of Prelude.Num etc
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Instances of Prelude classes Eq, Ord, Num etc
-}
module AERN2.Sequence.PreludeOps
(
)
where

import MixedTypesNumPrelude
import qualified Prelude as P

import AERN2.Norm

import AERN2.MP.Precision
import AERN2.MP.Enclosure
import AERN2.MP.Dyadic

import AERN2.QA.Protocol

import AERN2.AccuracySG
import AERN2.Sequence.Type
import AERN2.Sequence.Comparison ()
import AERN2.Sequence.Ring ()
import AERN2.Sequence.Field ()
import AERN2.Sequence.Elementary ()

{- Instances of Prelude numerical classes provided for convenient use outside AERN2
   and also because Template Haskell translates (-x) to (Prelude.negate x) -}

instance
  (HasEqCertainly a a)
  =>
  P.Eq (Sequence a)
  where
  a == b
    | aD !==! bD = True
    | aD !/=! bD = False
    | otherwise =
        error "Failed to decide equality of Sequences.  If you switch to MixedTypesNumPrelude instead of Prelude, comparison of Sequences returns Sequence (Maybe Bool) or similar instead of Bool."
    where
    aD = a ? default_acSG
    bD = b ? default_acSG

instance
  (HasEqCertainly a a, HasOrderCertainly a a)
  =>
  P.Ord (Sequence a)
  where
  compare a b
    | aD !==! bD = P.EQ
    | aD !<! bD = P.LT
    | aD !>! bD = P.GT
    | otherwise =
        error "Failed to decide order of Sequences.  If you switch to MixedTypesNumPrelude instead of Prelude, comparison of Sequences returns Sequence (Maybe Bool) or similar instead of Bool."
    where
    aD = a ? default_acSG
    bD = b ? default_acSG

instance
  (Ring a, CanAbsSameType a
  , SuitableForSeq a, CanSetPrecision a, HasNorm (EnsureNoCN a), CanEnsureCN a)
  =>
  P.Num (Sequence a)
  where
  fromInteger = convertExactly
  negate = negate
  (+) = (+)
  (*) = (*)
  abs = abs
  signum = error "Prelude.signum not implemented for Sequence"

instance
  (Field a, CanAbsSameType a, HasDyadics a
  , SuitableForSeq a, SuitableForSeq (EnsureCN a), CanSetPrecision a
  , HasNorm (EnsureNoCN a), CanEnsureCN a
  , CanIntersectCNSameType a)
  =>
  P.Fractional (Sequence a)
  where
  fromRational = convertExactly . dyadic
  recip a =
    case deEnsureCN (recip a) of
      Right r -> r
      Left es -> error $ show es
  (/) = (/!)
