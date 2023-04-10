{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-|
    Module      :  AERN2.Kleenean
    Description :  Three-valued logic
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}
module AERN2.Kleenean
(
    Kleenean(..), kleenean, tKleenean
)
where

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P

import qualified Control.CollectErrors as CE

-- import Test.SmallCheck.Series
import GHC.Generics

import Numeric.MixedTypes.Literals
    ( ConvertibleExactly(..), convertExactly, T(T) )
import Numeric.MixedTypes.Bool
    ( (&&),
      not,
      CanAndOrAsymmetric(..),
      CanNeg(negate),
      CanTestCertainly(..),
      and )

tKleenean :: T Kleenean
tKleenean = T "Kleenean"

data Kleenean = CertainTrue | CertainFalse | TrueOrFalse
  deriving (P.Eq, Show, Generic)

-- instance Serial IO Kleenean

type CanBeKleenean t = ConvertibleExactly t Kleenean
kleenean :: (CanBeKleenean t) => t -> Kleenean
kleenean = convertExactly

instance ConvertibleExactly Kleenean Kleenean where
  safeConvertExactly = Right

instance ConvertibleExactly Bool Kleenean where
  safeConvertExactly True = Right CertainTrue
  safeConvertExactly False = Right CertainFalse

instance CanTestCertainly Kleenean where
  isCertainlyTrue = (P.== CertainTrue)
  isCertainlyFalse = (P.== CertainFalse)

instance CanNeg Kleenean where
  negate CertainTrue = CertainFalse
  negate CertainFalse = CertainTrue
  negate TrueOrFalse = TrueOrFalse

_testNeg1 :: Kleenean
_testNeg1 = not CertainTrue

instance CanAndOrAsymmetric Kleenean Kleenean
  where
  type AndOrType Kleenean Kleenean = Kleenean
  and2 CertainFalse _ = CertainFalse
  and2 CertainTrue CertainTrue = CertainTrue
  and2 _ CertainFalse = CertainFalse
  and2 _ _ = TrueOrFalse
  or2 CertainTrue _ = CertainTrue
  or2 CertainFalse CertainFalse = CertainFalse
  or2 _ CertainTrue = CertainTrue
  or2 _ _ = TrueOrFalse

instance CanAndOrAsymmetric Bool Kleenean
  where
  type AndOrType Bool Kleenean = Kleenean
  and2 b = and2 (kleenean b)
  or2 b = or2 (kleenean b)

instance CanAndOrAsymmetric Kleenean Bool
  where
  type AndOrType Kleenean Bool = Kleenean
  and2 k b = and2 k (kleenean b)
  or2 k b = or2 k (kleenean b)

instance
  (CanAndOrAsymmetric t1 Kleenean, CE.CanBeErrors es)
  =>
  CanAndOrAsymmetric (CE.CollectErrors es t1) Kleenean
  where
  type AndOrType (CE.CollectErrors es t1) Kleenean = CE.CollectErrors es (AndOrType t1 Kleenean)
  and2 = CE.lift1T and2
  or2 = CE.lift1T or2

instance
  (CanAndOrAsymmetric Kleenean t2, CE.CanBeErrors es)
  =>
  CanAndOrAsymmetric Kleenean (CE.CollectErrors es t2)
  where
  type AndOrType Kleenean (CE.CollectErrors es t2) = CE.CollectErrors es (AndOrType Kleenean t2)
  and2 = CE.liftT1 and2
  or2 = CE.liftT1 or2


_testAndOr1 :: Kleenean
_testAndOr1 = TrueOrFalse && False

_testAndOr2 :: Kleenean
_testAndOr2 = and [CertainTrue, TrueOrFalse, CertainFalse]

