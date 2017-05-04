{-# LANGUAGE Arrows, FlexibleContexts, TypeOperators, TypeFamilies, ConstraintKinds #-}
module Tasks.MixedTypesOps where

import Numeric.MixedTypes

import Control.Arrow

import Tasks.PreludeOps (taskLogistic_c)

type HasLogisticOps r =
  (CanMulSameType r,
   CanSub Integer r, SubType Integer r ~ r,
   CanMulBy r Rational)

taskLogistic :: (HasLogisticOps r) => Integer -> r -> r
taskLogistic n x = r
  where
  (Just r) = taskLogisticWithHook n (const Just) x

taskLogisticWithHook ::
  (HasLogisticOps r)
  =>
  Integer -> (Integer -> r -> Maybe r) -> r -> Maybe r
taskLogisticWithHook = taskLogisticWithHookA

taskLogisticWithHookA ::
  (ArrowChoice to, HasLogisticOps r)
  =>
  Integer -> (Integer -> r `to` Maybe r) -> (r `to` Maybe r)
taskLogisticWithHookA n hookA =
  proc r ->
    logisticWithHookA hookA taskLogistic_c n -< (Just r)

logisticWithHook ::
  (HasLogisticOps r)
  =>
  (Integer -> r -> Maybe r) -> Rational -> Integer -> Maybe r -> Maybe r
logisticWithHook = logisticWithHookA

logisticWithHookA ::
  (ArrowChoice to, HasLogisticOps r)
  =>
  (Integer -> r `to` Maybe r) -> Rational -> Integer -> (Maybe r `to` Maybe r)
logisticWithHookA hookA c n =
    foldl1 (<<<) (take (int n) (map step [1..]))
    where
    step i = proc mx ->
      do
      case mx of
        Just x -> hookA i -< c * x * (1 - x)
        Nothing -> returnA -< Nothing
