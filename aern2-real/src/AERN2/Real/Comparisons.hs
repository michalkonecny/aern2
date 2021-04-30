{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  AERN2.Real.Comparisons
    Description :  comparison  on CReal
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Comparison relations over Cauchy Real numbers.
-}
module AERN2.Real.Comparisons
()
where

import MixedTypesNumPrelude

import Numeric.CollectErrors ( cn, CN )

-- import Data.Complex

import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.Real.Type
import AERN2.Real.CKleenean ()

-- equality:

instance
  (HasEqAsymmetric t1 t2, IsBool (CSequence (EqCompareType t1 t2)))
  => 
  HasEqAsymmetric (CSequence t1) (CSequence t2) 
  where
  type EqCompareType (CSequence t1) (CSequence t2) = CSequence (EqCompareType t1 t2)
  equalTo = lift2 equalTo

-- order:

instance 
  (HasOrderAsymmetric t1 t2, IsBool (CSequence (OrderCompareType t1 t2)))
  => 
  HasOrderAsymmetric (CSequence t1) (CSequence t2) 
  where
  type OrderCompareType (CSequence t1) (CSequence t2) = CSequence (OrderCompareType t1 t2)
  lessThan  = lift2 lessThan
  greaterThan = lift2 greaterThan
  leq = lift2 leq
  geq = lift2 geq

-- abs:

instance
  (CanAbs t1)
  => 
  CanAbs (CSequence t1)
  where
  type AbsType (CSequence t1) = CSequence (AbsType t1)
  abs = lift1 abs

-- min / max:

instance
  (CanMinMaxAsymmetric t1 t2)
  => 
  CanMinMaxAsymmetric (CSequence t1) (CSequence t2) 
  where
  type MinMaxType (CSequence t1) (CSequence t2) = CSequence (MinMaxType t1 t2)
  min = lift2 min
  max = lift2 max

-- mixed type instances:

instance
  (CanMinMaxAsymmetric a MPBall)
  => 
  CanMinMaxAsymmetric (CSequence a) MPBall
  where
  type MinMaxType (CSequence a) MPBall = MinMaxType (CN a) (CN MPBall)
  min s a = min (s ? (getPrecision a)) (cn a)
  max s a = max (s ? (getPrecision a)) (cn a)

instance
  (CanMinMaxAsymmetric a MPBall)
  => 
  CanMinMaxAsymmetric (CSequence a) (CN MPBall)
  where
  type MinMaxType (CSequence a) (CN MPBall) = MinMaxType (CN a) (CN MPBall)
  min s a = min (s ? (getPrecision a)) a
  max s a = max (s ? (getPrecision a)) a

instance
  (CanMinMaxAsymmetric MPBall a)
  => 
  CanMinMaxAsymmetric MPBall (CSequence a)
  where
  type MinMaxType MPBall (CSequence a) = MinMaxType (CN MPBall) (CN a)
  min a s = min (cn a) (s ? (getPrecision a))
  max a s = max (cn a) (s ? (getPrecision a))

instance
  (CanMinMaxAsymmetric MPBall a)
  => 
  CanMinMaxAsymmetric (CN MPBall) (CSequence a)
  where
  type MinMaxType (CN MPBall) (CSequence a) = MinMaxType (CN MPBall) (CN a)
  min a s = min a (s ? (getPrecision a))
  max a s = max a (s ? (getPrecision a))


instance
  (HasEqAsymmetric a MPBall)
  => 
  HasEqAsymmetric (CSequence a) MPBall
  where
  type EqCompareType (CSequence a) MPBall = EqCompareType (CN a) (CN MPBall)
  equalTo s a = equalTo (s ? (getPrecision a)) (cn a)
  notEqualTo s a = notEqualTo (s ? (getPrecision a)) (cn a)

instance
  (HasEqAsymmetric a MPBall)
  => 
  HasEqAsymmetric (CSequence a) (CN MPBall)
  where
  type EqCompareType (CSequence a) (CN MPBall) = EqCompareType (CN a) (CN MPBall)
  equalTo s a = equalTo (s ? (getPrecision a)) a
  notEqualTo s a = notEqualTo (s ? (getPrecision a)) a

instance
  (HasEqAsymmetric MPBall b)
  => 
  HasEqAsymmetric MPBall (CSequence b)
  where
  type EqCompareType MPBall (CSequence b) = EqCompareType (CN MPBall) (CN b)
  equalTo a s = equalTo (cn a) (s ? (getPrecision a))
  notEqualTo a s = notEqualTo (cn a) (s ? (getPrecision a))

instance
  (HasEqAsymmetric MPBall b)
  => 
  HasEqAsymmetric (CN MPBall) (CSequence b)
  where
  type EqCompareType (CN MPBall) (CSequence b) = EqCompareType (CN MPBall) (CN b)
  equalTo a s = equalTo a (s ? (getPrecision a))
  notEqualTo a s = notEqualTo a (s ? (getPrecision a))

instance
  (HasOrderAsymmetric a MPBall)
  => 
  HasOrderAsymmetric (CSequence a) MPBall
  where
  type OrderCompareType (CSequence a) MPBall = OrderCompareType (CN a) (CN MPBall)
  lessThan s a = lessThan (s ? (getPrecision a)) (cn a)
  greaterThan s a = greaterThan (s ? (getPrecision a)) (cn a)
  leq s a = leq (s ? (getPrecision a)) (cn a)
  geq s a = geq (s ? (getPrecision a)) (cn a)

instance
  (HasOrderAsymmetric a MPBall)
  => 
  HasOrderAsymmetric (CSequence a) (CN MPBall)
  where
  type OrderCompareType (CSequence a) (CN MPBall) = OrderCompareType (CN a) (CN MPBall)
  lessThan s a = lessThan (s ? (getPrecision a)) a
  greaterThan s a = greaterThan (s ? (getPrecision a)) a
  leq s a = leq (s ? (getPrecision a)) a
  geq s a = geq (s ? (getPrecision a)) a

instance
  (HasOrderAsymmetric MPBall b)
  => 
  HasOrderAsymmetric MPBall (CSequence b)
  where
  type OrderCompareType MPBall (CSequence b) = OrderCompareType (CN MPBall) (CN b)
  lessThan a s = lessThan (cn a) (s ? (getPrecision a))
  greaterThan a s = greaterThan (cn a) (s ? (getPrecision a))
  leq a s = leq (cn a) (s ? (getPrecision a))
  geq a s = geq (cn a) (s ? (getPrecision a))

instance
  (HasOrderAsymmetric MPBall b)
  => 
  HasOrderAsymmetric (CN MPBall) (CSequence b)
  where
  type OrderCompareType (CN MPBall) (CSequence b) = OrderCompareType (CN MPBall) (CN b)
  lessThan a s = lessThan a (s ? (getPrecision a))
  greaterThan a s = greaterThan a (s ? (getPrecision a))
  leq a s = leq a (s ? (getPrecision a))
  geq a s = geq a (s ? (getPrecision a))

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

    instance
      (CanMinMaxAsymmetric a $t)
      => 
      CanMinMaxAsymmetric (CSequence a) $t
      where
      type MinMaxType (CSequence a) $t = CSequence (MinMaxType a $t)
      min = lift1T min
      max = lift1T max

    instance
      (CanMinMaxAsymmetric a $t)
      => 
      CanMinMaxAsymmetric (CSequence a) (CN $t)
      where
      type MinMaxType (CSequence a) (CN $t) = CSequence (MinMaxType a $t)
      min = lift1T min
      max = lift1T max

    instance
      (CanMinMaxAsymmetric $t a)
      => 
      CanMinMaxAsymmetric $t (CSequence a)
      where
      type MinMaxType $t (CSequence a) = CSequence (MinMaxType $t a)
      min = liftT1 min
      max = liftT1 max

    instance
      (CanMinMaxAsymmetric $t a)
      => 
      CanMinMaxAsymmetric (CN $t) (CSequence a)
      where
      type MinMaxType (CN $t) (CSequence a) = CSequence (MinMaxType $t a)
      min = liftT1 min
      max = liftT1 max

    instance
      (HasEqAsymmetric a $t, IsBool (CSequence (EqCompareType a $t)))
      => 
      HasEqAsymmetric (CSequence a) $t
      where
      type EqCompareType (CSequence a) $t = CSequence (EqCompareType a $t)
      equalTo = lift1T (==)
      notEqualTo = lift1T (/=)

    instance
      (HasEqAsymmetric a $t, IsBool (CSequence (EqCompareType a $t)))
      => 
      HasEqAsymmetric (CSequence a) (CN $t)
      where
      type EqCompareType (CSequence a) (CN $t) = CSequence (EqCompareType a $t)
      equalTo = lift1T (==)
      notEqualTo = lift1T (/=)

    instance
      (HasEqAsymmetric $t a, IsBool (CSequence (EqCompareType $t a)))
      =>
      HasEqAsymmetric $t (CSequence a)
      where
      type EqCompareType $t (CSequence a) = CSequence (EqCompareType $t a)
      equalTo = liftT1 (==)
      notEqualTo = liftT1 (/=)

    instance
      (HasEqAsymmetric $t a, IsBool (CSequence (EqCompareType $t a)))
      =>
      HasEqAsymmetric (CN $t) (CSequence a)
      where
      type EqCompareType (CN $t) (CSequence a) = CSequence (EqCompareType $t a)
      equalTo = liftT1 (==)
      notEqualTo = liftT1 (/=)

    instance
      (HasOrderAsymmetric a $t, IsBool (CSequence (OrderCompareType a $t)))
      => 
      HasOrderAsymmetric (CSequence a) $t
      where
      type OrderCompareType (CSequence a) $t = CSequence (OrderCompareType a $t)
      lessThan = lift1T lessThan
      greaterThan = lift1T greaterThan
      leq = lift1T leq
      geq = lift1T geq

    instance
      (HasOrderAsymmetric a $t, IsBool (CSequence (OrderCompareType a $t)))
      => 
      HasOrderAsymmetric (CSequence a) (CN $t)
      where
      type OrderCompareType (CSequence a) (CN $t) = CSequence (OrderCompareType a $t)
      lessThan = lift1T lessThan
      greaterThan = lift1T greaterThan
      leq = lift1T leq
      geq = lift1T geq

    instance
      (HasOrderAsymmetric $t a, IsBool (CSequence (OrderCompareType $t a)))
      =>
      HasOrderAsymmetric $t (CSequence a)
      where
      type OrderCompareType $t (CSequence a) = CSequence (OrderCompareType $t a)
      lessThan = liftT1 lessThan
      greaterThan = liftT1 greaterThan
      leq = liftT1 leq
      geq = liftT1 geq

    instance
      (HasOrderAsymmetric $t a, IsBool (CSequence (OrderCompareType $t a)))
      =>
      HasOrderAsymmetric (CN $t) (CSequence a)
      where
      type OrderCompareType (CN $t) (CSequence a) = CSequence (OrderCompareType $t a)
      lessThan = liftT1 lessThan
      greaterThan = liftT1 greaterThan
      leq = liftT1 leq
      geq = liftT1 geq

  |]))
