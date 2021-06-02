{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
    Module      :  AERN2.MP.WithCurrentPrec.Comparisons
    Description :  WithCurrentPrec order relations and operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    WithCurrentPrec order relations and operations
-}
module AERN2.MP.WithCurrentPrec.Comparisons
()
where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

-- import qualified Numeric.CollectErrors as CN

import AERN2.MP.Dyadic

import AERN2.MP.WithCurrentPrec.Type

instance
    (HasEqAsymmetric t1 t2, p1 ~ p2)
    =>
    HasEqAsymmetric (WithCurrentPrec p1 t1) (WithCurrentPrec p2 t2) 
    where
    type EqCompareType (WithCurrentPrec p1 t1) (WithCurrentPrec p2 t2) = EqCompareType t1 t2
    equalTo = lift2P equalTo
    notEqualTo = lift2P notEqualTo

instance
    (HasOrderAsymmetric t1 t2, p1 ~ p2)
    =>
    HasOrderAsymmetric (WithCurrentPrec p1 t1) (WithCurrentPrec p2 t2) 
    where
    type OrderCompareType (WithCurrentPrec p1 t1) (WithCurrentPrec p2 t2) = OrderCompareType t1 t2
    greaterThan = lift2P greaterThan
    lessThan = lift2P lessThan
    geq = lift2P geq
    leq = lift2P leq

instance
    (CanAbs t)
    =>
    CanAbs (WithCurrentPrec p t)
    where
    type AbsType (WithCurrentPrec p t) = WithCurrentPrec p (AbsType t)
    abs = lift1 abs

instance
    (CanMinMaxAsymmetric t1 t2, p1 ~ p2)
    =>
    CanMinMaxAsymmetric (WithCurrentPrec p1 t1) (WithCurrentPrec p2 t2) 
    where
    type MinMaxType (WithCurrentPrec p1 t1) (WithCurrentPrec p2 t2) = WithCurrentPrec p1 (MinMaxType t1 t2)
    min = lift2 min
    max = lift2 max

-- mixed type instances:

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

    -- min, max

    instance
      (CanMinMaxAsymmetric a $t)
      => 
      CanMinMaxAsymmetric (WithCurrentPrec p a) $t
      where
      type MinMaxType (WithCurrentPrec p a) $t = WithCurrentPrec p (MinMaxType a $t)
      min = lift1T min
      max = lift1T max

    instance
      (CanMinMaxAsymmetric a (CN $t))
      => 
      CanMinMaxAsymmetric (WithCurrentPrec p a) (CN $t)
      where
      type MinMaxType (WithCurrentPrec p a) (CN $t) = WithCurrentPrec p (MinMaxType a (CN $t))
      min = lift1T min
      max = lift1T max

    instance
      (CanMinMaxAsymmetric $t a)
      => 
      CanMinMaxAsymmetric $t (WithCurrentPrec p a)
      where
      type MinMaxType $t (WithCurrentPrec p a) = WithCurrentPrec p (MinMaxType $t a)
      min = liftT1 min
      max = liftT1 max

    instance
      (CanMinMaxAsymmetric (CN $t) a)
      => 
      CanMinMaxAsymmetric (CN $t) (WithCurrentPrec p a)
      where
      type MinMaxType (CN $t) (WithCurrentPrec p a) = WithCurrentPrec p (MinMaxType (CN $t) a)
      min = liftT1 min
      max = liftT1 max

    -- equality

    instance
      (HasEqAsymmetric a $t)
      => 
      HasEqAsymmetric (WithCurrentPrec p a) $t
      where
      type EqCompareType (WithCurrentPrec p a) $t = EqCompareType a $t
      equalTo = lift1TP (==)
      notEqualTo = lift1TP (/=)

    instance
      (HasEqAsymmetric a (CN $t))
      => 
      HasEqAsymmetric (WithCurrentPrec p a) (CN $t)
      where
      type EqCompareType (WithCurrentPrec p a) (CN $t) = EqCompareType a (CN $t)
      equalTo = lift1TP (==)
      notEqualTo = lift1TP (/=)

    instance
      (HasEqAsymmetric $t a)
      =>
      HasEqAsymmetric $t (WithCurrentPrec p a)
      where
      type EqCompareType $t (WithCurrentPrec p a) = EqCompareType $t a
      equalTo = liftT1P (==)
      notEqualTo = liftT1P (/=)

    instance
      (HasEqAsymmetric (CN $t) a)
      =>
      HasEqAsymmetric (CN $t) (WithCurrentPrec p a)
      where
      type EqCompareType (CN $t) (WithCurrentPrec p a) = EqCompareType (CN $t) a
      equalTo = liftT1P (==)
      notEqualTo = liftT1P (/=)

    -- order

    instance
      (HasOrderAsymmetric a $t)
      => 
      HasOrderAsymmetric (WithCurrentPrec p a) $t
      where
      type OrderCompareType (WithCurrentPrec p a) $t = OrderCompareType a $t
      lessThan = lift1TP lessThan
      greaterThan = lift1TP greaterThan
      leq = lift1TP leq
      geq = lift1TP geq

    instance
      (HasOrderAsymmetric a (CN $t))
      => 
      HasOrderAsymmetric (WithCurrentPrec p a) (CN $t)
      where
      type OrderCompareType (WithCurrentPrec p a) (CN $t) = OrderCompareType a (CN $t)
      lessThan = lift1TP lessThan
      greaterThan = lift1TP greaterThan
      leq = lift1TP leq
      geq = lift1TP geq

    instance
      (HasOrderAsymmetric $t a)
      =>
      HasOrderAsymmetric $t (WithCurrentPrec p a)
      where
      type OrderCompareType $t (WithCurrentPrec p a) = OrderCompareType $t a
      lessThan = liftT1P lessThan
      greaterThan = liftT1P greaterThan
      leq = liftT1P leq
      geq = liftT1P geq

    instance
      (HasOrderAsymmetric (CN $t) a)
      =>
      HasOrderAsymmetric (CN $t) (WithCurrentPrec p a)
      where
      type OrderCompareType (CN $t) (WithCurrentPrec p a) = OrderCompareType (CN $t) a
      lessThan = liftT1P lessThan
      greaterThan = liftT1P greaterThan
      leq = liftT1P leq
      geq = liftT1P geq

  |]))
