{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE EmptyDataDecls #-}
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
    HasEqAsymmetric (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) 
    where
    type EqCompareType (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) = EqCompareType t1 t2
    equalTo = lift2P equalTo
    notEqualTo = lift2P notEqualTo

instance
    (HasOrderAsymmetric t1 t2, p1 ~ p2)
    =>
    HasOrderAsymmetric (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) 
    where
    type OrderCompareType (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) = OrderCompareType t1 t2
    greaterThan = lift2P greaterThan
    lessThan = lift2P lessThan
    geq = lift2P geq
    leq = lift2P leq

instance
    (CanAbs t)
    =>
    CanAbs (WithCurrentPrec t p)
    where
    type AbsType (WithCurrentPrec t p) = WithCurrentPrec (AbsType t) p
    abs = lift1 abs

instance
    (CanMinMaxAsymmetric t1 t2, p1 ~ p2)
    =>
    CanMinMaxAsymmetric (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) 
    where
    type MinMaxType (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) = WithCurrentPrec (MinMaxType t1 t2) p1
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
      CanMinMaxAsymmetric (WithCurrentPrec a p) $t
      where
      type MinMaxType (WithCurrentPrec a p) $t = WithCurrentPrec (MinMaxType a $t) p
      min = lift1T min
      max = lift1T max

    instance
      (CanMinMaxAsymmetric a (CN $t))
      => 
      CanMinMaxAsymmetric (WithCurrentPrec a p) (CN $t)
      where
      type MinMaxType (WithCurrentPrec a p) (CN $t) = WithCurrentPrec (MinMaxType a (CN $t)) p
      min = lift1T min
      max = lift1T max

    instance
      (CanMinMaxAsymmetric $t a)
      => 
      CanMinMaxAsymmetric $t (WithCurrentPrec a p)
      where
      type MinMaxType $t (WithCurrentPrec a p) = WithCurrentPrec (MinMaxType $t a) p
      min = liftT1 min
      max = liftT1 max

    instance
      (CanMinMaxAsymmetric (CN $t) a)
      => 
      CanMinMaxAsymmetric (CN $t) (WithCurrentPrec a p)
      where
      type MinMaxType (CN $t) (WithCurrentPrec a p) = WithCurrentPrec (MinMaxType (CN $t) a) p
      min = liftT1 min
      max = liftT1 max

    -- equality

    instance
      (HasEqAsymmetric a $t)
      => 
      HasEqAsymmetric (WithCurrentPrec a p) $t
      where
      type EqCompareType (WithCurrentPrec a p) $t = EqCompareType a $t
      equalTo = lift1TP (==)
      notEqualTo = lift1TP (/=)

    instance
      (HasEqAsymmetric a (CN $t))
      => 
      HasEqAsymmetric (WithCurrentPrec a p) (CN $t)
      where
      type EqCompareType (WithCurrentPrec a p) (CN $t) = EqCompareType a (CN $t)
      equalTo = lift1TP (==)
      notEqualTo = lift1TP (/=)

    instance
      (HasEqAsymmetric $t a)
      =>
      HasEqAsymmetric $t (WithCurrentPrec a p)
      where
      type EqCompareType $t (WithCurrentPrec a p) = EqCompareType $t a
      equalTo = liftT1P (==)
      notEqualTo = liftT1P (/=)

    instance
      (HasEqAsymmetric (CN $t) a)
      =>
      HasEqAsymmetric (CN $t) (WithCurrentPrec a p)
      where
      type EqCompareType (CN $t) (WithCurrentPrec a p) = EqCompareType (CN $t) a
      equalTo = liftT1P (==)
      notEqualTo = liftT1P (/=)

    -- order

    instance
      (HasOrderAsymmetric a $t)
      => 
      HasOrderAsymmetric (WithCurrentPrec a p) $t
      where
      type OrderCompareType (WithCurrentPrec a p) $t = OrderCompareType a $t
      lessThan = lift1TP lessThan
      greaterThan = lift1TP greaterThan
      leq = lift1TP leq
      geq = lift1TP geq

    instance
      (HasOrderAsymmetric a (CN $t))
      => 
      HasOrderAsymmetric (WithCurrentPrec a p) (CN $t)
      where
      type OrderCompareType (WithCurrentPrec a p) (CN $t) = OrderCompareType a (CN $t)
      lessThan = lift1TP lessThan
      greaterThan = lift1TP greaterThan
      leq = lift1TP leq
      geq = lift1TP geq

    instance
      (HasOrderAsymmetric $t a)
      =>
      HasOrderAsymmetric $t (WithCurrentPrec a p)
      where
      type OrderCompareType $t (WithCurrentPrec a p) = OrderCompareType $t a
      lessThan = liftT1P lessThan
      greaterThan = liftT1P greaterThan
      leq = liftT1P leq
      geq = liftT1P geq

    instance
      (HasOrderAsymmetric (CN $t) a)
      =>
      HasOrderAsymmetric (CN $t) (WithCurrentPrec a p)
      where
      type OrderCompareType (CN $t) (WithCurrentPrec a p) = OrderCompareType (CN $t) a
      lessThan = liftT1P lessThan
      greaterThan = liftT1P greaterThan
      leq = liftT1P leq
      geq = liftT1P geq

  |]))
