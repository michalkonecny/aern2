{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
    Module      :  AERN2.MP.WithCurrentPrec.Field
    Description :  WithCurrentPrec field operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    WithCurrentPrec field operations
-}
module AERN2.MP.WithCurrentPrec.Field
()
where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import AERN2.MP.Dyadic

-- import qualified Numeric.CollectErrors as CN

import AERN2.MP.WithCurrentPrec.Type

instance
    (CanAddAsymmetric t1 t2, p1~p2)
    =>
    (CanAddAsymmetric (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2)) where
    type AddType (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) = WithCurrentPrec (AddType t1 t2) p1
    add = lift2 add

instance
    (CanSub t1 t2, p1~p2)
    =>
    (CanSub (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2)) where
    type SubType (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) = WithCurrentPrec (SubType t1 t2) p1
    sub = lift2 sub

instance
    (CanMulAsymmetric t1 t2, p1~p2)
    =>
    (CanMulAsymmetric (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2)) where
    type MulType (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) = WithCurrentPrec (MulType t1 t2) p1
    mul = lift2 mul

instance
    (CanDiv t1 t2, p1~p2)
    =>
    (CanDiv (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2)) where
    type DivType (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) = WithCurrentPrec (DivType t1 t2) p1
    divide = lift2 divide


---------------------------------------------------
---------------------------------------------------
-- Integer, Rational etc. mixed-type arithmetic
---------------------------------------------------
---------------------------------------------------

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

    instance
      (CanAddAsymmetric a $t)
      => 
      CanAddAsymmetric (WithCurrentPrec a p) $t
      where
      type AddType (WithCurrentPrec a p) $t = WithCurrentPrec (AddType a $t) p
      add = lift1T add

    instance
      (CanAddAsymmetric a (CN $t))
      => 
      CanAddAsymmetric (WithCurrentPrec a p) (CN $t)
      where
      type AddType (WithCurrentPrec a p) (CN $t) = WithCurrentPrec (AddType a (CN $t)) p
      add = lift1T add

    instance
      (CanAddAsymmetric $t a)
      => 
      CanAddAsymmetric $t (WithCurrentPrec a p)
      where
      type AddType $t (WithCurrentPrec a p) = WithCurrentPrec (AddType $t a) p
      add = liftT1 add

    instance
      (CanAddAsymmetric (CN $t) a)
      => 
      CanAddAsymmetric (CN $t) (WithCurrentPrec a p)
      where
      type AddType (CN $t) (WithCurrentPrec a p) = WithCurrentPrec (AddType (CN $t) a) p
      add = liftT1 add

    instance
      (CanSub a $t)
      => 
      CanSub (WithCurrentPrec a p) $t
      where
      type SubType (WithCurrentPrec a p) $t = WithCurrentPrec (SubType a $t) p
      sub = lift1T sub

    instance
      (CanSub a (CN $t))
      => 
      CanSub (WithCurrentPrec a p) (CN $t)
      where
      type SubType (WithCurrentPrec a p) (CN $t) = WithCurrentPrec (SubType a (CN $t)) p
      sub = lift1T sub

    instance
      (CanSub $t a)
      => 
      CanSub $t (WithCurrentPrec a p)
      where
      type SubType $t (WithCurrentPrec a p) = WithCurrentPrec (SubType $t a) p
      sub = liftT1 sub

    instance
      (CanSub (CN $t) a)
      => 
      CanSub (CN $t) (WithCurrentPrec a p)
      where
      type SubType (CN $t) (WithCurrentPrec a p) = WithCurrentPrec (SubType (CN $t) a) p
      sub = liftT1 sub

    instance
      (CanMulAsymmetric a $t)
      => 
      CanMulAsymmetric (WithCurrentPrec a p) $t
      where
      type MulType (WithCurrentPrec a p) $t = WithCurrentPrec (MulType a $t) p
      mul = lift1T mul

    instance
      (CanMulAsymmetric a (CN $t))
      => 
      CanMulAsymmetric (WithCurrentPrec a p) (CN $t)
      where
      type MulType (WithCurrentPrec a p) (CN $t) = WithCurrentPrec (MulType a (CN $t)) p
      mul = lift1T mul

    instance
      (CanMulAsymmetric $t a)
      => 
      CanMulAsymmetric $t (WithCurrentPrec a p)
      where
      type MulType $t (WithCurrentPrec a p) = WithCurrentPrec (MulType $t a) p
      mul = liftT1 mul

    instance
      (CanMulAsymmetric (CN $t) a)
      => 
      CanMulAsymmetric (CN $t) (WithCurrentPrec a p)
      where
      type MulType (CN $t) (WithCurrentPrec a p) = WithCurrentPrec (MulType (CN $t) a) p
      mul = liftT1 mul

    instance
      (CanDiv a $t)
      => 
      CanDiv (WithCurrentPrec a p) $t
      where
      type DivType (WithCurrentPrec a p) $t = WithCurrentPrec (DivType a $t) p
      divide = lift1T divide

    instance
      (CanDiv a (CN $t))
      => 
      CanDiv (WithCurrentPrec a p) (CN $t)
      where
      type DivType (WithCurrentPrec a p) (CN $t) = WithCurrentPrec (DivType a (CN $t)) p
      divide = lift1T divide

    instance
      (CanDiv $t a)
      => 
      CanDiv $t (WithCurrentPrec a p)
      where
      type DivType $t (WithCurrentPrec a p) = WithCurrentPrec (DivType $t a) p
      divide = liftT1 divide

    instance
      (CanDiv (CN $t) a)
      => 
      CanDiv (CN $t) (WithCurrentPrec a p)
      where
      type DivType (CN $t) (WithCurrentPrec a p) = WithCurrentPrec (DivType (CN $t) a) p
      divide = liftT1 divide

  |]))
