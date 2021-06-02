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
    (CanAddAsymmetric (WithCurrentPrec p1 t1) (WithCurrentPrec p2 t2)) where
    type AddType (WithCurrentPrec p1 t1) (WithCurrentPrec p2 t2) = WithCurrentPrec p1 (AddType t1 t2)
    add = lift2 add

instance
    (CanSub t1 t2, p1~p2)
    =>
    (CanSub (WithCurrentPrec p1 t1) (WithCurrentPrec p2 t2)) where
    type SubType (WithCurrentPrec p1 t1) (WithCurrentPrec p2 t2) = WithCurrentPrec p1 (SubType t1 t2)
    sub = lift2 sub

instance
    (CanMulAsymmetric t1 t2, p1~p2)
    =>
    (CanMulAsymmetric (WithCurrentPrec p1 t1) (WithCurrentPrec p2 t2)) where
    type MulType (WithCurrentPrec p1 t1) (WithCurrentPrec p2 t2) = WithCurrentPrec p1 (MulType t1 t2)
    mul = lift2 mul

instance
    (CanDiv t1 t2, p1~p2)
    =>
    (CanDiv (WithCurrentPrec p1 t1) (WithCurrentPrec p2 t2)) where
    type DivType (WithCurrentPrec p1 t1) (WithCurrentPrec p2 t2) = WithCurrentPrec p1 (DivType t1 t2)
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
      CanAddAsymmetric (WithCurrentPrec p a) $t
      where
      type AddType (WithCurrentPrec p a) $t = WithCurrentPrec p (AddType a $t)
      add = lift1T add

    instance
      (CanAddAsymmetric a (CN $t))
      => 
      CanAddAsymmetric (WithCurrentPrec p a) (CN $t)
      where
      type AddType (WithCurrentPrec p a) (CN $t) = WithCurrentPrec p (AddType a (CN $t))
      add = lift1T add

    instance
      (CanAddAsymmetric $t a)
      => 
      CanAddAsymmetric $t (WithCurrentPrec p a)
      where
      type AddType $t (WithCurrentPrec p a) = WithCurrentPrec p (AddType $t a)
      add = liftT1 add

    instance
      (CanAddAsymmetric (CN $t) a)
      => 
      CanAddAsymmetric (CN $t) (WithCurrentPrec p a)
      where
      type AddType (CN $t) (WithCurrentPrec p a) = WithCurrentPrec p (AddType (CN $t) a)
      add = liftT1 add

    instance
      (CanSub a $t)
      => 
      CanSub (WithCurrentPrec p a) $t
      where
      type SubType (WithCurrentPrec p a) $t = WithCurrentPrec p (SubType a $t)
      sub = lift1T sub

    instance
      (CanSub a (CN $t))
      => 
      CanSub (WithCurrentPrec p a) (CN $t)
      where
      type SubType (WithCurrentPrec p a) (CN $t) = WithCurrentPrec p (SubType a (CN $t))
      sub = lift1T sub

    instance
      (CanSub $t a)
      => 
      CanSub $t (WithCurrentPrec p a)
      where
      type SubType $t (WithCurrentPrec p a) = WithCurrentPrec p (SubType $t a)
      sub = liftT1 sub

    instance
      (CanSub (CN $t) a)
      => 
      CanSub (CN $t) (WithCurrentPrec p a)
      where
      type SubType (CN $t) (WithCurrentPrec p a) = WithCurrentPrec p (SubType (CN $t) a)
      sub = liftT1 sub

    instance
      (CanMulAsymmetric a $t)
      => 
      CanMulAsymmetric (WithCurrentPrec p a) $t
      where
      type MulType (WithCurrentPrec p a) $t = WithCurrentPrec p (MulType a $t)
      mul = lift1T mul

    instance
      (CanMulAsymmetric a (CN $t))
      => 
      CanMulAsymmetric (WithCurrentPrec p a) (CN $t)
      where
      type MulType (WithCurrentPrec p a) (CN $t) = WithCurrentPrec p (MulType a (CN $t))
      mul = lift1T mul

    instance
      (CanMulAsymmetric $t a)
      => 
      CanMulAsymmetric $t (WithCurrentPrec p a)
      where
      type MulType $t (WithCurrentPrec p a) = WithCurrentPrec p (MulType $t a)
      mul = liftT1 mul

    instance
      (CanMulAsymmetric (CN $t) a)
      => 
      CanMulAsymmetric (CN $t) (WithCurrentPrec p a)
      where
      type MulType (CN $t) (WithCurrentPrec p a) = WithCurrentPrec p (MulType (CN $t) a)
      mul = liftT1 mul

    instance
      (CanDiv a $t)
      => 
      CanDiv (WithCurrentPrec p a) $t
      where
      type DivType (WithCurrentPrec p a) $t = WithCurrentPrec p (DivType a $t)
      divide = lift1T divide

    instance
      (CanDiv a (CN $t))
      => 
      CanDiv (WithCurrentPrec p a) (CN $t)
      where
      type DivType (WithCurrentPrec p a) (CN $t) = WithCurrentPrec p (DivType a (CN $t))
      divide = lift1T divide

    instance
      (CanDiv $t a)
      => 
      CanDiv $t (WithCurrentPrec p a)
      where
      type DivType $t (WithCurrentPrec p a) = WithCurrentPrec p (DivType $t a)
      divide = liftT1 divide

    instance
      (CanDiv (CN $t) a)
      => 
      CanDiv (CN $t) (WithCurrentPrec p a)
      where
      type DivType (CN $t) (WithCurrentPrec p a) = WithCurrentPrec p (DivType (CN $t) a)
      divide = liftT1 divide

  |]))
