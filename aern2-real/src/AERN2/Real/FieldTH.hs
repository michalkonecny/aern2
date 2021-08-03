{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  AERN2.Real.FieldTH
    Description :  field operations on CReal
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Field operations on Cauchy Real numbers.
-}
module AERN2.Real.FieldTH
(
-- * field ops (`add`, `sub`, `mul`, `div`) for `CReal -> CReal -> CReal`
-- * field ops for `CReal -> t -> CReal` and `t -> CReal -> CReal` where `t` is `Int`, `Integer`, `Rational`, `Dyadic`
-- * field ops for `CReal -> MPBall -> MPBall` and `CReal -> CN MPBall -> CN MPBall`
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

-- import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.Real.Type
import AERN2.Real.Comparisons ()

{- field operations -}

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |]]
  (\ e -> [d|

  instance 
    (CanPow b $e, HasOrderCertainly b Integer, HasEqCertainly b Integer, CanTestIsIntegerType b)
    =>
    CanPow (CSequence b) $e 
    where
    type PowType (CSequence b) $e = CSequence (PowType b $e)
    pow = lift1T pow
    type PPowType (CSequence b) $e = CSequence (PPowType b $e)
    ppow = lift1T ppow

  |]))

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |]]
  (\ b -> [d|

  instance 
    (CanPow $b e, HasOrderCertainly e Integer, CanTestIsIntegerType e, CanTestInteger e)
    =>
    CanPow $b (CSequence e) 
    where
    type PowType $b (CSequence e) = CSequence (PowType $b e)
    pow = liftT1 pow
    type PPowType $b (CSequence e) = CSequence (PPowType $b e)
    ppow = liftT1 ppow
  |]))

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
      CanAddAsymmetric (CSequence a) $t
      where
      type AddType (CSequence a) $t = CSequence (AddType a $t)
      add = lift1T add

    instance
      (CanAddAsymmetric a $t)
      => 
      CanAddAsymmetric (CSequence a) (CN $t)
      where
      type AddType (CSequence a) (CN $t) = CSequence (AddType a $t)
      add = lift1T add

    instance
      (CanAddAsymmetric $t a)
      => 
      CanAddAsymmetric $t (CSequence a)
      where
      type AddType $t (CSequence a) = CSequence (AddType $t a)
      add = liftT1 add

    instance
      (CanAddAsymmetric $t a)
      => 
      CanAddAsymmetric (CN $t) (CSequence a)
      where
      type AddType (CN $t) (CSequence a) = CSequence (AddType $t a)
      add = liftT1 add

    instance
      (CanSub a $t)
      => 
      CanSub (CSequence a) $t
      where
      type SubType (CSequence a) $t = CSequence (SubType a $t)
      sub = lift1T sub

    instance
      (CanSub a $t)
      => 
      CanSub (CSequence a) (CN $t)
      where
      type SubType (CSequence a) (CN $t) = CSequence (SubType a $t)
      sub = lift1T sub

    instance
      (CanSub $t a)
      => 
      CanSub $t (CSequence a)
      where
      type SubType $t (CSequence a) = CSequence (SubType $t a)
      sub = liftT1 sub

    instance
      (CanSub $t a)
      => 
      CanSub (CN $t) (CSequence a)
      where
      type SubType (CN $t) (CSequence a) = CSequence (SubType $t a)
      sub = liftT1 sub

    instance
      (CanMulAsymmetric a $t, CanGiveUpIfVeryInaccurate (MulType a $t))
      => 
      CanMulAsymmetric (CSequence a) $t
      where
      type MulType (CSequence a) $t = CSequence (MulType a $t)
      mul = lift1T mul

    instance
      (CanMulAsymmetric a $t, CanGiveUpIfVeryInaccurate (MulType a $t))
      => 
      CanMulAsymmetric (CSequence a) (CN $t)
      where
      type MulType (CSequence a) (CN $t) = CSequence (MulType a $t)
      mul = lift1T mul

    instance
      (CanMulAsymmetric $t a, CanGiveUpIfVeryInaccurate (MulType $t a))
      => 
      CanMulAsymmetric $t (CSequence a)
      where
      type MulType $t (CSequence a) = CSequence (MulType $t a)
      mul = liftT1 mul

    instance
      (CanMulAsymmetric $t a, CanGiveUpIfVeryInaccurate (MulType $t a))
      => 
      CanMulAsymmetric (CN $t) (CSequence a)
      where
      type MulType (CN $t) (CSequence a) = CSequence (MulType $t a)
      mul = liftT1 mul

    instance
      (CanDiv a $t)
      => 
      CanDiv (CSequence a) $t
      where
      type DivType (CSequence a) $t = CSequence (DivType a $t)
      divide = lift1T divide

    instance
      (CanDiv a $t)
      => 
      CanDiv (CSequence a) (CN $t)
      where
      type DivType (CSequence a) (CN $t) = CSequence (DivType a $t)
      divide = lift1T divide

    instance
      (CanDiv $t a, CanTestZero a)
      => 
      CanDiv $t (CSequence a)
      where
      type DivType $t (CSequence a) = CSequence (DivType $t a)
      divide = liftT1 divide

    instance
      (CanDiv $t a, CanTestZero a)
      => 
      CanDiv (CN $t) (CSequence a)
      where
      type DivType (CN $t) (CSequence a) = CSequence (DivType $t a)
      divide = liftT1 divide

  |]))

