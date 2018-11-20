{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  AERN2.MP.Ball.Field
    Description :  Field operations on arbitrary precision dyadic balls
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Field operations on arbitrary precision dyadic balls
-}
module AERN2.MP.Ball.Field
()
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

import Control.CollectErrors

import AERN2.Normalize

import AERN2.MP.Dyadic (Dyadic)
import AERN2.MP.Float (mpFloat)
import AERN2.MP.Float.Operators
import AERN2.MP.Precision
import qualified AERN2.MP.ErrorBound as EB

import AERN2.MP.Ball.Type
import AERN2.MP.Ball.Conversions ()
import AERN2.MP.Ball.Comparisons ()

{- addition -}

instance CanAddAsymmetric MPBall MPBall where
  type AddType MPBall MPBall = MPBall
  add (MPBall x1 e1) (MPBall x2 e2) =
    normalize $ MPBall sumUp ((sumUp `EB.subMP` sumDn) + e1 + e2)
    where
    sumUp = x1 +^ x2
    sumDn = x1 +. x2

instance CanAddAsymmetric MPBall Int where
  type AddType MPBall Int = MPBall
  add = convertSecond add
instance CanAddAsymmetric Int MPBall where
  type AddType Int MPBall = MPBall
  add = convertFirst add

instance CanAddAsymmetric MPBall Integer where
  type AddType MPBall Integer = MPBall
  add = convertSecond add
instance CanAddAsymmetric Integer MPBall where
  type AddType Integer MPBall = MPBall
  add = convertFirst add

instance CanAddAsymmetric MPBall Dyadic where
  type AddType MPBall Dyadic = MPBall
  add = convertSecond add
instance CanAddAsymmetric Dyadic MPBall where
  type AddType Dyadic MPBall = MPBall
  add = convertFirst add

instance CanAddAsymmetric MPBall Rational where
  type AddType MPBall Rational = MPBall
  add = convertPSecond add
instance CanAddAsymmetric Rational MPBall where
  type AddType Rational MPBall = MPBall
  add = convertPFirst add

instance
  (CanAddAsymmetric MPBall b
  , CanEnsureCE es b
  , CanEnsureCE es (AddType MPBall b)
  , SuitableForCE es)
  =>
  CanAddAsymmetric MPBall (CollectErrors es  b)
  where
  type AddType MPBall (CollectErrors es  b) =
    EnsureCE es (AddType MPBall b)
  add = lift2TLCE add

instance
  (CanAddAsymmetric a MPBall
  , CanEnsureCE es a
  , CanEnsureCE es (AddType a MPBall)
  , SuitableForCE es)
  =>
  CanAddAsymmetric (CollectErrors es a) MPBall
  where
  type AddType (CollectErrors es  a) MPBall =
    EnsureCE es (AddType a MPBall)
  add = lift2TCE add

{- subtraction -}

instance CanSub MPBall MPBall

instance CanSub MPBall Integer
instance CanSub Integer MPBall

instance CanSub MPBall Int
instance CanSub Int MPBall

instance CanSub MPBall Rational
instance CanSub Rational MPBall

instance CanSub MPBall Dyadic
instance CanSub Dyadic MPBall

instance
  (CanSub MPBall b
  , CanEnsureCE es b
  , CanEnsureCE es (SubType MPBall b)
  , SuitableForCE es)
  =>
  CanSub MPBall (CollectErrors es  b)
  where
  type SubType MPBall (CollectErrors es  b) =
    EnsureCE es (SubType MPBall b)
  sub = lift2TLCE sub

instance
  (CanSub a MPBall
  , CanEnsureCE es a
  , CanEnsureCE es (SubType a MPBall)
  , SuitableForCE es)
  =>
  CanSub (CollectErrors es a) MPBall
  where
  type SubType (CollectErrors es  a) MPBall =
    EnsureCE es (SubType a MPBall)
  sub = lift2TCE sub

{- multiplication -}

instance CanMulAsymmetric MPBall MPBall where
  mul (MPBall x1 e1) (MPBall x2 e2) =
    normalize $ MPBall x12Up (e12 + e1*(abs x2) + e2*(abs x1) + e1*e2)
      -- the mixed operations above automatically convert
      -- MPFloat to ErrorBound, checking non-negativity
    where
    x12Up = x1 *^ x2
    x12Down = x1 *. x2
    e12 = x12Up -^ x12Down

instance CanMulAsymmetric MPBall Int where
  type MulType MPBall Int = MPBall
  mul = convertSecond mul
instance CanMulAsymmetric Int MPBall where
  type MulType Int MPBall = MPBall
  mul = convertFirst mul

instance CanMulAsymmetric MPBall Integer where
  type MulType MPBall Integer = MPBall
  mul = convertSecond mul
instance CanMulAsymmetric Integer MPBall where
  type MulType Integer MPBall = MPBall
  mul = convertFirst mul

instance CanMulAsymmetric MPBall Dyadic where
  type MulType MPBall Dyadic = MPBall
  mul = convertSecond mul
instance CanMulAsymmetric Dyadic MPBall where
  type MulType Dyadic MPBall = MPBall
  mul = convertFirst mul

instance CanMulAsymmetric MPBall Rational where
  type MulType MPBall Rational = MPBall
  mul = convertPSecond mul
instance CanMulAsymmetric Rational MPBall where
  type MulType Rational MPBall = MPBall
  mul = convertPFirst mul

instance
  (CanMulAsymmetric MPBall b
  , CanEnsureCE es b
  , CanEnsureCE es (MulType MPBall b)
  , SuitableForCE es)
  =>
  CanMulAsymmetric MPBall (CollectErrors es  b)
  where
  type MulType MPBall (CollectErrors es  b) =
    EnsureCE es (MulType MPBall b)
  mul = lift2TLCE mul

instance
  (CanMulAsymmetric a MPBall
  , CanEnsureCE es a
  , CanEnsureCE es (MulType a MPBall)
  , SuitableForCE es)
  =>
  CanMulAsymmetric (CollectErrors es a) MPBall
  where
  type MulType (CollectErrors es  a) MPBall =
    EnsureCE es (MulType a MPBall)
  mul = lift2TCE mul


{- division -}

instance CanDiv MPBall MPBall where
  type DivTypeNoCN MPBall MPBall = MPBall
  divideNoCN b1 b2 = (~!) (divide b1 b2)
  type DivType MPBall MPBall = CN MPBall
  divide (MPBall x1 e1) b2@(MPBall x2 e2)
    | isCertainlyNonZero b2 =
        cn $ normalize $ MPBall x12Up err
    | isCertainlyZero b2 =
        noValueNumErrorCertainCN DivByZero
    | otherwise =
        noValueNumErrorPotentialCN DivByZero
    where
    x12Up = x1 /^ x2
    x12Down = x1 /. x2
    x12AbsUp = (abs x12Up) `max` (abs x12Down)
    e12 = x12Up -^ x12Down
    err =
        ((e12 *^ (abs x2)) -- e12 * |x2|
         +
         e1
         +
         (e2 * x12AbsUp) -- e2 * |x|
        )
        *
        ((mpFloat 1) /^ ((abs x2) -. (mpFloat e2)))
            -- 1/(|x2| - e2) rounded upwards
{-
A derivation of the above formula for an upper bound on the error:

    * e =
        * = max ( (x1 ± e1) / (x2 ± e2) - x )
        * = max ( ( x1 ± e1 - (x*(x2 ± e2) ) / (x2 ± e2) )
        * ≤ max ( ( x1 ± e1 - ((x1/x2) ± e12)x2 ± x*e2 ) / (x2 ± e2) )
        * = max ( ( x1 ± e1 - x1 ± e12*x2 ± x*e2 ) / (x2 ± e2) )
        * = max ( ( ± e1 ± e12*x2 ± x*e2 ) / (x2 ± e2) )
        * ≤ (e1 + e12*|x2| + |x|*e2 ) / (|x2| - e2)
        * ≤ (e1 +^ e12*^|x2| +^ |x|*^e2 ) /^ (|x2| -. e2)
-}

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Dyadic |]]
  (\ t -> [d|
    instance CanDiv MPBall $t where
      type DivType MPBall $t = CN MPBall
      divide = convertSecond divide
      type DivTypeNoCN MPBall $t = MPBall
      divideNoCN = convertSecond divideNoCN
    instance CanDiv $t MPBall where
      type DivType $t MPBall = CN MPBall
      divide = convertFirst divide
      type DivTypeNoCN $t MPBall = MPBall
      divideNoCN = convertFirst divideNoCN
  |]))

instance CanDiv Dyadic Dyadic where
  type DivTypeNoCN Dyadic Dyadic = MPBall
  divideNoCN a b = divideNoCN (mpBall a) (mpBall b)
  divide a b = divide (mpBall a) (mpBall b)

instance CanDiv MPBall Rational where
  type DivTypeNoCN MPBall Rational = MPBall
  divideNoCN = convertPSecond divideNoCN
  divide = convertPSecond divide
instance CanDiv Rational MPBall where
  type DivTypeNoCN Rational MPBall = MPBall
  divideNoCN = convertPFirst divideNoCN
  divide = convertPFirst divide

instance
  (CanDiv MPBall b
  , CanEnsureCE es b
  , CanEnsureCE es (DivType MPBall b)
  , CanEnsureCE es (DivTypeNoCN MPBall b)
  , SuitableForCE es)
  =>
  CanDiv MPBall (CollectErrors es  b)
  where
  type DivType MPBall (CollectErrors es  b) =
    EnsureCE es (DivType MPBall b)
  divide = lift2TLCE divide
  type DivTypeNoCN MPBall (CollectErrors es  b) =
    EnsureCE es (DivTypeNoCN MPBall b)
  divideNoCN = lift2TLCE divideNoCN

instance
  (CanDiv a MPBall
  , CanEnsureCE es a
  , CanEnsureCE es (DivType a MPBall)
  , CanEnsureCE es (DivTypeNoCN a MPBall)
  , SuitableForCE es)
  =>
  CanDiv (CollectErrors es a) MPBall
  where
  type DivType (CollectErrors es  a) MPBall =
    EnsureCE es (DivType a MPBall)
  divide = lift2TCE divide
  type DivTypeNoCN (CollectErrors es  a) MPBall =
    EnsureCE es (DivTypeNoCN a MPBall)
  divideNoCN = lift2TCE divideNoCN

{- integer power -}

instance CanPow MPBall Integer where
  powNoCN b e = (~!) $ powUsingMulRecip (mpBall 1) b e
  pow = powUsingMulRecip (mpBall 1)

instance CanPow MPBall Int where
  powNoCN b e = (~!) $ powUsingMulRecip (mpBall 1) b e
  pow = powUsingMulRecip (mpBall 1)

instance
  (CanPow MPBall b
  , CanEnsureCE es b
  , CanEnsureCE es (PowType MPBall b)
  , CanEnsureCE es (PowTypeNoCN MPBall b)
  , SuitableForCE es)
  =>
  CanPow MPBall (CollectErrors es  b)
  where
  type PowTypeNoCN MPBall (CollectErrors es  b) =
    EnsureCE es (PowTypeNoCN MPBall b)
  powNoCN = lift2TLCE powNoCN
  type PowType MPBall (CollectErrors es  b) =
    EnsureCE es (PowType MPBall b)
  pow = lift2TLCE pow

instance
  (CanPow a MPBall
  , CanEnsureCE es a
  , CanEnsureCE es (PowType a MPBall)
  , CanEnsureCE es (PowTypeNoCN a MPBall)
  , SuitableForCE es)
  =>
  CanPow (CollectErrors es a) MPBall
  where
  type PowTypeNoCN (CollectErrors es  a) MPBall =
    EnsureCE es (PowTypeNoCN a MPBall)
  powNoCN = lift2TCE powNoCN
  type PowType (CollectErrors es  a) MPBall =
    EnsureCE es (PowType a MPBall)
  pow = lift2TCE pow
