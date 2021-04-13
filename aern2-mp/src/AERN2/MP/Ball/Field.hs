{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
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

import qualified Control.CollectErrors as CE
import Control.CollectErrors ( CollectErrors, CanBeErrors )
-- import qualified Numeric.CollectErrors as CN
-- import Numeric.CollectErrors (CN, cn)

import AERN2.Normalize

import AERN2.MP.Dyadic (Dyadic)
import qualified AERN2.MP.Float as MPFloat
import AERN2.MP.Float (mpFloat)
import AERN2.MP.Float.Operators
import AERN2.MP.Precision
-- import qualified AERN2.MP.ErrorBound as EB

import AERN2.MP.Ball.Type
import AERN2.MP.Ball.Conversions ()
import AERN2.MP.Ball.Comparisons ()

{- addition -}

instance CanAddAsymmetric MPBall MPBall where
  type AddType MPBall MPBall = MPBall
  add (MPBall x1 e1) (MPBall x2 e2) =
    normalize $ MPBall sumC (e1 + e2 + sumErr)
    where
    (sumC, sumErr) = MPFloat.ceduCentreErr $ MPFloat.addCEDU x1 x2

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
  , CanBeErrors es)
  =>
  CanAddAsymmetric MPBall (CollectErrors es b)
  where
  type AddType MPBall (CollectErrors es b) =
    CollectErrors es (AddType MPBall b)
  add = CE.liftT1 add

instance
  (CanAddAsymmetric a MPBall
  , CanBeErrors es)
  =>
  CanAddAsymmetric (CollectErrors es a) MPBall
  where
  type AddType (CollectErrors es  a) MPBall =
    CollectErrors es (AddType a MPBall)
  add = CE.lift1T add

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
  , CanBeErrors es)
  =>
  CanSub MPBall (CollectErrors es  b)
  where
  type SubType MPBall (CollectErrors es  b) =
    CollectErrors es (SubType MPBall b)
  sub = CE.liftT1 sub

instance
  (CanSub a MPBall
  , CanBeErrors es)
  =>
  CanSub (CollectErrors es a) MPBall
  where
  type SubType (CollectErrors es  a) MPBall =
    CollectErrors es (SubType a MPBall)
  sub = CE.lift1T sub

{- multiplication -}

instance CanMulAsymmetric MPBall MPBall where
  mul (MPBall x1 e1) (MPBall x2 e2) =
    normalize $ MPBall x12C (e12 + e1*(abs x2) + e2*(abs x1) + e1*e2)
      -- the mixed operations above automatically convert
      -- MPFloat to ErrorBound, checking non-negativity
    where
    (x12C, e12) = MPFloat.ceduCentreErr $ MPFloat.mulCEDU x1 x2

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
  , CanBeErrors es)
  =>
  CanMulAsymmetric MPBall (CollectErrors es  b)
  where
  type MulType MPBall (CollectErrors es  b) =
    CollectErrors es (MulType MPBall b)
  mul = CE.liftT1 mul

instance
  (CanMulAsymmetric a MPBall
  , CanBeErrors es)
  =>
  CanMulAsymmetric (CollectErrors es a) MPBall
  where
  type MulType (CollectErrors es  a) MPBall =
    CollectErrors es (MulType a MPBall)
  mul = CE.lift1T mul


{- division -}

instance CanDiv MPBall MPBall where
  type DivType MPBall MPBall = MPBall
  divide (MPBall x1 e1) (MPBall x2 e2) = normalize $ MPBall x12C err
    where
    (x12C, e12) = MPFloat.ceduCentreErr $ MPFloat.divCEDU x1 x2
    x12AbsUp = (abs x12C) +^ e12
    x2abs = abs x2
    err =
        ((e12 *^ x2abs) -- e12 * |x2|
         +
         e1
         +
         (e2 * x12AbsUp) -- e2 * |x|
        )
        *
        ((mpFloat 1) /^ (x2abs -. (mpFloat e2)))
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
      type DivType MPBall $t = MPBall
      divide = convertSecond divide
    instance CanDiv $t MPBall where
      type DivType $t MPBall = MPBall
      divide = convertFirst divide
  |]))

instance CanDiv Dyadic Dyadic where
  type DivType Dyadic Dyadic = MPBall
  divide a b = divide (mpBall a) (mpBall b)

instance CanDiv MPBall Rational where
  type DivType MPBall Rational = MPBall
  divide = convertPSecond divide
instance CanDiv Rational MPBall where
  type DivType Rational MPBall = MPBall
  divide = convertPFirst divide

instance
  (CanDiv MPBall b
  , CanBeErrors es)
  =>
  CanDiv MPBall (CollectErrors es  b)
  where
  type DivType MPBall (CollectErrors es  b) =
    CollectErrors es (DivType MPBall b)
  divide = CE.liftT1 divide

instance
  (CanDiv a MPBall
  , CanBeErrors es)
  =>
  CanDiv (CollectErrors es a) MPBall
  where
  type DivType (CollectErrors es  a) MPBall =
    CollectErrors es (DivType a MPBall)
  divide = CE.lift1T divide

{- integer power -}

instance CanPow MPBall Integer where
  pow = powUsingMulRecipCutNeg (mpBall 1)

instance CanPow MPBall Int where
  pow = powUsingMulRecipCutNeg (mpBall 1)

powUsingMulRecipCutNeg :: _ => t -> t -> e -> DivType Integer t
powUsingMulRecipCutNeg one x e 
  | even e = 
      max 0 $ powUsingMulRecip one x e
  | otherwise = powUsingMulRecip one x e

instance
  (CanPow MPBall b
  , CanBeErrors es)
  =>
  CanPow MPBall (CollectErrors es  b)
  where
  type PowType MPBall (CollectErrors es  b) =
    CollectErrors es (PowType MPBall b)
  pow = CE.liftT1 pow

instance
  (CanPow a MPBall
  , CanBeErrors es)
  =>
  CanPow (CollectErrors es a) MPBall
  where
  type PowType (CollectErrors es  a) MPBall =
    CollectErrors es (PowType a MPBall)
  pow = CE.lift1T pow

instance
  CanDivIMod MPBall MPBall
  where
  type DivIType MPBall MPBall = Integer
  divIMod x m 
    | m !>! 0 = (d, xm)
    | otherwise = error $ "modulus not positive: " ++ show m
    where
    d = floor $ centre $ (centreAsBall x) / (centreAsBall m)
    xm = x - m*d
