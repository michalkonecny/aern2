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

import Numeric.MixedTypes
-- import qualified Prelude as P

import qualified Control.CollectErrors as CE
import Control.CollectErrors (CollectErrors, EnsureCE, CanEnsureCE)

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
  , CanEnsureCE es (AddType MPBall b)
  , Monoid es)
  =>
  CanAddAsymmetric MPBall (CollectErrors es  b)
  where
  type AddType MPBall (CollectErrors es  b) =
    EnsureCE es (AddType MPBall b)
  add = CE.unlift2first add

instance
  (CanAddAsymmetric a MPBall
  , CanEnsureCE es (AddType a MPBall)
  , Monoid es)
  =>
  CanAddAsymmetric (CollectErrors es a) MPBall
  where
  type AddType (CollectErrors es  a) MPBall =
    EnsureCE es (AddType a MPBall)
  add = CE.unlift2second add

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
  , CanEnsureCE es (SubType MPBall b)
  , Monoid es)
  =>
  CanSub MPBall (CollectErrors es  b)
  where
  type SubType MPBall (CollectErrors es  b) =
    EnsureCE es (SubType MPBall b)
  sub = CE.unlift2first sub

instance
  (CanSub a MPBall
  , CanEnsureCE es (SubType a MPBall)
  , Monoid es)
  =>
  CanSub (CollectErrors es a) MPBall
  where
  type SubType (CollectErrors es  a) MPBall =
    EnsureCE es (SubType a MPBall)
  sub = CE.unlift2second sub

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
  , CanEnsureCE es (MulType MPBall b)
  , Monoid es)
  =>
  CanMulAsymmetric MPBall (CollectErrors es  b)
  where
  type MulType MPBall (CollectErrors es  b) =
    EnsureCE es (MulType MPBall b)
  mul = CE.unlift2first mul

instance
  (CanMulAsymmetric a MPBall
  , CanEnsureCE es (MulType a MPBall)
  , Monoid es)
  =>
  CanMulAsymmetric (CollectErrors es a) MPBall
  where
  type MulType (CollectErrors es  a) MPBall =
    EnsureCE es (MulType a MPBall)
  mul = CE.unlift2second mul


{- division -}

instance CanDiv MPBall MPBall where
  type DivType MPBall MPBall = CollectNumErrors MPBall
  divide (MPBall x1 e1) b2@(MPBall x2 e2)
    | isCertainlyNonZero b2 =
        cn $ normalize $ MPBall x12Up err
    | otherwise =
        noValueNumErrorPotential DivByZero
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

instance CanDiv MPBall Int where
  type DivType MPBall Int = CollectNumErrors MPBall
  divide = convertSecond divide
instance CanDiv Int MPBall where
  type DivType Int MPBall = CollectNumErrors MPBall
  divide = convertFirst divide

instance CanDiv MPBall Integer where
  type DivType MPBall Integer = CollectNumErrors MPBall
  divide = convertSecond divide
instance CanDiv Integer MPBall where
  type DivType Integer MPBall = CollectNumErrors MPBall
  divide = convertFirst divide

instance CanDiv MPBall Dyadic where
  type DivType MPBall Dyadic = CollectNumErrors MPBall
  divide = convertSecond divide
instance CanDiv Dyadic MPBall where
  type DivType Dyadic MPBall = CollectNumErrors MPBall
  divide = convertFirst divide
instance CanDiv Dyadic Dyadic where
  type DivType Dyadic Dyadic = CollectNumErrors MPBall
  divide a b = divide (mpBall a) (mpBall b)

instance CanDiv MPBall Rational where
  type DivType MPBall Rational = CollectNumErrors MPBall
  divide = convertPSecond divide
instance CanDiv Rational MPBall where
  type DivType Rational MPBall = CollectNumErrors MPBall
  divide = convertPFirst divide

instance
  (CanDiv MPBall b
  , CanEnsureCE es (DivType MPBall b)
  , Monoid es)
  =>
  CanDiv MPBall (CollectErrors es  b)
  where
  type DivType MPBall (CollectErrors es  b) =
    EnsureCE es (DivType MPBall b)
  divide = CE.unlift2first divide

instance
  (CanDiv a MPBall
  , CanEnsureCE es (DivType a MPBall)
  , Monoid es)
  =>
  CanDiv (CollectErrors es a) MPBall
  where
  type DivType (CollectErrors es  a) MPBall =
    EnsureCE es (DivType a MPBall)
  divide = CE.unlift2second divide

{- integer power -}

instance CanPow MPBall Integer where
  type PowType MPBall Integer = CollectNumErrors MPBall
  pow = powUsingMulRecip

instance CanPow MPBall Int where
  type PowType MPBall Int = CollectNumErrors MPBall
  pow = powUsingMulRecip

instance
  (CanPow MPBall b
  , CanEnsureCE es (PowType MPBall b)
  , Monoid es)
  =>
  CanPow MPBall (CollectErrors es  b)
  where
  type PowType MPBall (CollectErrors es  b) =
    EnsureCE es (PowType MPBall b)
  pow = CE.unlift2first pow

instance
  (CanPow a MPBall
  , CanEnsureCE es (PowType a MPBall)
  , Monoid es)
  =>
  CanPow (CollectErrors es a) MPBall
  where
  type PowType (CollectErrors es  a) MPBall =
    EnsureCE es (PowType a MPBall)
  pow = CE.unlift2second pow
