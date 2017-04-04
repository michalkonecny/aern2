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

{- division -}

instance CanDiv MPBall MPBall where
  divide (MPBall x1 e1) b2@(MPBall x2 e2)
    | isNonZero b2 =
        normalize $ MPBall x12Up err
    | otherwise =
        error $ "Division by MPBall that contains 0: " ++ show b2
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
  type DivType MPBall Int = MPBall
  divide = convertSecond divide
instance CanDiv Int MPBall where
  type DivType Int MPBall = MPBall
  divide = convertFirst divide

instance CanDiv MPBall Integer where
  type DivType MPBall Integer = MPBall
  divide = convertSecond divide
instance CanDiv Integer MPBall where
  type DivType Integer MPBall = MPBall
  divide = convertFirst divide

instance CanDiv MPBall Dyadic where
  type DivType MPBall Dyadic = MPBall
  divide = convertSecond divide
instance CanDiv Dyadic MPBall where
  type DivType Dyadic MPBall = MPBall
  divide = convertFirst divide
instance CanDiv Dyadic Dyadic where
  type DivType Dyadic Dyadic = MPBall
  divide a b = divide (mpBall a) (mpBall b)

instance CanDiv MPBall Rational where
  type DivType MPBall Rational = MPBall
  divide = convertPSecond divide
instance CanDiv Rational MPBall where
  type DivType Rational MPBall = MPBall
  divide = convertPFirst divide


{- integer power -}

instance CanPow MPBall Integer where
  pow = powUsingMulRecip

instance CanPow MPBall Int where
  pow = powUsingMulRecip
