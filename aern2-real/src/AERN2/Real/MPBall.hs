{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module AERN2.Real.MPBall
    (MPBall(..), getAccuracy, 
     getPrecision, MP.standardPrecisions, MP.Precision, MP.prec, MP.prec2integer,
     isNonZero,
     toIntegerUp, toIntegerDown,
     integer2Ball, integer2BallP,  
     rational2BallP, rationalBall2BallP,
     ball2endpoints, endpoints2Ball,
     piBallP) 
where

import qualified Prelude as P
import AERN2.Real.Operations
import AERN2.Real.Norm

import Math.NumberTheory.Logarithms (integerLog2)

import AERN2.Real.IntegerRational ()
import qualified AERN2.Real.Accuracy as A
import qualified AERN2.Real.ErrorBound as EB
import AERN2.Real.ErrorBound (ErrorBound(..))
import qualified AERN2.Real.MPFloat as MP
import AERN2.Real.MPFloat (MPFloat, Precision)


data MPBall = MPBall { ball_value :: MPFloat, ball_error :: ErrorBound }

instance Ring MPBall
instance Field MPBall
instance CanAddMulScalar MPBall MPBall
instance CanAddMulDivScalar MPBall MPBall
instance CanAddMulScalar MPBall Integer
instance CanAddMulDivScalar MPBall Integer
instance CanAddMulScalar MPBall Rational
instance CanAddMulDivScalar MPBall Rational

instance Show MPBall
    where
    show (MPBall x e) = "[" ++ show x ++ " ± " ++ show e ++ "]"

rationalBall2BallP :: MP.Precision -> (Rational, Rational) -> MPBall 
rationalBall2BallP p (x,e) =
    MPBall xUp (xe + eUp)
    where
    (MPBall xUp xe) = rational2BallP p x
    eUp = EB.rational2ErrorBound e
    
rational2BallP :: MP.Precision -> Rational -> MPBall
rational2BallP p x =
    MPBall xUp (xUp `EB.subMP` xDn)
    where
    xUp = MP.rationalUp p x
    xDn = MP.rationalDown p x

integer2BallP :: MP.Precision -> Integer -> MPBall
integer2BallP p x =
    MPBall xUp (xUp `EB.subMP` xDn)
    where
    xUp = MP.integerUp p x
    xDn = MP.integerDown p x

instance HasIntegers MPBall where
    integer x =
        MPBall xMP (EB.rational2ErrorBound 0.0)
        where
        xMP = integer x
        
integer2Ball :: Integer -> MPBall
integer2Ball = integer

toIntegerUp :: MPBall -> Integer
toIntegerUp x = ceiling $ MP.toRational $ snd $ ball2endpointsMP x
toIntegerDown :: MPBall -> Integer
toIntegerDown x = floor $ MP.toRational $ fst $ ball2endpointsMP x

getAccuracy :: 
    MPBall -> A.Accuracy
getAccuracy (MPBall _ e) = 
    EB.getAccuracy e

getPrecision :: MPBall -> Precision
getPrecision (MPBall x _) =
    MP.getPrecision x

isNonZero :: MPBall -> Bool
isNonZero (MPBall x e) =
    (MP.abs x) `MP.subDown` (EB.er2mp e) > MP.zero


instance HasNorm MPBall where
    getNormLog ball
        | integerBound > 1 = 
            NormBits $ toInteger $ integerLog2 $ integerBound
        | integerRecipBound >= 1 = 
            NormBits  $ 1 + (neg $ toInteger $ integerLog2 $ integerRecipBound)
        | otherwise = NormZero
        where
        ballR =
            endpointsMP2Ball r r
            where
            r = snd $ ball2endpointsMP $ abs ball
        integerBound = toIntegerUp ballR
        integerRecipBound 
            | isNonZero ballR = toIntegerUp (1 / ballR)
            | otherwise = -1

instance HasEq MPBall MPBall where
    type EqCompareType MPBall MPBall = Maybe Bool
    equalTo b1 b2 =
        case (getAccuracy b1, getAccuracy b2, b1 < b2, b2 < b1) of
            (A.Exact, A.Exact, Just False, Just False) -> Just True
            (_, _, Just True, _) -> Just False
            (_, _, _, Just True) -> Just False
            _ -> Nothing
    notEqualTo b1 b2 = fmap not $ equalTo b1 b2
        
instance HasOrder MPBall MPBall where
    type OrderCompareType MPBall MPBall = Maybe Bool
    lessThan (MPBall x1 e1) (MPBall x2 e2) 
        | (x1 `MP.addUp` e1MP) < (x2 `MP.subDown` e2MP) = Just True
        | (x1 `MP.subDown` e1MP) >= (x2 `MP.addUp` e2MP) = Just False
        | otherwise = Nothing
        where
        e1MP = EB.er2mp e1
        e2MP = EB.er2mp e2
    leq (MPBall x1 e1) (MPBall x2 e2) 
        | (x1 `MP.addUp` e1MP) <= (x2 `MP.subDown` e2MP) = Just True
        | (x1 `MP.subDown` e1MP) > (x2 `MP.addUp` e2MP) = Just False
        | otherwise = Nothing
        where
        e1MP = EB.er2mp e1
        e2MP = EB.er2mp e2

instance HasEq MPBall Integer where
    type EqCompareType MPBall Integer = Maybe Bool
    equalTo b1 n2 = equalTo b1 (integer2Ball n2)
    notEqualTo b1 n2 = notEqualTo b1 (integer2Ball n2)

instance HasEq Integer MPBall where
    type EqCompareType Integer MPBall = Maybe Bool
    equalTo n1 b2 = equalTo (integer2Ball n1) b2
    notEqualTo n1 b2 = notEqualTo (integer2Ball n1) b2

instance HasOrder MPBall Integer where
    type OrderCompareType MPBall Integer = Maybe Bool
    lessThan b1 n2 = lessThan b1 (integer2Ball n2) 
    leq b1 n2 = leq b1 (integer2Ball n2) 

instance HasOrder Integer MPBall where
    type OrderCompareType Integer MPBall = Maybe Bool
    lessThan n1 b2 = lessThan (integer2Ball n1) b2
    leq n1 b2 = leq (integer2Ball n1) b2

instance HasEq MPBall Rational where
    type EqCompareType MPBall Rational = Maybe Bool
    equalTo b1 q2 = equalTo b1 (rational2BallP (getPrecision b1) q2)
    notEqualTo b1 q2 = notEqualTo b1 (rational2BallP (getPrecision b1) q2)

instance HasEq Rational MPBall where
    type EqCompareType Rational MPBall = Maybe Bool
    equalTo q1 b2 = equalTo (rational2BallP (getPrecision b2) q1) b2
    notEqualTo q1 b2 = notEqualTo (rational2BallP (getPrecision b2) q1) b2

instance HasOrder MPBall Rational where
    type OrderCompareType MPBall Rational = Maybe Bool
    lessThan b1 q2 = lessThan b1 (rational2BallP (getPrecision b1) q2) 
    leq b1 q2 = leq b1 (rational2BallP (getPrecision b1) q2) 

instance HasOrder Rational MPBall where
    type OrderCompareType Rational MPBall = Maybe Bool
    lessThan q1 b2 = lessThan (rational2BallP (getPrecision b2) q1) b2
    leq q1 b2 = leq (rational2BallP (getPrecision b2) q1) b2


instance CanNeg MPBall where
    neg (MPBall x1 e1) = MPBall (MP.neg x1) e1

instance CanNegSameType MPBall

instance CanAbs MPBall where
    abs (MPBall x1 e1) = MPBall (MP.abs x1) e1

instance CanAbsSameType MPBall

instance CanRecip MPBall where
    recip b = 1 / b

instance CanRecipSameType MPBall

instance CanAdd MPBall MPBall where
    add (MPBall x1 e1) (MPBall x2 e2) =
        MPBall sumUp ((sumUp `EB.subMP` sumDn) + e1 + e2)
        where
        sumUp = MP.addUp x1 x2
        sumDn = MP.addDown x1 x2

instance CanAddThis MPBall MPBall

instance CanAddSameType MPBall

instance CanSub MPBall MPBall  
        
instance CanSubThis MPBall MPBall

instance CanSubSameType MPBall

instance CanMul MPBall MPBall where
    mul (MPBall x1 e1) (MPBall x2 e2) =
        MPBall x12Up (e12 + e1*(EB.absMP x2) + e2*(EB.absMP x1) + e1*e2)
        where
        x12Up = MP.mulUp x1 x2 
        x12Down = MP.mulDown x1 x2
        e12 = EB.mp2ErrorBound $ x12Up `MP.subUp` x12Down

instance CanMulBy MPBall MPBall

instance CanMulSameType MPBall

instance CanDiv MPBall MPBall where
    div (MPBall x1 e1) b2@(MPBall x2 e2)
        | isNonZero b2 =
            MPBall x12Up err
        | otherwise =
            error $ "Division by MPBall that contains 0: " ++ show b2
        where
        x12Up = MP.divUp x1 x2 
        x12Down = MP.divDown x1 x2
        e12 = EB.mp2ErrorBound $ x12Up `MP.subUp` x12Down
        err =
            ((e12 * (EB.mp2ErrorBound (MP.abs x2))) -- e12 * |x2|
             +
             e1
             +
             (EB.mp2ErrorBound (MP.abs x12Up) * e2) -- e2 * |x|
            ) 
            * 
            (EB.mp2ErrorBound $ MP.recipUp (MP.abs x2 `MP.subDown` (EB.er2mp e2))) 
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


instance CanDivBy MPBall MPBall

instance CanDivSameType MPBall

piBallP :: Precision -> MPBall
piBallP p = MPBall piUp (piUp `EB.subMP` piDown)
    where
    piUp = MP.piUp p 
    piDown = MP.piDown p 

{- Ball-Integer operations -}

instance CanAdd Integer MPBall where
    type AddType Integer MPBall = MPBall
    add a b = (integer2BallP (getPrecision b) a) + b

instance CanSub Integer MPBall

instance CanAdd MPBall Integer where
    type AddType MPBall Integer = MPBall
    add a b = a + (integer2BallP (getPrecision a) b)

instance CanAddThis MPBall Integer

instance CanSub MPBall Integer

instance CanSubThis MPBall Integer

instance CanMul Integer MPBall where
    type MulType Integer MPBall = MPBall
    mul a b = (integer2BallP (getPrecision b) a) * b

instance CanMul MPBall Integer where
    type MulType MPBall Integer = MPBall
    mul a b = a * (integer2BallP (getPrecision a) b)

instance CanMulBy MPBall Integer

instance CanDiv Integer MPBall where
    type DivType Integer MPBall = MPBall
    div a b = (integer2BallP (getPrecision b) a) / b

instance CanDiv MPBall Integer where
    type DivType MPBall Integer = MPBall
    div a b = a / (integer2BallP (getPrecision a) b)

instance CanDivBy MPBall Integer

{- Ball-Rational operations -}

instance CanAdd Rational MPBall where
    type AddType Rational MPBall = MPBall
    add a b = (rational2BallP (getPrecision b) a) + b

instance CanSub Rational MPBall

instance CanAdd MPBall Rational where
    type AddType MPBall Rational = MPBall
    add a b = a + (rational2BallP (getPrecision a) b)

instance CanAddThis MPBall Rational

instance CanSub MPBall Rational

instance CanSubThis MPBall Rational

instance CanMul Rational MPBall where
    type MulType Rational MPBall = MPBall
    mul a b = (rational2BallP (getPrecision b) a) * b

instance CanMul MPBall Rational where
    type MulType MPBall Rational = MPBall
    mul a b = a * (rational2BallP (getPrecision a) b)

instance CanMulBy MPBall Rational

instance CanDiv Rational MPBall where
    type DivType Rational MPBall = MPBall
    div a b = (rational2BallP (getPrecision b) a) / b

instance CanDiv MPBall Rational where
    type DivType MPBall Rational = MPBall
    div a b = a / (rational2BallP (getPrecision a) b)

instance CanDivBy MPBall Rational

{- generic methods for computing real functions from MPFR-approximations -}

{-|
    Computes a real function @f@ from correctly rounded MPFR-approximations and a number @lip@ which is a
    Lipschitz constant for @f@, i.e. @|f(x) - f(y)| <= lip * |x - y|@ for all @x@,@y@.
-}
fromApproxWithLipschitz :: 
    (MPFloat -> MPFloat) {-^ @fDown@: a version of @f@ on MPFloat rounding *downwards* -} -> 
    (MPFloat -> MPFloat) {-^ @fUp@: a version of @f@ on MPFloat rounding *upwards* -} ->
    MPFloat {-^ @lip@ a Lipschitz constant for @f@, @lip > 0@ -} -> 
    (MPBall -> MPBall) {-^ @f@ on MPBall rounding *outwards* -}
fromApproxWithLipschitz fDown fUp lip _x@(MPBall xc xe) = 
    MPBall fxc err
    where
    fxl = fDown xc
    fxu = fUp xc
    (MPBall fxc fxe) = endpointsMP2Ball fxl fxu 
    err = (EB.mp2ErrorBound lip) * xe  +  fxe

{-|
    Computes a *monotone* real function @f@ from correctly rounded MPFR-approximations.
-}
monotoneFromApprox :: 
    (MPFloat -> MPFloat) {-^ @fDown@: a version of @f@ on MPFloat rounding *downwards* -} -> 
    (MPFloat -> MPFloat) {-^ @fUp@: a version of @f@ on MPFloat rounding *upwards* -} -> 
    (MPBall -> MPBall) {-^ @f@ on MPBall rounding *outwards* -}
monotoneFromApprox fDown fUp x = 
    endpointsMP2Ball (fDown l) (fUp u)
    where
    (l,u) = ball2endpointsMP x

endpointsMP2Ball :: MPFloat -> MPFloat -> MPBall
endpointsMP2Ball l u =
    MPBall c e
    where
    c = MP.avgUp l u
    e = EB.mp2ErrorBound $ P.max (MP.distUp c l) (MP.distUp c u)

ball2endpointsMP :: MPBall -> (MPFloat, MPFloat)
ball2endpointsMP x = (l,u)
    where
    c    = ball_value x
    r    = er2mp (ball_error x)
    l   = c `MP.subDown` r
    u   = c `MP.addUp` r

endpoints2Ball :: MPBall -> MPBall -> MPBall
endpoints2Ball l u =
    endpointsMP2Ball lMP uMP
    where
    (lMP, _) = ball2endpointsMP l
    (_, uMP) = ball2endpointsMP u

ball2endpoints :: MPBall -> (MPBall, MPBall)
ball2endpoints x = (l,u)
    where
    l = MPBall lMP (EB.rational2ErrorBound 0.0)
    u = MPBall uMP (EB.rational2ErrorBound 0.0)
    (lMP, uMP) = ball2endpointsMP x
    

{- common functions -}

instance CanSqrt MPBall where
    sqrt x = monotoneFromApprox MP.sqrtDown MP.sqrtUp x     
        
instance CanSineCosine MPBall where
    sin = sinB 1
    cos = cosB 1


sinB :: Integer -> MPBall -> MPBall
sinB i x = 
    fromApproxWithLipschitz MP.sinDown MP.sinUp lip x
    where
    lip
        | i == 0 = MP.one
        | otherwise = snd $ ball2endpointsMP $ abs $ cosB (i - 1) x

cosB :: Integer -> MPBall -> MPBall
cosB i x = 
    fromApproxWithLipschitz MP.cosDown MP.cosUp lip x
    where
    lip
        | i == 0 = MP.one
        | otherwise = snd $ ball2endpointsMP $ abs $ sinB (i - 1) x
