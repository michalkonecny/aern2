{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module AERN2.Num.MPBall
    (MPBall(..), getAccuracy, getFiniteAccuracy,
     getPrecision, MP.standardPrecisions, MP.Precision, MP.prec, MP.prec2integer,
     isNonZero,
     toIntegerUp, toIntegerDown, toRationalUp, toRationalDown,
     integer2Ball, integer2BallP,  
     rational2BallP, rationalBall2BallP,
     ball2endpoints, endpoints2Ball,
     getCentreAndErrorBall,
     piBallP) 
where

import qualified Prelude as P
import AERN2.Num.Operations
import AERN2.Num.Norm

import Math.NumberTheory.Logarithms (integerLog2)

import AERN2.Num.IntegerRational ()
import qualified AERN2.Num.Accuracy as A
import qualified AERN2.Num.ErrorBound as EB
import AERN2.Num.ErrorBound (ErrorBound(..))
import qualified AERN2.Num.MPFloat as MP
import AERN2.Num.MPFloat (MPFloat, Precision)

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const P.id


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

instance HasIntegersA (->) MPBall where
    integerA x =
        MPBall xMP EB.zero
        where
        xMP = integer x
        
integer2Ball :: Integer -> MPBall
integer2Ball = integer

toIntegerUp :: MPBall -> Integer
toIntegerUp x = ceiling $ toRationalUp x
toIntegerDown :: MPBall -> Integer
toIntegerDown x = floor $ toRationalDown x

toRationalUp :: MPBall -> Rational
toRationalUp x = MP.toRational $ snd $ ball2endpointsMP x
toRationalDown :: MPBall -> Rational
toRationalDown x = MP.toRational $ fst $ ball2endpointsMP x

getAccuracy :: 
    MPBall -> A.Accuracy
getAccuracy (MPBall _ e) =
    maybeTrace
    (
        "MPBall.getAccuracy: e = " ++ show e ++ "; ac = " ++ show ac
    )
    ac
    where 
    ac = EB.getAccuracy e

getFiniteAccuracy :: MPBall -> A.Accuracy
getFiniteAccuracy b =
    case getAccuracy b of
        A.Exact -> A.bits $ MP.prec2integer (getPrecision b)
        a -> a

getPrecision :: MPBall -> Precision
getPrecision (MPBall x _) =
    MP.getPrecision x

isNonZero :: MPBall -> Bool
isNonZero (MPBall x e) =
    (MP.abs x) `MP.subDown` (EB.er2mp e) > MP.zero


instance HasNorm MPBall where
    getNormLog ball
        | not (isNonZero ballR) = NormZero
        | integerBound > 1 = 
            NormBits $ toInteger $ integerLog2 $ integerBound
        | integerRecipBound >= 1 = 
            NormBits  $ 1 + (neg $ toInteger $ integerLog2 $ integerRecipBound)
        | otherwise = error "internal error in getNormLog"
        where
        ballR =
            endpointsMP2Ball r r
            where
            r = snd $ ball2endpointsMP $ abs ball
        integerBound = toIntegerUp ballR
        integerRecipBound 
            | isNonZero ballR = toIntegerUp (1 / ballR)
            | otherwise = -1

instance HasEqA (->) MPBall MPBall where
    type EqCompareTypeA (->) MPBall MPBall = Maybe Bool
    equalToA (b1, b2) =
        case (getAccuracy b1, getAccuracy b2, b1 < b2, b2 < b1) of
            (A.Exact, A.Exact, Just False, Just False) -> Just True
            (_, _, Just True, _) -> Just False
            (_, _, _, Just True) -> Just False
            _ -> Nothing
    notEqualToA (b1, b2) = fmap not $ equalTo b1 b2
        
instance HasOrderA (->) MPBall MPBall where
    type OrderCompareTypeA (->) MPBall MPBall = Maybe Bool
    lessThanA (MPBall x1 e1, MPBall x2 e2) 
        | (x1 `MP.addUp` e1MP) < (x2 `MP.subDown` e2MP) = Just True
        | (x1 `MP.subDown` e1MP) >= (x2 `MP.addUp` e2MP) = Just False
        | otherwise = Nothing
        where
        e1MP = EB.er2mp e1
        e2MP = EB.er2mp e2
    leqA (MPBall x1 e1, MPBall x2 e2) 
        | (x1 `MP.addUp` e1MP) <= (x2 `MP.subDown` e2MP) = Just True
        | (x1 `MP.subDown` e1MP) > (x2 `MP.addUp` e2MP) = Just False
        | otherwise = Nothing
        where
        e1MP = EB.er2mp e1
        e2MP = EB.er2mp e2

instance HasEqA (->) MPBall Integer where
    type EqCompareTypeA (->) MPBall Integer = Maybe Bool
    equalToA (b1, n2) = equalTo b1 (integer2Ball n2)
    notEqualToA (b1, n2) = notEqualTo b1 (integer2Ball n2)

instance HasEqA (->) Integer MPBall where
    type EqCompareTypeA (->) Integer MPBall = Maybe Bool
    equalToA (n1, b2) = equalTo (integer2Ball n1) b2
    notEqualToA (n1, b2) = notEqualTo (integer2Ball n1) b2

instance HasOrderA (->) MPBall Integer where
    type OrderCompareTypeA (->) MPBall Integer = Maybe Bool
    lessThanA (b1, n2) = lessThan b1 (integer2Ball n2) 
    leqA (b1, n2) = leq b1 (integer2Ball n2) 

instance HasOrderA (->) Integer MPBall where
    type OrderCompareTypeA (->) Integer MPBall = Maybe Bool
    lessThanA (n1, b2) = lessThan (integer2Ball n1) b2
    leqA (n1, b2) = leq (integer2Ball n1) b2

instance HasEqA (->) MPBall Rational where
    type EqCompareTypeA (->) MPBall Rational = Maybe Bool
    equalToA (b1, q2) = equalTo b1 (rational2BallP (getPrecision b1) q2)
    notEqualToA (b1, q2) = notEqualTo b1 (rational2BallP (getPrecision b1) q2)

instance HasEqA (->) Rational MPBall where
    type EqCompareTypeA (->) Rational MPBall = Maybe Bool
    equalToA (q1, b2) = equalTo (rational2BallP (getPrecision b2) q1) b2
    notEqualToA (q1, b2) = notEqualTo (rational2BallP (getPrecision b2) q1) b2

instance HasOrderA (->) MPBall Rational where
    type OrderCompareTypeA (->) MPBall Rational = Maybe Bool
    lessThanA (b1, q2) = lessThan b1 (rational2BallP (getPrecision b1) q2) 
    leqA (b1, q2) = leq b1 (rational2BallP (getPrecision b1) q2) 

instance HasOrderA (->) Rational MPBall where
    type OrderCompareTypeA (->) Rational MPBall = Maybe Bool
    lessThanA (q1, b2) = lessThan (rational2BallP (getPrecision b2) q1) b2
    leqA (q1, b2) = leq (rational2BallP (getPrecision b2) q1) b2


instance CanNegA (->) MPBall where
    negA (MPBall x1 e1) = MPBall (MP.neg x1) e1

instance CanNegSameType MPBall

instance CanAbsA (->) MPBall where
    absA (MPBall x1 e1) = MPBall (MP.abs x1) e1

instance CanAbsSameType MPBall

instance CanRecipA (->) MPBall where
    recipA b = 1 / b

instance CanRecipSameType MPBall

instance CanAddA (->) MPBall MPBall where
    addA (MPBall x1 e1, MPBall x2 e2) =
        MPBall sumUp ((sumUp `EB.subMP` sumDn) + e1 + e2)
        where
        sumUp = MP.addUp x1 x2
        sumDn = MP.addDown x1 x2

instance CanAddThis MPBall MPBall

instance CanAddSameType MPBall

instance CanSub MPBall MPBall  
        
instance CanSubThis MPBall MPBall

instance CanSubSameType MPBall

instance CanMulA (->) MPBall MPBall where
    mulA (MPBall x1 e1, MPBall x2 e2) =
        MPBall x12Up (e12 + e1*(EB.absMP x2) + e2*(EB.absMP x1) + e1*e2)
        where
        x12Up = MP.mulUp x1 x2 
        x12Down = MP.mulDown x1 x2
        e12 = EB.mp2ErrorBound $ x12Up `MP.subUp` x12Down

instance CanMulBy MPBall MPBall

instance CanMulSameType MPBall

instance CanDivA (->) MPBall MPBall where
    divA (MPBall x1 e1, b2@(MPBall x2 e2))
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

instance CanAddA (->) Integer MPBall where
    type AddTypeA (->) Integer MPBall = MPBall
    addA (a, b) = (integer2BallP (getPrecision b) a) + b

instance CanSub Integer MPBall

instance CanAddA (->) MPBall Integer where
    type AddTypeA (->) MPBall Integer = MPBall
    addA (a, b) = a + (integer2BallP (getPrecision a) b)

instance CanAddThis MPBall Integer

instance CanSub MPBall Integer

instance CanSubThis MPBall Integer

instance CanMulA (->) Integer MPBall where
    type MulTypeA (->) Integer MPBall = MPBall
    mulA (a, b) = (integer2BallP (getPrecision b) a) * b

instance CanMulA (->) MPBall Integer where
    type MulTypeA (->) MPBall Integer = MPBall
    mulA (a, b) = a * (integer2BallP (getPrecision a) b)

instance CanMulBy MPBall Integer

instance CanDivA (->) Integer MPBall where
    type DivTypeA (->) Integer MPBall = MPBall
    divA (a, b) = (integer2BallP (getPrecision b) a) / b

instance CanDivA (->) MPBall Integer where
    type DivTypeA (->) MPBall Integer = MPBall
    divA (a, b) = a / (integer2BallP (getPrecision a) b)

instance CanDivBy MPBall Integer

{- Ball-Rational operations -}

instance CanAddA (->) Rational MPBall where
    type AddTypeA (->) Rational MPBall = MPBall
    addA (a, b) = (rational2BallP (getPrecision b) a) + b

instance CanSub Rational MPBall

instance CanAddA (->) MPBall Rational where
    type AddTypeA (->) MPBall Rational = MPBall
    addA (a, b) = a + (rational2BallP (getPrecision a) b)

instance CanAddThis MPBall Rational

instance CanSub MPBall Rational

instance CanSubThis MPBall Rational

instance CanMulA (->) Rational MPBall where
    type MulTypeA (->) Rational MPBall = MPBall
    mulA (a, b) = (rational2BallP (getPrecision b) a) * b

instance CanMulA (->) MPBall Rational where
    type MulTypeA (->) MPBall Rational = MPBall
    mulA (a, b) = a * (rational2BallP (getPrecision a) b)

instance CanMulBy MPBall Rational

instance CanDivA (->) Rational MPBall where
    type DivTypeA (->) Rational MPBall = MPBall
    divA (a, b) = (rational2BallP (getPrecision b) a) / b

instance CanDivA (->) MPBall Rational where
    type DivTypeA (->) MPBall Rational = MPBall
    divA (a, b) = a / (rational2BallP (getPrecision a) b)

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
    l = MPBall lMP EB.zero
    u = MPBall uMP EB.zero
    (lMP, uMP) = ball2endpointsMP x
    
getCentreAndErrorBall :: MPBall -> (MPBall, MPBall)
getCentreAndErrorBall x = (cB,eB)
    where
    (MPBall cMP eEB) = x
    cB = MPBall cMP EB.zero
    eB = MPBall MP.zero eEB

{- common functions -}

instance CanSqrtA (->) MPBall where
    sqrtA x = monotoneFromApprox MP.sqrtDown MP.sqrtUp x     
        
instance CanExpA (->) MPBall where
    expA x = monotoneFromApprox MP.expDown MP.expUp x     
        
instance CanSineCosineA (->) MPBall where
    sinA = sinB 1
    cosA = cosB 1


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
