{-|
    Module      :  AERN2.MP.Ball
    Description :  Arbitrary precision ball arithmetic
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision ball arithmetic
-}
module AERN2.MP.Ball
(
  -- * Auxiliary types
  module AERN2.MP.Precision
  , module AERN2.MP.Accuracy
  , module AERN2.Norm
  -- * The Ball type
  , MPBall(..), CanBeMPBall, mpBall
  , setPrecisionAtLeastAccuracy
  , reducePrecionIfInaccurate
  -- * Ball construction/extraction functions
  , centre, radius
  , centreAndErrorBall
  , endpoints, fromEndpoints
  -- * Ball operations (see also instances)
  , intersect
  , piBallP
  -- * Helpers for constructing ball functions
  , fromApproxWithLipschitz
  , monotoneFromApprox
  , byEndpointsMP
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

import AERN2.Norm
import AERN2.MP.Dyadic (Dyadic)
import qualified AERN2.MP.Float as MPFloat
import AERN2.MP.Float (MPFloat, mpFloat)
import AERN2.MP.Float.Operators
import AERN2.MP.Precision
import AERN2.MP.Accuracy
import qualified AERN2.MP.ErrorBound as EB
import AERN2.MP.ErrorBound (ErrorBound, errorBound)

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace
    | shouldTrace = trace
    | otherwise = const id

_dummy :: ()
_dummy = maybeTrace "dummy" ()

data MPBall = MPBall { ball_value :: MPFloat, ball_error :: ErrorBound }

instance Show MPBall
    where
    show (MPBall x e) = "[" ++ show x ++ " ± " ++ show e ++ "]"

{- ball construction/extraction functions -}

fromEndpointsMP :: MPFloat -> MPFloat -> MPBall
fromEndpointsMP l u =
    MPBall c e
    where
    c = MPFloat.avgUp l u
    e = errorBound $ max (MPFloat.distUp c l) (MPFloat.distUp c u)

endpointsMP :: MPBall -> (MPFloat, MPFloat)
endpointsMP x = (l,u)
    where
    c    = ball_value x
    r    = mpFloat (ball_error x)
    l   = c -. r
    u   = c +^ r

fromEndpoints :: MPBall -> MPBall -> MPBall
fromEndpoints l u =
    fromEndpointsMP lMP uMP
    where
    (lMP, _) = endpointsMP l
    (_, uMP) = endpointsMP u

endpoints :: MPBall -> (MPBall, MPBall)
endpoints x = (l,u)
    where
    l = MPBall lMP (errorBound 0)
    u = MPBall uMP (errorBound 0)
    (lMP, uMP) = endpointsMP x

centreAndErrorBall :: MPBall -> (MPBall, MPBall)
centreAndErrorBall x = (cB,eB)
    where
    (MPBall cMP eEB) = x
    cB = MPBall cMP (errorBound 0)
    eB = MPBall (mpFloat 0) eEB

centre :: MPBall -> MPBall
centre =
    fst . centreAndErrorBall

radius :: MPBall -> MPBall
radius =
    snd . endpoints . snd . centreAndErrorBall

{--- constructing a ball with a given precision ---}

type CanBeMPBallP t = (ConvertWithPrecision t MPBall)

mpBallP :: (CanBeMPBallP t) => Precision -> t -> MPBall
mpBallP = convertP

instance ConvertWithPrecision Integer MPBall where
  safeConvertP p x =
    Right $ MPBall xUp (xUp `EB.subMP` xDn)
    where
    xUp = MPFloat.fromIntegerUp p x
    xDn = MPFloat.fromIntegerDown p x

instance ConvertWithPrecision Int MPBall where
  safeConvertP p = safeConvertP p . integer

instance ConvertWithPrecision Rational MPBall where
  safeConvertP p x =
    Right $ MPBall xUp (xUp `EB.subMP` xDn)
    where
    xUp = MPFloat.fromRationalUp p x
    xDn = MPFloat.fromRationalDown p x

instance ConvertWithPrecision (Rational, Rational) MPBall where
  safeConvertP p (x,e) =
    Right $ MPBall xFlt (xe + eUp)
    where
    (MPBall xFlt xe) = mpBallP p x
    eUp = errorBound e

{--- constructing an exact ball ---}

type CanBeMPBall t = ConvertibleExactly t MPBall

mpBall :: (CanBeMPBall t) => t -> MPBall
mpBall = convertExactly

instance ConvertibleExactly MPBall MPBall where
  safeConvertExactly = Right

instance ConvertibleExactly Dyadic MPBall where
  safeConvertExactly x = Right $ MPBall (convertExactly x) (errorBound 0)

instance ConvertibleExactly Integer MPBall where
  safeConvertExactly x = Right $ MPBall (convertExactly x) (errorBound 0)

instance ConvertibleExactly Int MPBall where
  safeConvertExactly x = Right $ MPBall (convertExactly x) (errorBound 0)

{-- extracting approximate information about a ball --}

instance HasNorm MPBall where
    getNormLog ball = getNormLog boundMP
        where
        (_, MPBall boundMP _) = endpoints $ abs ball

instance HasAccuracy MPBall where
    getAccuracy = getAccuracy . ball_error

instance HasApproximate MPBall where
    type Approximate MPBall = (MPFloat, Bool)
    getApproximate ac b@(MPBall x e) =
        (approx, isAccurate)
        where
        isAccurate = getAccuracy b < ac
        approx
            | closeToN = n
            | otherwise = MPFloat.setPrecisionUp (prec (fromAccuracy ac)) x
            where
            n = mpFloat $ round $ rational x
            closeToN = ((abs $ x -^ n) <= e)

instance HasPrecision MPBall where
    getPrecision  = getPrecision . ball_value

instance CanSetPrecision MPBall where
    setPrecision p (MPBall x e)
        | p >= pPrev = MPBall xUp e
        | otherwise  = MPBall xUp (e + (xUp `EB.subMP` xDown))
        where
        pPrev = MPFloat.getPrecision x
        xUp = MPFloat.setPrecisionUp p x
        xDown = MPFloat.setPrecisionDown p x

{-|
    Change the precision of the ball centre so that
    it is at least as high as the supplied accuracy
    (assuming the accuracy is finite).
-}
setPrecisionAtLeastAccuracy :: Accuracy -> MPBall -> MPBall
setPrecisionAtLeastAccuracy acc b
    | p_b < p_acc = setPrecision p_acc b
    | otherwise = b
    where
    p_acc =
        case acc of
          Exact -> error $ "setPrecisionAtLeastAccuracy: cannot match Exact accuracy"
          NoInformation -> p_b
          _ -> prec $ max 2 (fromAccuracy acc)
    p_b = getPrecision b

{-|
    Reduce the precision of the ball centre if the
    accuracy of the ball is poor.

    More precisely, reduce the precision of the centre
    so that the ulp is approximately (radius / 1024),
    unless the ulp is already lower than this.
-}
reducePrecionIfInaccurate :: MPBall -> MPBall
reducePrecionIfInaccurate b@(MPBall x _) =
    case (acc, norm) of
        (Exact, _) -> b
        (_, NormZero) -> b
        _ | p_e_nb < p_x -> setPrecision p_e_nb b
        _ -> b
    where
    acc = getAccuracy b
    norm = getNormLog b
    p_x = getPrecision x
    p_e_nb = prec $ max 2 (10 + nb + fromAccuracy acc)
    (NormBits nb) = norm

{- comparisons -}

instance HasEqAsymmetric MPBall MPBall where
  type EqCompareType MPBall MPBall = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance HasEqAsymmetric MPBall Integer where
  type EqCompareType MPBall Integer = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2
instance HasEqAsymmetric Integer MPBall where
  type EqCompareType Integer MPBall = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance HasEqAsymmetric MPBall Rational where
  type EqCompareType MPBall Rational = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2
instance HasEqAsymmetric Rational MPBall where
  type EqCompareType Rational MPBall = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance HasEqAsymmetric MPBall Dyadic where
  type EqCompareType MPBall Dyadic = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2
instance HasEqAsymmetric Dyadic MPBall where
  type EqCompareType Dyadic MPBall = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance HasOrderAsymmetric MPBall MPBall where
  type OrderCompareType MPBall MPBall = Maybe Bool
  lessThan b1 b2
    | r1 < l2 = Just True
    | r2 <= l1 = Just False
    | otherwise = Nothing
    where
    (l1, r1) = endpointsMP b1
    (l2, r2) = endpointsMP b2
  leq b1 b2
    | r1 <= l2 = Just True
    | r2 < l1 = Just False
    | otherwise = Nothing
    where
    (l1, r1) = endpointsMP b1
    (l2, r2) = endpointsMP b2

instance HasOrderAsymmetric Integer MPBall where
  type OrderCompareType Integer MPBall = Maybe Bool
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric MPBall Integer where
  type OrderCompareType MPBall Integer = Maybe Bool
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric Int MPBall where
  type OrderCompareType Int MPBall = Maybe Bool
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric MPBall Int where
  type OrderCompareType MPBall Int = Maybe Bool
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric Dyadic MPBall where
  type OrderCompareType Dyadic MPBall = Maybe Bool
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric MPBall Dyadic where
  type OrderCompareType MPBall Dyadic = Maybe Bool
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric MPBall Rational where
  type OrderCompareType MPBall Rational = Maybe Bool
  lessThan b1 q2
    | r1 < l2 = Just True
    | r2 <= l1 = Just False
    | otherwise = Nothing
    where
    (l1, r1) = endpointsMP b1
    l2 = q2
    r2 = q2
  leq b1 q2
    | r1 <= l2 = Just True
    | r2 < l1 = Just False
    | otherwise = Nothing
    where
    (l1, r1) = endpointsMP b1
    l2 = q2
    r2 = q2

instance HasOrderAsymmetric Rational MPBall where
  type OrderCompareType Rational MPBall = Maybe Bool
  lessThan q1 b2
    | r1 < l2 = Just True
    | r2 <= l1 = Just False
    | otherwise = Nothing
    where
    (l2, r2) = endpointsMP b2
    l1 = q1
    r1 = q1
  leq q1 b2
    | r1 <= l2 = Just True
    | r2 < l1 = Just False
    | otherwise = Nothing
    where
    (l2, r2) = endpointsMP b2
    l1 = q1
    r1 = q1

instance CanTestZero MPBall
instance CanTestPosNeg MPBall

instance CanMinMaxAsymmetric MPBall MPBall where
  min = byEndpointsMP min
  max = byEndpointsMP max

instance CanMinMaxAsymmetric MPBall Integer where
  type MinMaxType MPBall Integer = MPBall
  min = convertSecond min
  max = convertSecond max
instance CanMinMaxAsymmetric Integer MPBall where
  type MinMaxType Integer MPBall = MPBall
  min = convertFirst min
  max = convertFirst max

instance CanMinMaxAsymmetric MPBall Int where
  type MinMaxType MPBall Int = MPBall
  min = convertSecond min
  max = convertSecond max
instance CanMinMaxAsymmetric Int MPBall where
  type MinMaxType Int MPBall = MPBall
  min = convertFirst min
  max = convertFirst max

instance CanMinMaxAsymmetric MPBall Dyadic where
  type MinMaxType MPBall Dyadic = MPBall
  min = convertSecond min
  max = convertSecond max
instance CanMinMaxAsymmetric Dyadic MPBall where
  type MinMaxType Dyadic MPBall = MPBall
  min = convertFirst min
  max = convertFirst max

instance CanMinMaxAsymmetric MPBall Rational where
  type MinMaxType MPBall Rational = MPBall
  min = convertPSecond min
  max = convertPSecond max
instance CanMinMaxAsymmetric Rational MPBall where
  type MinMaxType Rational MPBall = MPBall
  min = convertPFirst min
  max = convertPFirst max

{- intersection -}

intersect :: MPBall -> MPBall -> MPBall
intersect a b
  | rL > rR = error $ "intersect: empty intersection: " ++ show a ++ "; " ++ show b
  | otherwise = fromEndpointsMP rL rR
  where
  rL = max aL bL
  rR = min aR bR
  (aL,aR) = endpointsMP a
  (bL,bR) = endpointsMP b

{- negation & abs -}

instance CanNeg MPBall where
  negate (MPBall x e) = MPBall (-x) e

instance CanAbs MPBall where
  abs b
    | l < 0 && 0 < r =
      fromEndpointsMP (mpFloat 0) (max l r)
    | 0 <= l = b
    | otherwise = -b
    where
    (l,r) = endpointsMP b

{- addition -}

instance CanAddAsymmetric MPBall MPBall where
  type AddType MPBall MPBall = MPBall
  add (MPBall x1 e1) (MPBall x2 e2) =
    MPBall sumUp ((sumUp `EB.subMP` sumDn) + e1 + e2)
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

{- multiplication -}

instance CanMulAsymmetric MPBall MPBall where
  mul (MPBall x1 e1) (MPBall x2 e2) =
    MPBall x12Up (e12 + e1*(abs x2) + e2*(abs x1) + e1*e2)
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
        MPBall x12Up err
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

{- trigonometrics -}

piBallP :: Precision -> MPBall
piBallP p = MPBall piUp (piUp `EB.subMP` piDown)
  where
  piUp = MPFloat.piUp p
  piDown = MPFloat.piDown p

instance CanSinCos MPBall where
  sin = sinB 1
  cos = cosB 1

sinB :: Integer -> MPBall -> MPBall
sinB i x =
    fromApproxWithLipschitz MPFloat.sinDown MPFloat.sinUp lip x
    where
    lip
        | i == 0 = mpFloat 1
        | otherwise = snd $ endpointsMP $ abs $ cosB (i - 1) x

cosB :: Integer -> MPBall -> MPBall
cosB i x =
    fromApproxWithLipschitz MPFloat.cosDown MPFloat.cosUp lip x
    where
    lip
        | i == 0 = mpFloat 1
        | otherwise = snd $ endpointsMP $ abs $ sinB (i - 1) x

{- exp, log, power -}

instance CanExp MPBall where
  exp = monotoneFromApprox MPFloat.expDown MPFloat.expUp

instance CanLog MPBall where
  log x
    | x !>! 0 = monotoneFromApprox MPFloat.logDown MPFloat.logUp x
    | otherwise = error $ "MPBall log: cannot establish that the argument is positive: " ++ show x

instance CanPow MPBall MPBall where
  pow = powViaExpLog

instance CanPow MPBall Dyadic where
  pow x q = powViaExpLog x (mpBall q)

instance CanPow MPBall Rational where
  pow x q = powViaExpLog x (mpBallP (getPrecision x) q)

instance CanSqrt MPBall where
  sqrt x
    | x !>=! 0 = aux x
    | x ?>=? 0 = aux (max 0 x)
    | otherwise = error $ "MPBall sqrt: cannot establish that the argument is non-negative: " ++ show x
    where
      aux = monotoneFromApprox MPFloat.sqrtDown MPFloat.sqrtUp


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
    (MPBall fxc fxe) = fromEndpointsMP fxl fxu
    err = (errorBound lip) * xe  +  fxe

{-|
    Computes a *monotone* ball function @f@ from correctly rounded MPFR-approximations.
-}
monotoneFromApprox ::
    (MPFloat -> MPFloat) {-^ @fDown@: a version of @f@ on MPFloat rounding *downwards* -} ->
    (MPFloat -> MPFloat) {-^ @fUp@: a version of @f@ on MPFloat rounding *upwards* -} ->
    (MPBall -> MPBall) {-^ @f@ on MPBall rounding *outwards* -}
monotoneFromApprox fDown fUp x =
    fromEndpointsMP (fDown l) (fUp u)
    where
    (l,u) = endpointsMP x

{-|
  Computes an *increasing* ball fucntion @f@ from *exact* MPFR operations.
-}
byEndpointsMP ::
    (MPFloat -> MPFloat -> MPFloat) ->
    (MPBall -> MPBall -> MPBall)
byEndpointsMP op b1 b2 =
    fromEndpointsMP (l1 `op` l2) (r1 `op` r2)
    where
    (l1,r1) = endpointsMP b1
    (l2,r2) = endpointsMP b2
