{-|
    Module      :  AERN2.MP.Ball.Type
    Description :  Arbitrary precision dyadic balls
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision dyadic balls
-}
module AERN2.MP.Ball.Type
(
  -- * Auxiliary types
  module AERN2.MP.Precision
  , module AERN2.MP.Accuracy
  -- * The Ball type
  , MPBall(..), CanBeMPBall, mpBall, CanBeMPBallP, mpBallP
  , reducePrecionIfInaccurate
  , contains
  -- * Ball construction/extraction functions
  , IsBall(..), makeExactCentre
  , IsInterval(..), intervalFunctionByEndpoints
  , endpointsMP, fromEndpointsMP
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

import GHC.Generics (Generic)

import Numeric.CatchingExceptions (CanTestValid(..))

import AERN2.Normalize

import AERN2.Norm

import AERN2.MP.Dyadic
import qualified AERN2.MP.Float as MPFloat
import AERN2.MP.Float (MPFloat, mpFloat)
import AERN2.MP.Float.Operators
import AERN2.MP.Precision
import AERN2.MP.Accuracy
import qualified AERN2.MP.ErrorBound as EB
import AERN2.MP.ErrorBound (ErrorBound, errorBound)

-- import Debug.Trace (trace)
--
-- shouldTrace :: Bool
-- shouldTrace = False
-- --shouldTrace = True
--
-- maybeTrace :: String -> a -> a
-- maybeTrace
--     | shouldTrace = trace
--     | otherwise = const id
--
-- _dummy :: ()
-- _dummy = maybeTrace "dummy" ()

data MPBall = MPBall { ball_value :: MPFloat, ball_error :: ErrorBound }
  deriving (Generic)

instance Show MPBall
    where
    show (MPBall x e) =
      "[" ++ show x ++ " ± " ++ show e ++ "](prec=" ++ (show $ integer $ getPrecision x) ++ ")"

instance CanTestValid MPBall where
  isValid (MPBall x e) = isFinite x && isFinite (mpFloat e)

instance CanTestFinite MPBall where
  isNaN = not . isValid
  isInfinite = const False

instance CanNormalize MPBall where
  normalize b
    | isValid b =
        b
        -- reducePrecionIfInaccurate b
    | otherwise = error $ "invalid MPBall: " ++ show b

{-|
    Reduce the precision of the ball centre if the
    accuracy of the ball is poor.

    More precisely, reduce the precision of the centre
    so that the ulp is approximately (radius / 1024),
    unless the ulp is already lower than this.
-}
reducePrecionIfInaccurate :: MPBall -> MPBall
reducePrecionIfInaccurate b@(MPBall x _) =
    case (bAcc, bNorm) of
        (Exact, _) -> b
        (_, NormZero) -> b
        _ | p_e_nb < p_x -> setPrecision p_e_nb b
        _ -> b
    where
    bAcc = getAccuracy b
    bNorm = getNormLog b
    p_x = getPrecision x
    p_e_nb = prec $ max 2 (10 + nb + fromAccuracy bAcc)
    (NormBits nb) = bNorm

contains :: MPBall -> MPBall -> Bool
contains (MPBall xLarge eLarge) (MPBall xSmall eSmall) =
  xLargeDy - eLargeDy <= xSmallDy - eSmallDy
  &&
  xSmallDy + eSmallDy <= xLargeDy + eLargeDy
  where
  xLargeDy = dyadic xLarge
  eLargeDy = dyadic eLarge
  xSmallDy = dyadic xSmall
  eSmallDy = dyadic eSmall

{- ball construction/extraction functions -}

fromEndpointsMP :: MPFloat -> MPFloat -> MPBall
fromEndpointsMP = fromEndpoints

endpointsMP :: MPBall -> (MPFloat, MPFloat)
endpointsMP = endpoints

class IsInterval i e where
  fromEndpoints :: e -> e -> i
  endpoints :: i -> (e,e)

{-|
    Computes a *monotone* ball function @f@ on intervals using the interval endpoints.
-}
intervalFunctionByEndpoints ::
  (IsInterval t t, HasEqCertainly t t)
  =>
  (t -> t) {-^ @fThin@: a version of @f@ that works well on thin intervals -} ->
  (t -> t) {-^ @f@ on large intervals rounding *outwards* -}
intervalFunctionByEndpoints fThin x
  | l !==! u = fThin l
  | otherwise = fromEndpoints (fThin l) (fThin u)
  where
  (l,u) = endpoints x

instance IsInterval MPBall MPFloat where
  fromEndpoints l u
    | u < l = fromEndpoints u l
    | otherwise =
      MPBall (mpFloat cDy) (errorBound $ mpFloat eDy)
      where
      lDy = dyadic l
      uDy = dyadic u
      cDy = (lDy + uDy) * 0.5
      eDy = (uDy - cDy) `max` (cDy - lDy)
  endpoints (MPBall x e) = (mpFloat lDy, mpFloat uDy)
      where
      xDy = dyadic x
      eDy = dyadic e
      lDy   = xDy - eDy
      uDy   = xDy + eDy

instance IsInterval MPBall MPBall where
  fromEndpoints l r = -- works as union even when r < l
      fromEndpointsMP lMP uMP
      where
      lMP = min llMP rlMP
      uMP = max luMP ruMP
      (llMP, luMP) = endpointsMP l
      (rlMP, ruMP) = endpointsMP r
  endpoints x = (l,u)
      where
      l = MPBall lMP (errorBound 0)
      u = MPBall uMP (errorBound 0)
      (lMP, uMP) = endpointsMP x

class IsBall t where
  type CentreType t
  centre :: t -> CentreType t
  centreAsBallAndRadius :: t-> (t,ErrorBound)
  centreAsBall :: t -> t
  centreAsBall = fst . centreAsBallAndRadius
  radius :: t -> ErrorBound
  radius = snd . centreAsBallAndRadius
  updateRadius :: (ErrorBound -> ErrorBound) -> (t -> t)

{-|  When the radius of the ball is implicitly contributed to by imprecision in the centre
   (eg if the centre is a polynomial with inexact coefficients), move all that imprecision
   to the explicit radius, making the centre exact.  This may lose some information,
   but as a ball is equivalent to the original.
   For MPBall this function is pointless because it is equivalent to the identity.  -}
makeExactCentre :: (IsBall t) => t -> t
makeExactCentre v =
  updateRadius (+r) c
  where
  (c, r) = centreAsBallAndRadius v

instance IsBall MPBall where
  type CentreType MPBall = Dyadic
  centre (MPBall cMP _e) = dyadic cMP
  centreAsBallAndRadius x = (cB,e)
    where
    (MPBall cMP e) = x
    cB = MPBall cMP (errorBound 0)
  radius (MPBall _ e) = e
  updateRadius updateFn (MPBall c e) = MPBall c (updateFn e)

{--- constructing a ball with a given precision ---}

type CanBeMPBallP t = (ConvertibleWithPrecision t MPBall)

mpBallP :: (CanBeMPBallP t) => Precision -> t -> MPBall
mpBallP = convertP


{--- constructing an exact ball ---}

type CanBeMPBall t = ConvertibleExactly t MPBall

mpBall :: (CanBeMPBall t) => t -> MPBall
mpBall = convertExactly

{-- extracting approximate information about a ball --}

instance HasAccuracy MPBall where
    getAccuracy = getAccuracy . ball_error

instance CanReduceSizeUsingAccuracyGuide MPBall where
  reduceSizeUsingAccuracyGuide acGuide b@(MPBall x _e) =
    lowerPrecisionIfAbove (getPrecision bWithLowAC) b
    where
    bWithLowAC = normalize $
          MPBall x (errorBound $ 0.5^(fromAccuracy acGuide))

instance HasNorm MPBall where
    getNormLog ball = getNormLog boundMP
        where
        (_, MPBall boundMP _) = endpoints $ absRaw ball

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

{- negation & abs -}

instance CanNeg MPBall where
  negate (MPBall x e) = MPBall (-x) e

instance CanAbs MPBall where
  abs = normalize . absRaw

absRaw :: MPBall -> MPBall
absRaw b
  | l < 0 && 0 < r =
    fromEndpointsMP (mpFloat 0) (max (-l) r)
  | 0 <= l = b
  | otherwise = -b
  where
  (l,r) = endpointsMP b
