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
  , normalise, reducePrecionIfInaccurate
  , setPrecisionAtLeastAccuracy
  , contains
  -- * Ball construction/extraction functions
  , centre, radius
  , centreDyadic
  , centreAndErrorBall
  , endpoints, fromEndpoints
  , endpointsMP, fromEndpointsMP
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

import Numeric.CatchingExceptions (CanTestValid(..))

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

instance Show MPBall
    where
    show (MPBall x e) =
      "[" ++ show x ++ " ± " ++ show e ++ "](prec=" ++ (show $ integer $ getPrecision x) ++ ")"

instance CanTestValid MPBall where
  isValid (MPBall x e) = isFinite x && isFinite (mpFloat e)

normalise :: MPBall -> MPBall
normalise b
  | isValid b = reducePrecionIfInaccurate b
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
fromEndpointsMP l u =
    MPBall (mpFloat cDy) (errorBound $ mpFloat eDy)
    where
    lDy = dyadic l
    uDy = dyadic u
    cDy = (lDy + uDy) * 0.5
    eDy = (uDy - cDy) `max` (cDy - lDy)

endpointsMP :: MPBall -> (MPFloat, MPFloat)
endpointsMP (MPBall x e) = (mpFloat lDy, mpFloat uDy)
    where
    xDy = dyadic x
    eDy = dyadic e
    lDy   = xDy - eDy
    uDy   = xDy + eDy

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

centreDyadic :: MPBall -> Dyadic
centreDyadic (MPBall cMP _eEB) =
    dyadic cMP

radius :: MPBall -> MPBall
radius =
    snd . endpoints . snd . centreAndErrorBall

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

{- negation & abs -}

instance CanNeg MPBall where
  negate (MPBall x e) = MPBall (-x) e

instance CanAbs MPBall where
  abs = normalise . absRaw

absRaw :: MPBall -> MPBall
absRaw b
  | l < 0 && 0 < r =
    fromEndpointsMP (mpFloat 0) (max (-l) r)
  | 0 <= l = b
  | otherwise = -b
  where
  (l,r) = endpointsMP b
