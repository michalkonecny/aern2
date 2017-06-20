{-# LANGUAGE TemplateHaskell #-}
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
  , module AERN2.MP.Enclosure
  -- * The Ball type
  , MPBall(..), CanBeMPBall, mpBall, CanBeMPBallP, mpBallP
  , reducePrecionIfInaccurate
  -- * Ball construction/extraction functions
  , endpointsMP, fromEndpointsMP
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

import Control.CollectErrors

import GHC.Generics (Generic)

import Text.Printf

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
import AERN2.MP.Enclosure

data MPBall = MPBall
  { ball_value :: MPFloat
  , ball_error :: ErrorBound
  }
  -- { ball_value :: {-# UNPACK #-} ! MPFloat
  -- , ball_error :: {-# UNPACK #-} ! ErrorBound
  -- }
  deriving (Generic)

instance Show MPBall
    where
    show b@(MPBall x _e) =
      printf "[%s ± %s]" (show x) (showAC $ getAccuracy b)
      -- "[" ++ show x ++ " ± " ++ show e ++ "](prec=" ++ (show $ integer $ getPrecision x) ++ ")"
      where
      showAC Exact = "0"
      showAC NoInformation = "oo"
      showAC ac = "<2^(" ++ show (negate $ fromAccuracy ac) ++ ")"


instance (SuitableForCE es) => CanEnsureCE es MPBall where

-- instance CanTestValid MPBall where
--   isValid = isFinite

instance CanTestNaN MPBall where
  isNaN = not . isFinite
instance CanTestFinite MPBall where
  isInfinite = const False
  isFinite (MPBall x e) = isFinite x && isFinite (mpFloat e)

instance CanNormalize MPBall where
  normalize b
    | isFinite b =
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

instance CanTestContains MPBall MPBall where
  contains (MPBall xLarge eLarge) (MPBall xSmall eSmall) =
    xLargeDy - eLargeDy <= xSmallDy - eSmallDy
    &&
    xSmallDy + eSmallDy <= xLargeDy + eLargeDy
    where
    xLargeDy = dyadic xLarge
    eLargeDy = dyadic eLarge
    xSmallDy = dyadic xSmall
    eSmallDy = dyadic eSmall

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|
    instance CanTestContains MPBall $t where
      contains (MPBall c e) x =
        l <= x && x <= r
        where
        l = cDy - eDy
        r = cDy + eDy
        cDy = dyadic c
        eDy = dyadic e
  |]))

{- ball construction/extraction functions -}

instance IsInterval MPBall MPFloat where
  fromEndpoints l u
    | u < l = fromEndpoints u l
    | otherwise =
      MPBall (mpFloat cDy) (errorBound $ mpFloat eDy)
      where
      lDy = dyadic l
      uDy = dyadic u
      cDy = (lDy + uDy) * (dyadic 0.5)
      eDy = (uDy - cDy) `max` (cDy - lDy)
  endpoints (MPBall x e) = (mpFloat lDy, mpFloat uDy)
      where
      xDy = dyadic x
      eDy = dyadic e
      lDy   = xDy - eDy
      uDy   = xDy + eDy

fromEndpointsMP :: MPFloat -> MPFloat -> MPBall
fromEndpointsMP = fromEndpoints

endpointsMP :: MPBall -> (MPFloat, MPFloat)
endpointsMP = endpoints

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
    case acGuide of
      Exact -> b
      NoInformation ->
        lowerPrecisionIfAbove (prec 2) b
      _ ->
        lowerPrecisionIfAbove newPrec b
    where
    queryBits = fromAccuracy acGuide
    newPrec =
      case (getNormLog x) of
        NormBits xNormBits ->
          prec (max 2 (queryBits + xNormBits + 2))
        NormZero ->
          prec $ max 2 queryBits
    -- bWithLowAC =
    --   case acGuide of
    --     Exact -> b
    --     NoInformation -> b
    --     _ -> normalize $
    --           MPBall x (errorBound ((0.5^(fromAccuracy acGuide))⚡))

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
