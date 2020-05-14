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
  , MPBall(..), CanBeMPBall, mpBall, cnMPBall
  , CanBeMPBallP, mpBallP, cnMPBallP
  , reducePrecionIfInaccurate
  -- * Ball construction/extraction functions
  , fromMPFloatEndpoints
  , mpBallEndpoints, fromMPBallEndpoints
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

import Control.Applicative

import Control.CollectErrors

import GHC.Generics (Generic)

import Text.Printf

import AERN2.Normalize

import AERN2.Norm

import AERN2.MP.Dyadic
import qualified AERN2.MP.Float as MPFloat
import AERN2.MP.Float (MPFloat, mpFloat, showMPFloat)
import AERN2.MP.Float.Operators
import AERN2.MP.Precision
import AERN2.MP.Accuracy
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
      -- printf "[%s ± %s](prec=%s)" (show x) (showAC $ getAccuracy b) (show $ integer $ getPrecision b)
      printf "[%s ± %s]" (showMPFloat x) (showAC $ getAccuracy b)
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

instance IsInterval MPBall where
  type IntervalEndpoint MPBall = MPFloat
  fromEndpoints l u
    | u < l = fromEndpoints u l
    | otherwise =
      MPBall c (errorBound e)
      where
      c = (l +. u) *. (mpFloat $ dyadic 0.5)
      e = (u -^ c) `max` (c -^ l)
  endpoints (MPBall x e) = (l, u)
      where
      eFl = mpFloat e
      l   = x -. eFl
      u   = x +^ eFl

instance (IsInterval (CN MPBall)) where
    type (IntervalEndpoint (CN MPBall)) = CN MPFloat
    fromEndpoints l u = liftA2 fromEndpoints l u
    endpoints x = (fmap endpointL x, fmap endpointR x)

fromMPFloatEndpoints :: MPFloat -> MPFloat -> MPBall
fromMPFloatEndpoints = fromEndpoints

fromMPBallEndpoints :: MPBall -> MPBall -> MPBall
fromMPBallEndpoints = fromEndpointsAsIntervals

mpBallEndpoints :: MPBall -> (MPBall, MPBall)
mpBallEndpoints = endpointsAsIntervals

instance IsBall MPBall where
  type CentreType MPBall = Dyadic
  centre (MPBall cMP _e) = dyadic cMP
  centreAsBallAndRadius x = (cB,e)
    where
    (MPBall cMP e) = x
    cB = MPBall cMP (errorBound 0)
  radius (MPBall _ e) = e
  updateRadius updateFn (MPBall c e) = MPBall c (updateFn e)

instance (IsBall (CN MPBall)) where
    type CentreType (CN MPBall) = CN Dyadic
    centre = fmap centre


{--- constructing a ball with a given precision ---}

type CanBeMPBallP t = (ConvertibleWithPrecision t MPBall)

mpBallP :: (CanBeMPBallP t) => Precision -> t -> MPBall
mpBallP = convertP

cnMPBallP :: (CanBeMPBallP a) => Precision -> CN a -> CN MPBall
cnMPBallP p = fmap (mpBallP p)

{--- constructing an exact ball ---}

type CanBeMPBall t = ConvertibleExactly t MPBall

mpBall :: (CanBeMPBall t) => t -> MPBall
mpBall = convertExactly

cnMPBall :: (CanBeMPBall a) => CN a -> CN MPBall
cnMPBall = fmap mpBall


{-- extracting approximate information about a ball --}

instance HasAccuracy MPBall where
    getAccuracy = getAccuracy . ball_error

instance CanReduceSizeUsingAccuracyGuide MPBall where
  reduceSizeUsingAccuracyGuide acGuide b@(MPBall x _e) =
    case acGuide of
      NoInformation -> lowerPrecisionIfAbove (prec 2) b
      _ | getAccuracy b > acGuide -> tryPrec newPrec
      _ -> b
    where
    tryPrec p
      | getAccuracy bP >= acGuide = bP
      | otherwise = tryPrec (p + 10)
      where
      bP = lowerPrecisionIfAbove p b
    queryBits = 1 + fromAccuracy acGuide
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
        (_, MPBall boundMP _) = mpBallEndpoints $ absRaw ball

instance HasApproximate MPBall where
    type Approximate MPBall = (MPFloat, Bool)
    getApproximate ac b@(MPBall x e) =
        (approx, isAccurate)
        where
        isAccurate = getAccuracy b < ac
        approx
            | closeToN = n
            | otherwise = MPFloat.ceduCentre $ MPFloat.setPrecisionCEDU (prec (fromAccuracy ac)) x
            where
            n = mpFloat $ round $ rational x
            closeToN = ((abs $ x -^ n) <= e)

instance HasPrecision MPBall where
    getPrecision  = getPrecision . ball_value

instance CanSetPrecision MPBall where
    setPrecision p (MPBall x e)
        | p >= pPrev = MPBall xC e
        | otherwise  = MPBall xC (e + (xErr))
        where
        pPrev = MPFloat.getPrecision x
        (xC, xErr) = MPFloat.ceduCentreErr $ MPFloat.setPrecisionCEDU p x

{- negation & abs -}

instance CanNeg MPBall where
  negate (MPBall x e) = MPBall (-x) e

instance CanAbs MPBall where
  abs = normalize . absRaw

absRaw :: MPBall -> MPBall
absRaw b
  | l < 0 && 0 < r =
    fromEndpoints (mpFloat 0) (max (-l) r)
  | 0 <= l = b
  | otherwise = -b
  where
  (l,r) = endpoints b
