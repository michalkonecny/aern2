{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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

import qualified Numeric.CollectErrors as CN

import GHC.Generics (Generic)
import Control.DeepSeq

import qualified Data.List as List

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

instance NFData MPBall

instance Show MPBall where
  show  = showWithAccuracy (bits 50)

instance ShowWithAccuracy MPBall where
  showWithAccuracy displayAC b@(MPBall x e) =
    -- printf "[%s ± %s](prec=%s)" (show x) (showAC $ getAccuracy b) (show $ integer $ getPrecision b)
    printf "[%s ± %s%s]" (dropSomeDigits $ showMPFloat x) eDS (showAC $ getAccuracy b)
    -- "[" ++ show x ++ " ± " ++ show e ++ "](prec=" ++ (show $ integer $ getPrecision x) ++ ")"
    where
    eDS 
      | e == 0 = "0"
      | otherwise  =
        case safeConvert (dyadic e) of
          Right (eD :: Double) -> printf "~%.4g" $ eD
          _ -> ""
    dropSomeDigits s =
      case List.findIndex (== '.') s of
        Nothing -> s
        Just ix -> withDotIx ix
      where
      withDotIx ix =
        let maxLength = ix + displayAC_n in
        let sTrimmed = take maxLength s in
        if length sTrimmed < maxLength
          then sTrimmed
          else (take (maxLength - 3) sTrimmed) <> "..."
    displayAC_n = 
      case displayAC of
        Exact -> 1000000000
        NoInformation -> 0
        _ -> round $ (log (double 2)/log (double 10)) * (integer $ ac2prec displayAC)
    showAC Exact = ""
    showAC NoInformation = "(oo)"
    showAC ac = " ~2^(" ++ show (negate $ fromAccuracy ac) ++ ")"
    
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

instance CanGiveUpIfVeryInaccurate MPBall where
  giveUpIfVeryInaccurate = (aux =<<)
    where
    aux b@(MPBall _ e)
      | e > 1000 = CN.noValueNumErrorPotential $ numErrorVeryInaccurate "MPBall" ""
      | otherwise = cn b

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
