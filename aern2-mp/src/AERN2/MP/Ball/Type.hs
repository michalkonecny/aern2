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
  , MPBall(..), ball_value, ball_error, ball_valueError
  , CanBeMPBall, mpBall, cnMPBall
  , CanBeMPBallP, mpBallP, cnMPBallP
  , reducePrecionIfInaccurate
  -- * Ball construction/extraction functions
  , fromMPFloatEndpoints
  , mpBallEndpoints, fromMPBallEndpoints
)
where

import MixedTypesNumPrelude
import qualified Prelude as P

import qualified Numeric.CollectErrors as CN

import GHC.Generics (Generic)
import Control.DeepSeq

import qualified Data.List as List

import Text.Printf

import AERN2.Normalize

import AERN2.Norm

import AERN2.MP.Dyadic
import qualified AERN2.MP.Float as MPFloat
import qualified Data.CDAR as CDAR
import AERN2.MP.Float (MPFloat(..), mpFloat)
import AERN2.MP.Float.Operators
import AERN2.MP.Precision
import AERN2.MP.Accuracy
import AERN2.MP.ErrorBound (ErrorBound, errorBound)
import AERN2.MP.Enclosure

newtype MPBall = MPBall { unMPBall :: CDAR.Approx }
  -- { ball_value :: {-# UNPACK #-} ! MPFloat
  -- , ball_error :: {-# UNPACK #-} ! ErrorBound
  -- }
  deriving (Generic)

ball_value :: MPBall -> MPFloat
ball_value = fst . ball_valueError

ball_error :: MPBall -> ErrorBound
ball_error = snd . ball_valueError

ball_valueError :: MPBall -> (MPFloat, ErrorBound)
ball_valueError (MPBall CDAR.Bottom) = error "ball_valueError: illegal MPBall"
ball_valueError (MPBall (CDAR.Approx mb m e s)) = 
  (              MPFloat (CDAR.Approx mb m 0 s)
  , errorBound $ MPFloat (CDAR.Approx mb e 0 s))

instance NFData MPBall

instance Show MPBall where
  show  = showWithAccuracy (bits 50)

instance ShowWithAccuracy MPBall where
  showWithAccuracy displayAC b = -- @(MPBall x e) =
    -- printf "[%s ± %s](prec=%s)" (show x) (showAC $ getAccuracy b) (show $ integer $ getPrecision b)
    printf "[%s ± %s%s]" (dropSomeDigits $ show x) eDS (showAC $ getAccuracy b)
    -- "[" ++ show x ++ " ± " ++ show e ++ "](prec=" ++ (show $ integer $ getPrecision x) ++ ")"
    where
    (x,e) = ball_valueError b
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
    
instance CanTestIsIntegerType MPBall -- False by default

instance CanTestValid MPBall where
  isValid = isFinite

instance CanTestNaN MPBall where
  isNaN = not . isFinite
instance CanTestFinite MPBall where
  isInfinite = const False
  isFinite (MPBall CDAR.Bottom) = False
  isFinite _ = True

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
reducePrecionIfInaccurate b =
    case (bAcc, bNorm) of
        (Exact, _) -> b
        (_, NormZero) -> b
        _ | p_e_nb < p_x -> setPrecision p_e_nb b
        _ -> b
    where
    x = ball_value b
    bAcc = getAccuracy b
    bNorm = getNormLog b
    p_x = getPrecision x
    p_e_nb = prec $ max 2 (10 + nb + fromAccuracy bAcc)
    (NormBits nb) = bNorm

instance CanGiveUpIfVeryInaccurate MPBall where
  giveUpIfVeryInaccurate = (aux =<<)
    where
    aux b -- @(MPBall _ e)
      | e > 1000000 = CN.noValueNumErrorPotential $ numErrorVeryInaccurate "MPBall" ""
      | otherwise = cn b
      where
      e = ball_error b

{- ball construction/extraction functions -}

instance IsInterval MPBall where
  type IntervalEndpoint MPBall = MPFloat
  fromEndpoints l@(MPFloat lA) u@(MPFloat uA)
    | u < l = fromEndpoints u l
    | otherwise = MPBall a
    where
    a = CDAR.endToApprox mb (CDAR.lowerBound lA) (CDAR.upperBound uA)
    mb = CDAR.mBound lA `max` CDAR.mBound uA
      -- c = (l +. u) *. (mpFloat $ dyadic 0.5)
      -- e = (u -^ c) `max` (c -^ l)
  endpoints b = (l, u)
      where
      (x,e) = ball_valueError b
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
  centre = dyadic . ball_value
  centreAsBallAndRadius b@(MPBall bA) = 
    (MPBall (CDAR.centreA bA), ball_error b)
  radius = ball_error
  updateRadius updateFn (MPBall (CDAR.Approx mb m e s)) 
    | s == s' = MPBall $ CDAR.Approx mb m e' s
    | s < s' = MPBall $ CDAR.Approx mb m (CDAR.scale e' (s' P.- s)) s
    | otherwise = MPBall $ CDAR.Approx mb (CDAR.scale m (s P.- s')) e' s'
    where
    -- (c, e) = centreAsBallAndRadius b
    MPFloat (CDAR.Approx _ e' 0 s') =  
      mpFloat $ updateFn $ errorBound $ MPFloat (CDAR.Approx mb e 0 s)
  updateRadius _ _ = error "internal error in updateRadius: Bottom"

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
    getNormLog ball = getNormLog (MPFloat boundA)
        where
        (_, MPBall boundA) = mpBallEndpoints $ absRaw ball

instance HasApproximate MPBall where
    type Approximate MPBall = (MPFloat, Bool)
    getApproximate ac b =
        (approx, isAccurate)
        where
        (x,e) = ball_valueError b
        isAccurate = getAccuracy b < ac
        approx
            | closeToN = n
            | otherwise = MPFloat.ceduCentre $ MPFloat.setPrecisionCEDU (prec (fromAccuracy ac)) x
            where
            n = mpFloat $ round $ rational x
            closeToN = ((abs $ x -^ n) <= e)

instance HasPrecision MPBall where
  getPrecision (MPBall (CDAR.Approx mb _ _ _)) = prec (P.toInteger $ mb)
  getPrecision (MPBall CDAR.Bottom) = error "getPrecision: illegal MPBall (Bottom)"

instance CanSetPrecision MPBall where
    setPrecision p (MPBall aA) = MPBall aA'
      where
      aA' = CDAR.enforceMB $ CDAR.setMB (MPFloat.p2cdarPrec p) aA

{- negation & abs -}

instance CanNeg MPBall where
  negate (MPBall a) = MPBall (P.negate a)

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

instance CanTestContains MPBall MPBall where
  contains (MPBall aLarge) (MPBall aSmall) =
    aSmall `CDAR.better` aLarge

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|
    instance CanTestContains MPBall $t where
      contains b x =
        lD <= x && x <= rD
        where
        (l,r) = endpoints b
        lD = dyadic l
        rD = dyadic r
  |]))

