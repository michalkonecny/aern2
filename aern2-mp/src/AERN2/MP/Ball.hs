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
  MPBall(..), CanBeMPBall, mpBall
)
where

import Numeric.MixedTypes
import qualified Prelude as P

import AERN2.Norm
import qualified AERN2.MP.Float as MPFloat
import AERN2.MP.Float (MPFloat, mpFloat)
import AERN2.MP.Float.Operators
import AERN2.MP.Precision
import AERN2.MP.Accuracy
import qualified AERN2.MP.ErrorBound as EB
import AERN2.MP.ErrorBound (ErrorBound(..), errorBound)

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

{--- constructing a ball with a given precision ---}

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
    (MPBall xFlt xe) = convertP p x
    eUp = errorBound e

{--- constructing an exact ball ---}

type CanBeMPBall t = ConvertibleExactly t MPBall

mpBall :: (CanBeMPBall t) => t -> MPBall
mpBall = convertExactly

instance ConvertibleExactly MPBall MPBall where
  safeConvertExactly = Right

instance ConvertibleExactly Integer MPBall where
  safeConvertExactly x = Right $ MPBall (convertExactly x) (errorBound 0)

instance ConvertibleExactly Int MPBall where
  safeConvertExactly x = Right $ MPBall (convertExactly x) (errorBound 0)

{-- extracting approximate information about a ball --}

instance HasPrecision MPBall where
    getPrecision  = getPrecision . ball_value

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
