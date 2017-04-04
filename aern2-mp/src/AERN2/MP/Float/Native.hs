{-|
    Module      :  AERN2.MP.Float.Native
    Description :  Integer-based arbitrary precision floating point numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Integer-based arbitrary precision floating point numbers.
    Currently not used.  It is much slower than to MPFR.
-}

module AERN2.MP.Float.Native
( module AERN2.MP.Float.Native
, Precision
)
where

import Numeric.MixedTypes hiding (div)
import qualified Prelude as P

import Text.Printf

import Data.Ratio
import Data.Bits

-- import Math.NumberTheory.Logarithms (integerLog2)

import AERN2.Norm
import AERN2.MP.Precision

{-| @MPFloat n e p@ represents the number @n*2^e@.
    Moreover, @|n| < 2^p@ and either $n=0$ or @n@ is odd.  -}
-- data MPFloat =
--   MPFloat
--   { mpFloat_integer :: Integer
--   , mpFloat_exponent :: Integer
--   , mpFloat_precision :: Precision
--   }
data MPFloat =
  MPFloat
  { mpFloat_integer :: ! Integer
  , mpFloat_exponent :: ! Integer
  , mpFloat_precision :: ! Precision
  }

makeNOdd :: MPFloat -> MPFloat
makeNOdd mpf@(MPFloat n e p)
  | n == 0 = MPFloat 0 0 p
  | odd n = mpf
  | otherwise = MPFloat (shiftR n (int z)) (e+z) p
  where
  z = countZeroBits n

countZeroBits :: Integer -> Integer
countZeroBits n = aux 0
  where
  aux i
    | testBit n (int i) = i
    | otherwise = aux (i+1)

zero :: MPFloat
zero = MPFloat 0 0 (prec 10)

one :: MPFloat
one = MPFloat 1 0 (prec 10)

instance P.Eq MPFloat where
  (MPFloat n1 e1 _) == (MPFloat n2 e2 _)
    | e1 == e2  || n1 == 0 || n2 == 0 = n1 == n2
    | e1 < e2 = n1==n2*(2^(e2-e1))
    | otherwise = n1*(2^(e1-e2))==n2

instance P.Ord MPFloat where
  compare (MPFloat n1 e1 _) (MPFloat n2 e2 _)
    | e1 == e2 || n1 == 0 || n2 == 0 = P.compare n1 n2
    | e1 < e2 = P.compare n1 (n2*(2^(e2-e1)))
    | otherwise = P.compare (n1*(2^(e1-e2))) n2

getPrec :: MPFloat -> Precision
getPrec = mpFloat_precision

instance HasNorm MPFloat where
  getNormLog (MPFloat n e _p) =
    case getNormLog n of
      NormZero -> NormZero
      NormBits nn -> NormBits $ nn + e

data RoundMode = Up | Down deriving (Show, P.Eq)

-- {-# INLINE reverseRoundMode #-}
reverseRoundMode :: RoundMode -> RoundMode
reverseRoundMode Up = Down
reverseRoundMode Down = Up

-- {-# INLINE set #-}
set :: RoundMode -> Precision -> MPFloat -> MPFloat
set r p mpf
  | (abs n0) < limit = MPFloat n0 e0 p
  | (abs n1) < limit = MPFloat n1 e1 p
  | (abs n2) < limit = MPFloat n2 e2 p
  | otherwise =
      error $ printf "set: r = %s, limit = %d, n0 = %d, e0 = %d, n1 = %d, n2 = %d" (show r) limit n0 e0 n1 n2
  where
  limit = 2 ^ (integer p)
  (MPFloat n0 e0 _) = makeNOdd mpf
  (n1,e1) = reduceNByBits eD (n0,e0)
    where
    eD =
      case getNormLog ratioCeil of
        NormZero -> 0
        NormBits b -> b
      where
      ratioCeil = ((abs n0 - 1) `P.div` limit) + 1
        -- If @n0 == 0@ then @n1@ is never computed; thus we have @|n0| > 0@ here.
  (n2,e2) = reduceNByBits 1 (n1,e1)

  reduceNByBits b (nP,eP) = (n,e)
    where
    e = eP + b
    ratio = 2^b
    n
      | b == 0 = nP
      | otherwise =
        case r of
          Down -> nP `P.div` ratio
          Up -> ((nP-1) `P.div` ratio) + 1

{-  arithmetic -}

type UnaryOp = RoundMode -> Precision -> MPFloat -> MPFloat
type BinaryOp = RoundMode -> Precision -> MPFloat -> MPFloat -> MPFloat

neg :: UnaryOp
neg r p (MPFloat n e p1) =
  set r p $ MPFloat (-n) e p1

-- {-# INLINE add #-}
add :: BinaryOp
add r p (MPFloat n1 e1 p1) (MPFloat n2 e2 _p2) =
  set r p $ MPFloat n e p1
  where
  (n,e)
    | e1 == e2 = (n1+n2,e2)
    | e1 < e2 = (n1+(n2*2^(e2-e1)),e1)
    | otherwise = ((n1*2^(e1-e2))+n2,e2)

sub :: BinaryOp
sub r p (MPFloat n1 e1 p1) (MPFloat n2 e2 _p2) =
  set r p $ MPFloat n e p1
  where
  (n,e)
    | e1 == e2 = (n1-n2,e2)
    | e1 < e2 = (n1-(n2*2^(e2-e1)),e1)
    | otherwise = ((n1*2^(e1-e2))-n2,e2)

mul :: BinaryOp
mul r p (MPFloat n1 e1 p1) (MPFloat n2 e2 _p2) =
  set r p $ MPFloat (n1*n2) (e1+e2) p1

div :: BinaryOp
div r p (MPFloat n1Pre e1 _p1) (MPFloat n2Pre e2 p2)
  | n2Pre == 0 = error "MPFloat division by 0"
  | n1Pre == 0 = MPFloat 0 0 p
  | otherwise = set r p $ MPFloat n e p
  where
  (n1,n2)
    | n2Pre < 0 = (-n1Pre, -n2Pre)
    | otherwise = (n1Pre, n2Pre)

  pI = integer p
  p2I = integer p2
  b = pI+p2I
  limit2 = 2 ^ b

  e = e1 - b - e2
  n =
    case r of
      Down -> (n1 * limit2) `P.div` n2
      Up -> (((n1 * limit2) - 1) `P.div` n2) + 1

{- conversions -}

instance Show MPFloat where
  show mpf@(MPFloat _n _e _p) =
    show (toDoubleNear mpf)
      ++ printf "(MPFloat (%d) (%d) (prec %d))" _n _e (integer _p)

toDouble :: RoundMode -> MPFloat -> Double
toDouble r mpf =
  case r of
    Up -> encodeFloat (nNear + 1) eNear
    Down -> encodeFloat (nNear - 1) eNear
  where
  dNear = toDoubleNear mpf
  (nNear, eNear) = decodeFloat dNear

toDoubleNear :: MPFloat -> Double
toDoubleNear (MPFloat n e _p) = encodeFloat n (int e)

toRational :: MPFloat -> Rational
toRational (MPFloat n e _)
  | e >= 0 = rational $ n * 2^e
  | otherwise = n / (2^(-e))

fromIntegerA :: RoundMode -> Precision -> Integer -> MPFloat
fromIntegerA r p x =
  set r p $ MPFloat x 0 p

fromRationalA :: RoundMode -> Precision -> Rational -> MPFloat
fromRationalA r p xOverY
  | x == 0 = zero
  | otherwise = div r p xP yP
  where
  x = numerator xOverY
  y = denominator xOverY -- positive
  xP = set r (p+1) $ MPFloat x 0 p
  yP = set rY (p+1) $ MPFloat y 0 p
  rY
    | x > 0 = reverseRoundMode r
    | otherwise = r
