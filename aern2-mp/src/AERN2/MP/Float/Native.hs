{-|
    Module      :  AERN2.MP.Float.Native
    Description :  Integer-based arbitrary precision floating point numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Integer-based arbitrary precision floating point numbers.
-}

module AERN2.MP.Float.Native
( module AERN2.MP.Float.Native
, Precision
)
where

import Numeric.MixedTypes hiding (div)
import qualified Prelude as P

import Data.Ratio
import Data.Bits

import Math.NumberTheory.Logarithms (integerLog2)

import AERN2.MP.Precision

{-| @MPFloat n e p@ represents the number @n*2^e@.  Moreover, @|n| < 2^p@ .  -}
data MPFloat =
  MPFloat
  { mpFloat_integer :: Integer
  , mpFloat_exponent :: Integer
  , mpFloat_precision :: Precision
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
    | e1 == e2 = n1 == n2
    | e1 < e2 = n1*(2^(e2-e1))==n2
    | otherwise = n1==n2*(2^(e1-e2))

instance P.Ord MPFloat where
  compare (MPFloat n1 e1 _) (MPFloat n2 e2 _)
    | e1 == e2 = P.compare n1 n2
    | e1 < e2 = P.compare (n1*(2^(e2-e1))) n2
    | otherwise = P.compare n1 (n2*(2^(e1-e2)))

getPrec :: MPFloat -> Precision
getPrec = mpFloat_precision

data RoundMode = Up | Down deriving (P.Eq)

reverseRoundMode :: RoundMode -> RoundMode
reverseRoundMode Up = Down
reverseRoundMode Down = Up

set :: RoundMode -> Precision -> MPFloat -> MPFloat
set mode p mpf = MPFloat n e p
  where
  (MPFloat n0 e0 _) = makeNOdd mpf
  limit = 2 ^ (integer p)
  e = e0 + eD
  eD
    | ratioFloor > 0 = 1 + integerLog2 ratioFloor
    | otherwise = 0
    where
    ratioFloor = (abs n0) `P.div` limit
  ratio = 2^eD
  n
    | eD == 0 = n0
    | n0 >= 0 =
      case mode of
        Down -> n0 `P.div` ratio
        Up -> ((n0-1) `P.div` ratio) + 1
    | otherwise =
      case mode of
        Down -> ((n0+1) `P.div` ratio) - 1
        Up -> n0 `P.div` ratio

{-  arithmetic -}

type UnaryOp = RoundMode -> Precision -> MPFloat -> MPFloat
type BinaryOp = RoundMode -> Precision -> MPFloat -> MPFloat -> MPFloat

neg :: UnaryOp
neg r p (MPFloat n e p1) =
  set r p $ MPFloat (-n) e p1

add :: BinaryOp
add r p (MPFloat n1 e1 p1) (MPFloat n2 e2 _p2) =
  set r p $ MPFloat n e p1
  where
  (n,e)
    | e1 == e2 = (n1+n2,e2)
    | e1 < e2 = ((n1*2^(e2-e1))+n2,e2)
    | otherwise = (n1+(n2*2^(e1-e2)),e1)

sub :: BinaryOp
sub r p (MPFloat n1 e1 p1) (MPFloat n2 e2 _p2) =
  set r p $ MPFloat n e p1
  where
  (n,e)
    | e1 == e2 = (n1-n2,e2)
    | e1 < e2 = ((n1*2^(e2-e1))-n2,e2)
    | otherwise = (n1-(n2*2^(e1-e2)),e1)

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

  p2I = integer p2
  limit2 = 2 ^ p2I

  e = e1 - p2I - e2
  n =
    case r of
      Down
        | n1 > 0  -> (n1 * limit2) `P.div` n2
        | otherwise -> (((n1 * limit2) + 1) `P.div` n2) - 1
      Up
        | n1 > 0  -> (((n1 * limit2) - 1) `P.div` n2) + 1
        | otherwise -> (n1 * limit2) `P.div` n2

{- conversions -}

instance Show MPFloat where
  show mpf@(MPFloat _n _e _p) =
    show (toDoubleNear mpf)
      ++ "{prec=" ++ show (integer _p) ++ "; n=" ++ show _n ++ "; e=" ++ show _e ++ "}"

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
  | otherwise = n / 2^(-e)

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
