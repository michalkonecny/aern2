{-|
    Module      :  AERN2.Poly.Cheb.DCT
    Description :  Interpolation using Discrete cosine transform
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Interpolation using Discrete cosine transform
-}

module AERN2.Poly.Cheb.DCT
(
  lift1_DCT, lift2_DCT
)
where

import Numeric.MixedTypes
import qualified Prelude as P
-- import Text.Printf

import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

import Math.NumberTheory.Logarithms (integerLog2)

import AERN2.Normalize

import AERN2.MP

import AERN2.Real

-- import AERN2.Interval
-- import AERN2.RealFun.Operations
-- import AERN2.RealFun.UnaryFun

import AERN2.Poly.Basics

import AERN2.Poly.Cheb.Type


import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
-- shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace
    | shouldTrace = trace
    | otherwise = const id

{-|
  DCT-approximate the result of applying the given binary function @f@
  pointwise to the given polynomials @p1@ and @p2@.
-}
lift2_DCT ::
  (PolyCoeff c)
  =>
  (Degree -> Degree -> Degree)
    {-^ detemining a degree bound for the result from the degrees of @p1@ and @p2@ -} ->
  (c -> c -> c) {-^ the function @f@ to apply pointwise to @p1@ and @p2@ -} ->
  ChPoly c {-^ @p1@ -} -> ChPoly c {-^ @p2@ -} -> ChPoly c
lift2_DCT getDegree op pA pB
  | domA /= domB = error "lift2_DCT: combining functions with incompatible domains"
  | otherwise =
    -- maybeTrace
    -- (
    --     "lift2_DCT:"
    --     ++ "\n cN = " ++ show cN
    --     ++ "\n prc = " ++ show prc
    --     ++ "\n workingPrec = " ++ show workingPrec
    --     -- ++ "\n dA = " ++ show dA
    --     -- ++ "\n dB = " ++ show dB
    --     -- ++ "\n aT = " ++ show aT
    --     -- ++ "\n bT = " ++ show bT
    --     -- ++ "\n cT = " ++ show cT
    --     -- ++ "\n c = " ++ show c
    -- ) $
    result
  where
  dA = terms_degree $ chPoly_terms pA
  dB = terms_degree $ chPoly_terms pB
  resultDegree = getDegree dA dB
  cNexponent = 1 + (integer $ integerLog2 $ max 1 (resultDegree + 1))
  cN = 2 ^ cNexponent

  -- prc = (getPrecision pA) `max` (getPrecision pB)
  workingPrec = prec $ 100 + cN
  (ChPoly domA (Poly termsA) _) = raisePrecisionIfBelow workingPrec pA
  (ChPoly domB (Poly termsB) _) = raisePrecisionIfBelow workingPrec pB

  aT = coeffs2gridvalues cN termsA
  bT = coeffs2gridvalues cN termsB

  cT = zipWith op aT bT -- multiplication of the cN+1 values of the polynomials on the grid

  (c0Double : c) = map (* (2 / cN)) (tDCT_I_nlogn cT) -- interpolate the values using a polynomial

  result =
    normalize $
    -- setPrecision prc $
    reduceDegree resultDegree $
      ChPoly domA (Poly $ terms_fromList $ zip [0..] (c0Double / 2 : c)) Nothing
--    terms_fromList [(0, mpBall 1)] -- dummy for debugging exceptions

{-|
  DCT-approximate the result of applying the given function @f@
  pointwise to the given polynomial @p@.
-}
lift1_DCT ::
  (Field c, CanMulBy c CauchyReal, CanNormalize (ChPoly c), Show c) =>
  (Degree -> Degree) {-^ detemining a degree bound for the result from the degree of @p@ -} ->
  (c -> c) {-^ the function @f@ to apply pointwise to @p@ -} ->
  ChPoly c {-^ @p@ -} ->
  ChPoly c
lift1_DCT getDegree op (ChPoly dom (Poly termsA) _) =
    maybeTrace
    (
        "lift1_DCT:"
        ++ "\n cN = " ++ show cN
        ++ "\n dA = " ++ show dA
        ++ "\n aT = " ++ show aT
        ++ "\n cT = " ++ show cT
        ++ "\n c0Double = " ++ show c0Double
        ++ "\n c = " ++ show c
    ) $
    normalize $
    ChPoly dom (Poly terms) Nothing
    where
    terms =
      terms_fromList $ zip [0..] (c0Double / 2 : c)
--    terms_fromList [(0, mpBall 1)] -- dummy for debugging exceptions
    (c0Double : c) = map (* (2 / cN)) (tDCT_I_nlogn cT) -- interpolate the values using a polynomial

    -- op on the cN+1 values of the polynomials on the grid:
    cT = map op aT

    aT = coeffs2gridvalues cN termsA

    cN = 2 ^ (1 + (integer $ integerLog2 $ max 1 (getDegree dA + 1)))
    dA = terms_degree termsA


{-|
    Compute the values of the polynomial termsA on a grid.
-}
coeffs2gridvalues ::
  (Field c, CanMulBy c CauchyReal) =>
  Integer -> Terms c -> [c]
coeffs2gridvalues cN terms =
    tDCT_I_nlogn coeffs
    where
    -- convert from sparse to dense representation:
    coeffs = pad0 $ map (terms_lookupCoeffDoubleConstTerm terms) [0..(terms_degree terms)]
    pad0 list = take (int $ cN + 1) $ list ++ (repeat (convertExactly 0))


{-|
    DCT-I computed directly from its definition in
    [BT97, page 18, display (6.1)].

    This is quite inefficient for large N.
    It is to be used only for N<8 and as a reference in tests.
-}
tDCT_I_reference ::
  (Field c, CanMulBy c CauchyReal) =>
  [c] {-^ @a@ a vector of validated real numbers -} ->
  [c] {-^ @a~@ a vector of validated real numbers -}
tDCT_I_reference a =
    [sum [ (eps cN k) * (a !! k) * cos ( ((mu * k) * pi) / cN)
            | k <- [0..cN]
         ]
        | mu <- [0..cN]
    ]
    where
    cN = integer (length a) - 1

{-| An auxiliary family of constants, frequently used in Chebyshev-basis expansions. -}
eps :: Integer -> Integer -> Rational
eps n k
    | k == 0 = 0.5
    | k == n = 0.5
    | otherwise = 1.0

{-|
    DCT-I computed by splitting N and via DCT-III as described in
    [BT97, page 18, Proposition 6.1].

    Precondition: (length a) = 1+2^{t+1} where t > 1
-}
tDCT_I_nlogn ::
  (Field c, CanMulBy c CauchyReal) =>
  [c] {-^ @a@ a vector of validated real numbers -} ->
  [c] {-^ @a~@ a vector of validated real numbers -}
tDCT_I_nlogn a
    | cN < 8 = tDCT_I_reference a
    | otherwise = map aTilde [0..cN]
    where
    aTilde i
        | even i = fTilde !! (floor (i/2))
        | otherwise = gTilde !! (floor ((i - 1)/2))
    fTilde = tDCT_I_nlogn f
    gTilde = tDCT_III_nlogn g
    f = [ (a !! ell) + (a !! (cN - ell)) | ell <- [0..cN1]]
    g = [ (a !! ell) - (a !! (cN - ell)) | ell <- [0..cN1-1]]
    cN = integer (length a) - 1
    cN1 = floor (cN / 2)

{-|
    DCT-III computed directly from its definition in
    [BT97, page 18, display (6.2)].

    This is quite inefficient.  It is to be used only as a reference in tests.
-}
_tDCT_III_reference ::
  (Field c, CanMulBy c CauchyReal) =>
  [c] {-^ g a vector of validated real numbers -} ->
  [c] {-^ g~ a vector of validated real numbers -}
_tDCT_III_reference g =
    [sum [ (eps cN1 k) * (g !! k) * cos ( (((2*j+1)*k) * pi) / cN)
            | k <- [0..(cN1-1)]
         ]
        | j <- [0..(cN1-1)]
    ]
    where
    cN = cN1 * 2
    cN1 = integer (length g)

{-|
    DCT-III computed via SDCT-III.  The reduction is described on page 20.

    Precondition: integer (length g) is a power of 2
-}
tDCT_III_nlogn ::
  (Field c, CanMulBy c CauchyReal) =>
  [c] {-^ g a vector of validated real numbers -} ->
  [c] {-^ g~ a vector of validated real numbers -}
tDCT_III_nlogn g =
    h2g $ tSDCT_III_nlogn $ map g2h $ zip [0..] g
    where
    g2h (i,gi) = (eps cN1 i) * gi
    h2g h = map get_g [0..cN1-1]
        where
        get_g i
            | even i = h !! (floor (i/2 :: Rational))
            | otherwise = h !! (floor $ (2*cN1 - i - 1)/2)
    cN1 = integer (length g)

{-|
    Simplified DCT-III computed directly from its definition in
    [BT97, page 20, display (6.3)].

    This is quite inefficient.  It is to be used only as a reference in tests.
-}
_tSDCT_III_reference ::
  (Field c, CanMulBy c CauchyReal) =>
  [c] {-^ h a vector of validated real numbers -} ->
  [c] {-^ h~ a vector of validated real numbers -}
_tSDCT_III_reference h =
    [sum [ (h !! ell) * cos ( (((4*j+1)*ell) * pi) / cN)
            | ell <- [0..(cN1-1)]
         ]
        | j <- [0..(cN1-1)]
    ]
    where
    cN = cN1 * 2
    cN1 = integer (length h)

{-|
    Simplified DCT-III computed as described in
    [BT97, page 21, Algorithm 1].

    Changed many occurrences of N1 with N1-1 because the indices were out of range.
    This is part of a trial and error process.

    Precondition: length h is a power of 2
-}
tSDCT_III_nlogn ::
  (Field c, CanMulBy c CauchyReal) =>
  [c] {-^ h a vector of validated real numbers -} ->
  [c] {-^ h~ a vector of validated real numbers -}
tSDCT_III_nlogn (h :: [c]) =
    map (\ (_,[a],_) -> a) $
        List.sortBy (\ (i,_,_) (j,_,_) -> P.compare i j) $
        splitUntilSingletons $ [(0, h, 1)]
    where
    splitUntilSingletons :: [(Integer, [c], Integer)] -> [(Integer, [c], Integer)]
    splitUntilSingletons groups
        | allSingletons = groups
        | otherwise =
            splitUntilSingletons $
                concat $ map splitGroup groups
        where
        allSingletons = and $ map isSingleton groups
        isSingleton (_, [_], _) = True
        isSingleton _ = False
    splitGroup :: (Integer, [c], Integer) -> [(Integer, [c], Integer)]
    splitGroup (c_Itau_minus_1, hItau_minus_1, two_pow_tau_minus_1) =
        [subgroup 0, subgroup 1]
        where
        subgroup bit_iTauMinus1 =
            (c_Itau_minus_1 + bit_iTauMinus1 * two_pow_tau_minus_1,
             map hItau [0..c_Ntau_plus_1-1],
             2 * two_pow_tau_minus_1)
            where
            hItau 0 =
                (hItau_minus_1 !! 0)
                +
                (minusOnePow bit_iTauMinus1) * (hItau_minus_1 !! (c_Ntau_plus_1)) * gamma
            hItau n =
                (hItau_minus_1 !! n)
                -
                (hItau_minus_1 !! (c_Ntau - n))
                +
                ((2 * (minusOnePow bit_iTauMinus1)) * (hItau_minus_1 !! (c_Ntau_plus_1+n)) * gamma)
            gamma =
                cos $ (((4 * c_Itau_minus_1) + 1) * pi) / (4*two_pow_tau_minus_1)
        c_Ntau = integer (length hItau_minus_1)
        c_Ntau_plus_1
            | even c_Ntau = floor (c_Ntau/2)
            | otherwise = error "tSDCT_III_nlogn: precondition violated: (length h) has to be a power of 2"

    minusOnePow :: Integer -> Integer
    minusOnePow 0 = 1
    minusOnePow 1 = -1
    minusOnePow _ = error "tSDCT_III_nlogn: minusOnePow called with a value other than 0,1"
