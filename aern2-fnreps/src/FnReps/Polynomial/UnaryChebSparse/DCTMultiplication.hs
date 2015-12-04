{-|
    This module implements the fast polynomial multiplication algorithm presented in:
    [BT97] G. Baszenski and M. Tasche, 
    “Fast polynomial multiplication and convolutions related to the discrete cosine transform,” 
    Linear Algebra Appl., vol. 252, no. 1-3, pp. 1–25, Feb. 1997
    http://dx.doi.org/10.1016/0024-3795(95)00696-6
-}
module FnReps.Polynomial.UnaryChebSparse.DCTMultiplication 
( 
 multiplyDirect_terms, multiplyDCT_terms,
 tDCT_I_nlogn, tDCT_III_nlogn, tSDCT_III_nlogn,
 tDCT_I_reference, tDCT_III_reference, tSDCT_III_reference
 )
where

import Math.NumberTheory.Logarithms (integerLog2)

import Data.List (sortBy, genericIndex)

import AERN2.Real

import FnReps.Polynomial.UnaryChebSparse.Basics

import Debug.Trace (trace)


shouldTrace :: Bool
shouldTrace = False

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id

instance CanMul UnaryChebSparse UnaryChebSparse where
    type MulType UnaryChebSparse UnaryChebSparse = UnaryChebSparse
    (UnaryChebSparse termsL) `mul` (UnaryChebSparse termsR) =
        UnaryChebSparse $ multiply_terms termsL termsR
        where
        multiply_terms
            | ((terms_size termsL) * (terms_size termsR)) < 40000 = multiplyDirect_terms
            | otherwise = multiplyDCT_terms

instance CanMulBy UnaryChebSparse UnaryChebSparse
instance CanMulSameType UnaryChebSparse


(!!!) :: [a] -> Integer -> a
(!!!) = genericIndex

multiplyDirect_terms
     :: Terms -> Terms -> Terms
multiplyDirect_terms terms1 terms2 =
    terms
    where
    terms =
        foldl addTerm terms_empty newTerms
        where
        addTerm prevTerms (i,a) = 
            terms_insertWith (+) i a prevTerms 
        newTerms =
            concat
            [   let c = a*b/2 in [(i+j, c), (abs (i-j), c)]
                | 
                (i,a) <- terms_toList terms1,
                (j,b) <- terms_toList terms2
            ]
            
multiplyDCT_terms :: Terms -> Terms -> Terms
multiplyDCT_terms termsA termsB =
    maybeTrace
    (
        "multiplyDCT_terms:"
        ++ "\n dA = " ++ show dA
        ++ "\n dB = " ++ show dB
        ++ "\n cN = " ++ show cN
        ++ "\n a = " ++ show a
        ++ "\n b = " ++ show b
        ++ "\n aT = " ++ show aT
        ++ "\n bT = " ++ show bT
        ++ "\n cT = " ++ show cT
        ++ "\n c = " ++ show c
    ) $
    terms_fromList $ zip [0..] (c0Double / 2 : c)
    where
    (c0Double : c) = map (* (2 / cN)) (tDCT_I_nlogn cT) -- interpolate the values using a polynomial 
     
    cT = zipWith (*) aT bT -- multiplication of the cN+1 values of the polynomials on the grid
    
    aT = tDCT_I_nlogn a -- compute the values of the polynomial termsA on a grid
    bT = tDCT_I_nlogn b -- compute the values of the polynomial termsB on a grid
    
    -- convert from sparse to dense representation:
    a = pad0 $ (2 * a0) : [terms_lookupDefault (integer2Ball 0) i termsA | i <- [1..dA]]
    a0 = terms_lookupDefault (integer2Ball 0)  0 termsA
    b = pad0 $ (2 * b0) : [terms_lookupDefault (integer2Ball 0)  i termsB | i <- [1..dB]]
    b0 = terms_lookupDefault (integer2Ball 0) 0 termsB
    pad0 list = take (toInt $ cN + 1) $ list ++ (repeat (integer2Ball 0))
    
    cN = 2 ^ (1 + (fromInt $ integerLog2 $ max 1 (dA + dB)))
    dA = maximum $ terms_degrees termsA
    dB = maximum $ terms_degrees termsB

{-|
    DCT-I computed directly from its definition in
    [BT97, page 18, display (6.1)].
    
    This is quite inefficient for large N.
    It is to be used only for N<8 and as a reference in tests.
-}
tDCT_I_reference :: 
    [RA] {-^ @a@ a vector of validated real numbers -} -> 
    [RA] {-^ @a~@ a vector of validated real numbers -}
tDCT_I_reference a =
    [sum [ (eps cN k) * (a !!! k) * cos ( ((mu * k) * pi) / cN)
            | k <- [0..cN] 
         ] 
        | mu <- [0..cN]
    ]
    where
    cN = fromInt (length a) - 1

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
    [RA] {-^ @a@ a vector of validated real numbers -} -> 
    [RA] {-^ @a~@ a vector of validated real numbers -}

tDCT_I_nlogn a 
    | cN < 8 = tDCT_I_reference a
    | otherwise = map aTilde [0..cN]
    where
    aTilde i 
        | even i = fTilde !!! (floor (i/2))
        | otherwise = gTilde !!! (floor ((i - 1) `div` 2))
    fTilde = tDCT_I_nlogn f
    gTilde = tDCT_III_nlogn g
    f = [ (a !!! ell) + (a !!! (cN - ell)) | ell <- [0..cN1]]
    g = [ (a !!! ell) - (a !!! (cN - ell)) | ell <- [0..cN1-1]]
    cN = fromInt (length a) - 1
    cN1 = floor (cN / 2)

{-|
    DCT-III computed directly from its definition in
    [BT97, page 18, display (6.2)].
    
    This is quite inefficient.  It is to be used only as a reference in tests.
-}
tDCT_III_reference :: 
    [RA] {-^ g a vector of validated real numbers -} -> 
    [RA] {-^ g~ a vector of validated real numbers -}
tDCT_III_reference g =
    [sum [ (eps cN1 k) * (g !!! k) * cos ( (((2*j+1)*k) * pi) / cN)
            | k <- [0..(cN1-1)] 
         ] 
        | j <- [0..(cN1-1)]
    ]
    where
    cN = cN1 * 2
    cN1 = fromInt (length g)

{-|
    DCT-III computed via SDCT-III.  The reduction is described on page 20. 
    
    Precondition: fromInt (length g) is a power of 2
-}
tDCT_III_nlogn :: 
    [RA] {-^ g a vector of validated real numbers -} -> 
    [RA] {-^ g~ a vector of validated real numbers -}
tDCT_III_nlogn g =
    h2g $ tSDCT_III_nlogn $ map g2h $ zip [0..] g 
    where
    g2h (i,gi) = (eps cN1 i) * gi
    h2g h = map get_g [0..cN1-1]
        where
        get_g i  
            | even i = h !!! (floor (i/2 :: Rational))
            | otherwise = h !!! (floor $ (2*cN1 - i - 1)/2)   
    cN1 = fromInt (length g)


{-|
    Simplified DCT-III computed directly from its definition in
    [BT97, page 20, display (6.3)].
    
    This is quite inefficient.  It is to be used only as a reference in tests.
-}
tSDCT_III_reference :: 
    [RA] {-^ h a vector of validated real numbers -} -> 
    [RA] {-^ h~ a vector of validated real numbers -}
tSDCT_III_reference h =
    [sum [ (h !!! ell) * cos ( (((4*j+1)*ell) * pi) / cN)
            | ell <- [0..(cN1-1)] 
         ] 
        | j <- [0..(cN1-1)]
    ]
    where
    cN = cN1 * 2
    cN1 = fromInt (length h)

{-|
    Simplified DCT-III computed as described in 
    [BT97, page 21, Algorithm 1].
    
    Changed many occurrences of N1 with N1-1 because the indices were out of range.
    This is part of a trial and error process.
    
    Precondition: length h is a power of 2
-}
tSDCT_III_nlogn :: 
    [RA] {-^ h a vector of validated real numbers -} -> 
    [RA] {-^ h~ a vector of validated real numbers -}
tSDCT_III_nlogn h =
    map (\ (_,[a],_) -> a) $
        sortBy (\ (i,_,_) (j,_,_) -> compare i j) $ 
        splitUntilSingletons $ [(0, h, 1)]
    where
    splitUntilSingletons :: [(Integer, [RA], Integer)] -> [(Integer, [RA], Integer)]
    splitUntilSingletons groups
        | allSingletons = groups
        | otherwise =
            splitUntilSingletons $
                concat $ map splitGroup groups
        where
        allSingletons = and $ map isSingleton groups
        isSingleton (_, [_], _) = True
        isSingleton _ = False
    splitGroup :: (Integer, [RA], Integer) -> [(Integer, [RA], Integer)]
    splitGroup (c_Itau_minus_1, hItau_minus_1, two_pow_tau_minus_1) =
        [subgroup 0, subgroup 1]
        where
        subgroup bit_iTauMinus1 =
            (c_Itau_minus_1 + bit_iTauMinus1 * two_pow_tau_minus_1, 
             map hItau [0..c_Ntau_plus_1-1], 
             2 * two_pow_tau_minus_1)
            where
            hItau 0 = 
                (hItau_minus_1 !!! 0) 
                + 
                (minusOnePow bit_iTauMinus1) * (hItau_minus_1 !!! (c_Ntau_plus_1)) * gamma
            hItau n = 
                (hItau_minus_1 !!! n)
                -
                (hItau_minus_1 !!! (c_Ntau - n))
                + 
                ((2 * (minusOnePow bit_iTauMinus1)) * (hItau_minus_1 !!! (c_Ntau_plus_1+n)) * gamma)
            gamma =
                cos $ (((4 * c_Itau_minus_1) + 1) * pi) / (4*two_pow_tau_minus_1)
        c_Ntau = fromInt (length hItau_minus_1)
        c_Ntau_plus_1 
            | even c_Ntau = floor (c_Ntau/2)
            | otherwise = error "tSDCT_III_nlogn: precondition violated: (length h) has to be a power of 2"
        
    minusOnePow :: Integer -> Integer
    minusOnePow 0 = 1
    minusOnePow 1 = -1 
    minusOnePow _ = error "tSDCT_III_nlogn: minusOnePow called with a value other than 0,1"

{-
allBits :: Integer -> [[Integer]]
allBits n 
    | n > 0 =
        (map (0:) allBits_n_minus_1) ++ (map (1:) allBits_n_minus_1)
    | n == 0 = [[]]
    | otherwise = error "allBits: precondition violated: n >= 0"
    where
    allBits_n_minus_1 = allBits (n - 1)
-}        
  
  
{-
{-| 
    The formula at the bottom of page 20, fixed by Eike. 
    
    This should be equivalent to tSDCT_III_reference.
-}
aux1 h =
    [htld j 
        | j <- [0..(cN1-1)]
    ]
    where
    htld j =
        (h !!! 0) 
        +
        (sum 
            [
                ((h !!! n) - (h !!! (cN1 - n)))
                *
                (cos ((((4*j + 1)*n) * pi)/| cN))
                | n <- [1..(cN2 - 1)]
            ]
        ) 
        +
        2 * cos(((4*j + 1) |* rPi)/ 4) *
        (
            0.5 * (h !!! cN2)
            +
            (sum
                [
                    (h !!! (cN2 + n))
                    *
                    (cos ((((4*j + 1)*(n)) * pi)/| cN))
                    | n <- [1..(cN2 - 1)]
                ]
            )
        )
    cN = cN1 * 2
    cN1 = fromInt (length h)
    cN2 = floor (cN1/2)
    
-}