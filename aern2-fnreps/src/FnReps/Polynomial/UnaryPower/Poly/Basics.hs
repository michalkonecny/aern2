module FnReps.Polynomial.UnaryPower.Poly.Basics
(
    module AERN2.Num,
    Poly(..),
    fromList,
    fromIntegerListP,
    fromRationalListP,
    Degree,
    degree,
    Terms,
    terms_size,
    terms_empty,
    terms_degree,
    terms_degrees,
    terms_coeffs,
    terms_insertWith,
    terms_fromList,
    terms_toList,
    terms_lookupCoeff,
    terms_lookupCoeffDoubleConstTerm,
    terms_unionWith,
    terms_filter,
    shiftLeft,
    shiftRight
)
where

import qualified Data.List as List
import qualified Data.Map as Map

import qualified Prelude as Prelude
import AERN2.Num

{-|
    Unary polynomials over the domain @[-1,1]@ with interval coefficients in the monomial basis.
-}
data Poly = 
    Poly
    {
        unaryPowerDense_terms :: Terms
    }

instance Show Poly where
    show (Poly terms) =
        List.intercalate " + " $
            map showTerm $ reverse $ List.sortBy (\(a,_) (b,_) -> compare b a) $ reverse $ terms_toList terms
        where
        showTerm (deg, coeff) = show coeff ++ showPower
            where
            showPower
                | deg == 0 = ""
                | otherwise = "*x^" ++ show deg  

data ApproxPoly = ApproxPoly Accuracy Poly

instance HasApproximate Poly where
    type Approximate Poly = ApproxPoly
    getApproximate = ApproxPoly

instance Show ApproxPoly where
    show (ApproxPoly ac (Poly terms)) =
        List.intercalate " + " $
            map showTerm $ reverse $ List.sortBy (\(a,_) (b,_) -> compare b a) $ reverse $ terms_toList terms
        where
        showTerm (deg, coeff) = showApproxCoeff ++ showPower
            where
            showApproxCoeff =
                show approxCoeff ++ (if coeffInaccurate then "!" else "")
            (approxCoeff, coeffInaccurate) = getApproximate ac coeff
            showPower
                | deg == 0 = ""
                | otherwise = "*x^" ++ show deg  

type Degree = Integer

type Terms = Map.Map Degree MPBall
terms_size :: Terms -> Integer
terms_size = integer . Map.size
terms_empty :: Terms
terms_empty = Map.empty
terms_degree :: Terms -> Degree
terms_degree = fst . Map.findMax
terms_degrees :: Terms -> [Degree]
terms_degrees = Map.keys
terms_coeffs :: Terms -> [MPBall]
terms_coeffs = Map.elems
terms_insertWith :: (MPBall -> MPBall -> MPBall) -> Degree -> MPBall -> Terms -> Terms
terms_insertWith = Map.insertWith
terms_fromList :: [(Degree, MPBall)] -> Terms
terms_fromList = Map.fromList
terms_toList :: Terms -> [(Degree, MPBall)]
terms_toList = Map.toList
terms_lookupCoeff :: Terms -> Degree -> MPBall
terms_lookupCoeff terms deg = case Map.lookup deg terms of Nothing -> (mpBall 0); Just cf -> cf
terms_lookupCoeffDoubleConstTerm :: Terms -> Degree -> MPBall
terms_lookupCoeffDoubleConstTerm terms deg 
    | deg == 0 = 2 * (terms_lookupCoeff terms deg)
    | otherwise = terms_lookupCoeff terms deg
terms_unionWith :: (MPBall -> MPBall -> MPBall) -> Terms -> Terms -> Terms
terms_unionWith = Map.unionWith
terms_filter :: (Degree -> MPBall -> Bool) -> Terms -> Terms
terms_filter = Map.filterWithKey

-- alternative map implementation:
--type Terms = HM.HashMap Integer MPBall
--terms_empty :: Terms
--terms_empty = HM.empty
--terms_degrees :: Terms -> [Integer]
--terms_degrees = HM.keys
--terms_insertWith :: (MPBall -> MPBall -> MPBall) -> Integer -> MPBall -> Terms -> Terms
--terms_insertWith = HM.insertWith
--terms_fromList :: [(Integer, MPBall)] -> Terms
--terms_fromList = HM.fromList
--terms_toList :: Terms -> [(Integer, MPBall)]
--terms_toList = HM.toList
--terms_lookupDefault :: MPBall -> Integer -> Terms -> MPBall
--terms_lookupDefault = HM.lookupDefault
--terms_unionWith :: (MPBall -> MPBall -> MPBall) -> Terms -> Terms -> Terms
--terms_unionWith = HM.unionWith

degree :: Poly -> Integer
degree (Poly ts) = terms_degree ts

fromList :: [(Degree, MPBall)] -> Poly
fromList termsAsList =
    Poly (terms_fromList termsAsList)

fromIntegerListP :: Precision -> [(Degree,Integer)] -> Poly
fromIntegerListP p termsAsList =
    Poly (terms_fromList $ map i2b termsAsList)
    where
    i2b (deg,c) = (deg, integer2BallP p c)

fromRationalListP :: Precision -> [(Degree, Rational)] -> Poly
fromRationalListP p termsAsList =
    Poly (terms_fromList $ map r2b termsAsList)
    where
    r2b (deg, q) = (deg, rational2BallP p q)

instance CanNegA (->) Poly where
    negA (Poly terms) = 
        Poly $ fmap neg terms 

instance CanNegSameType Poly

instance CanAddA (->) Poly Poly where
    addA (Poly termsL, Poly termsR) =
        Poly $ terms_unionWith (+) termsL termsR

instance CanAddA (->) MPBall Poly where
    type AddTypeA (->) MPBall Poly = Poly
    addA (c, Poly ts) =
        Poly $ Map.insert 0 (c + terms_lookupCoeff ts 0) ts

instance CanAddThis Poly Poly
instance CanAddSameType Poly
    
instance CanSub Poly Poly
instance CanSubThis Poly Poly
instance CanSubSameType Poly

instance CanMulA (->) MPBall Poly where
        type MulTypeA (->) MPBall Poly = Poly
        mulA (l, Poly terms) =
                Poly $ Map.mapWithKey (\_ c -> c*l) terms
    
instance CanMulA (->) Poly Poly where
    type MulTypeA (->) Poly Poly = Poly
    mulA (p@(Poly ts), q@(Poly ts')) =
        {-if terms_degree ts > 100 || terms_degree ts' > 100 then --TODO: best strategy?
            karatsuba p q
        else-}
            Map.foldl' (+) (fromList [(0,integer2BallP (prec 53) 0)]) $ Map.mapWithKey (\p c -> c*(Poly $ Map.mapKeys (\p' -> p' + p) ts')) ts  

shiftRight :: Integer -> Poly -> Poly
shiftRight n (Poly ts) = Poly $ Map.mapKeys (\p -> p + n) ts

shiftLeft :: Integer -> Poly -> Poly
shiftLeft n (Poly ts) = Poly $ Map.filterWithKey (\p _ -> p >= 0)  $ Map.mapKeys (\p -> p - n) ts

takeTerms :: Integer -> Poly -> Poly
takeTerms n (Poly ts) = Poly $ Map.filterWithKey (\p c -> p <= n) ts
        
karatsuba :: Poly -> Poly -> Poly
karatsuba p@(Poly ts) q@(Poly ts') =
    if degree p < 25 || degree q < 25 then -- TODO best strategy?
        Map.foldl' (+) (fromList [(0,integer2BallP (prec 53) 0)]) $ Map.mapWithKey (\p c -> c*(Poly $ Map.mapKeys (\p' -> p' + p) ts')) ts
    else 
        shiftRight (2*m) r2 + shiftRight m r1 + r0
        where
        m = (degree p) `Prelude.div` 2
        r2 = karatsuba p1 q1
        r1 = karatsuba (p1 + p0) (q1 + q0) - r2 - r0 
        r0 = karatsuba p0 q0
        p1 = shiftLeft m p
        q1 = shiftLeft m q
        p0 = takeTerms (m - 1) p
        q0 = takeTerms (m - 1) q


