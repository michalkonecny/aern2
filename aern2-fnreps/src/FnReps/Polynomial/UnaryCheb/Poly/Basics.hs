module FnReps.Polynomial.UnaryCheb.Poly.Basics 
(
    module AERN2.Num,
    Poly(..),
    poly_degree,
    polyFixedDomain, 
    showRawPoly, printRawPoly,
    fromList,
    fromListRationalWithPrec,
    normaliseCoeffs,
    setPrecision_poly,
    Degree,
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
    terms_updateConst,
    terms_fromListAddCoeffs
)
where

import qualified Data.Map as Map
--import qualified Data.HashMap.Strict as HM

import AERN2.Num
import qualified Data.List as List

{-|
    Unary polynomials over the domain @[-1,1]@ with interval coefficients in the Chebyshev basis.
    The interval coefficients are supposed to have zero radius, except in the constant term.
-}
data Poly = 
    Poly
    {
        poly_terms :: Terms
    }

type Degree = Integer

poly_degree :: Poly -> Degree
poly_degree (Poly terms) = terms_degree terms 

polyFixedDomain :: Interval Rational
polyFixedDomain = Interval (-1.0) 1.0

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
terms_map :: (MPBall -> MPBall) -> Terms -> Terms
terms_map = Map.map
terms_filter :: (Degree -> MPBall -> Bool) -> Terms -> Terms
terms_filter = Map.filterWithKey
terms_updateConst :: (MPBall -> MPBall) -> Terms -> Terms
terms_updateConst updateFn = Map.adjust updateFn 0
terms_fromListAddCoeffs :: [(Degree, MPBall)] -> Terms
terms_fromListAddCoeffs newTerms = 
    foldl addTerm terms_empty newTerms
    where
    addTerm prevTerms (i,a) = 
        terms_insertWith (+) i a prevTerms 


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

printRawPoly :: Poly -> IO ()
printRawPoly = putStrLn . showRawPoly

showRawPoly :: Poly -> String
showRawPoly (Poly terms) =
    List.intercalate " + " $
        map showTerm $ reverse $ List.sortBy (\(a,_) (b,_) -> compare b a) $ reverse $ terms_toList terms
    where
    showTerm (deg, coeff) = show coeff ++ showPower
        where
        showPower
            | deg == 0 = ""
            | otherwise = "*T_" ++ show deg  


fromList :: [(Degree, MPBall)] -> Poly
fromList termsAsList =
    Poly (terms_fromList termsAsList)

fromListRationalWithPrec :: Precision -> [(Degree, Rational)] -> Poly
fromListRationalWithPrec p termsAsList =
    Poly (terms_fromList $ map r2b termsAsList)
    where
    r2b (deg, q) = (deg, rational2BallP p q)

{-|
    Convert any non-exact coefficients of non-constant terms to exact coefficients.
-}
normaliseCoeffs :: Poly -> Poly
normaliseCoeffs (Poly terms) =
    Poly (terms_insertWith (+) 0 errorBall (terms_fromList $ concat termListN))
    where
    termList = terms_toList terms
    (termListN, errorBalls) = unzip $ map normaliseTerm termList
    normaliseTerm (deg, coeff) 
        | deg > 0 && containsZero = ([], abs centre + errorB)
        | otherwise = ([(deg, centre)], errorB)
        where
        (centre, errorB) = getCentreAndErrorBall coeff
        containsZero = ((abs centre) <= errorB) == Just True
    errorBall = sum errorBalls


instance HasPrecision Poly where
    getPrecision (Poly terms) =
        foldl1 min $ map getPrecision $ terms_coeffs terms

setPrecision_poly :: Precision -> Poly -> Poly
setPrecision_poly p (Poly terms) = Poly (terms_map (setPrecision p) terms)

instance HasAccuracy Poly where
    getAccuracy (Poly terms) =
        getAccuracy $ terms_lookupCoeff terms 0

instance CanNegA (->) Poly where
    negA (Poly terms) = 
        Poly $ fmap neg terms 

instance CanNegSameType Poly

instance CanAddA (->) Poly Poly where
    addA (Poly termsL, Poly termsR) =
        Poly $ terms_unionWith (+) termsL termsR

instance CanAddThis Poly Poly
instance CanAddSameType Poly
    
instance CanSub Poly Poly
instance CanSubThis Poly Poly
instance CanSubSameType Poly
    
{- Mixed operations with Integer -}
    
instance CanAddMulScalar Poly Integer
instance CanAddMulDivScalar Poly Integer
    
instance CanAddA (->) Poly Integer where
    type AddTypeA (->) Poly Integer = Poly
    addA (Poly terms, n) =
        Poly $ terms_updateConst (+n) terms
    
instance CanAddA (->) Integer Poly where
    type AddTypeA (->) Integer Poly = Poly
    addA (n, Poly terms) =
        Poly $ terms_updateConst (+n) terms

instance CanAddThis Poly Integer

instance CanSub Poly Integer
instance CanSubThis Poly Integer

instance CanSubA (->) Integer Poly where
    type SubTypeA (->) Integer Poly = Poly
    subA (n, poly) = addA (n,  neg poly)

instance CanMulA (->) Poly Integer where
    type MulTypeA (->) Poly Integer = Poly
    mulA (Poly terms, n) =
        Poly $ terms_map (*n) terms
    
instance CanMulA (->) Integer Poly where
    type MulTypeA (->) Integer Poly = Poly
    mulA (n, Poly terms) =
        Poly $ terms_map (*n) terms

instance CanMulBy Poly Integer

instance CanDivA (->) Poly Integer where
    type DivTypeA (->) Poly Integer = Poly
    divA (Poly terms, n) =
        Poly $ terms_map (/n) terms
    
instance CanDivBy Poly Integer
    
{- Mixed operations with Rational -}
    
instance CanAddMulScalar Poly Rational
instance CanAddMulDivScalar Poly Rational

instance CanAddA (->) Poly Rational where
    type AddTypeA (->) Poly Rational = Poly
    addA (Poly terms, n) =
        Poly $ terms_updateConst (+n) terms
    
instance CanAddA (->) Rational Poly where
    type AddTypeA (->) Rational Poly = Poly
    addA (n, Poly terms) =
        Poly $ terms_updateConst (+n) terms

instance CanAddThis Poly Rational

instance CanSub Poly Rational
instance CanSubThis Poly Rational

instance CanSubA (->) Rational Poly where
    type SubTypeA (->) Rational Poly = Poly
    subA (n, poly) = addA (n,  neg poly)

instance CanMulA (->) Poly Rational where
    type MulTypeA (->) Poly Rational = Poly
    mulA (Poly terms, n) =
        Poly $ terms_map (*n) terms
    
instance CanMulA (->) Rational Poly where
    type MulTypeA (->) Rational Poly = Poly
    mulA (n, Poly terms) =
        Poly $ terms_map (*n) terms

instance CanMulBy Poly Rational

instance CanDivA (->) Poly Rational where
    type DivTypeA (->) Poly Rational = Poly
    divA (Poly terms, n) =
        Poly $ terms_map (/n) terms
    
instance CanDivBy Poly Rational

{- Mixed operations with MPBall -}
    
instance CanAddMulScalar Poly MPBall
instance CanAddMulDivScalar Poly MPBall
    
instance CanAddA (->) Poly MPBall where
    type AddTypeA (->) Poly MPBall = Poly
    addA (Poly terms, n) =
        Poly $ terms_updateConst (+n) terms
    
instance CanAddA (->) MPBall Poly where
    type AddTypeA (->) MPBall Poly = Poly
    addA (n, Poly terms) =
        Poly $ terms_updateConst (+n) terms

instance CanAddThis Poly MPBall

instance CanSub Poly MPBall
instance CanSubThis Poly MPBall

instance CanSubA (->) MPBall Poly where
    type SubTypeA (->) MPBall Poly = Poly
    subA (n, poly) = addA (n,  neg poly)

instance CanMulA (->) Poly MPBall where
    type MulTypeA (->) Poly MPBall = Poly
    mulA (Poly terms, n) =
        Poly $ terms_map (*n) terms
    
instance CanMulA (->) MPBall Poly where
    type MulTypeA (->) MPBall Poly = Poly
    mulA (n, Poly terms) =
        Poly $ terms_map (*n) terms

instance CanMulBy Poly MPBall

instance CanDivA (->) Poly MPBall where
    type DivTypeA (->) Poly MPBall = Poly
    divA (Poly terms, n) =
        Poly $ terms_map (/n) terms
    
instance CanDivBy Poly MPBall

{- Mixed operations with CauchyReal -}
    
instance CanAddMulScalar Poly CauchyReal
instance CanAddMulDivScalar Poly CauchyReal
    
instance CanAddA (->) Poly CauchyReal where
    type AddTypeA (->) Poly CauchyReal = Poly
    addA (Poly terms, n) =
        Poly $ terms_updateConst (+n) terms
    
instance CanAddA (->) CauchyReal Poly where
    type AddTypeA (->) CauchyReal Poly = Poly
    addA (n, Poly terms) =
        Poly $ terms_updateConst (+n) terms

instance CanAddThis Poly CauchyReal

instance CanSub Poly CauchyReal
instance CanSubThis Poly CauchyReal

instance CanSubA (->) CauchyReal Poly where
    type SubTypeA (->) CauchyReal Poly = Poly
    subA (n, poly) = addA (n,  neg poly)

instance CanMulA (->) Poly CauchyReal where
    type MulTypeA (->) Poly CauchyReal = Poly
    mulA (Poly terms, n) =
        Poly $ terms_map (*n) terms
    
instance CanMulA (->) CauchyReal Poly where
    type MulTypeA (->) CauchyReal Poly = Poly
    mulA (n, Poly terms) =
        Poly $ terms_map (*n) terms

instance CanMulBy Poly CauchyReal

instance CanDivA (->) Poly CauchyReal where
    type DivTypeA (->) Poly CauchyReal = Poly
    divA (Poly terms, n) =
        Poly $ terms_map (/n) terms
    
instance CanDivBy Poly CauchyReal

