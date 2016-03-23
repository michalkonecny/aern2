module FnReps.Polynomial.UnaryPower.IntPoly.Basics
(
    module AERN2.Num,
    toFPPoly,
    fracListFromFPPoly, --TODO: somewhat unsafe as this function can only be used on [-1,1]
    fromFracList,
    normaliseFracList,
    derivative,
    remIntPoly,
    separablePart,
    IntPoly(..),
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
    fromList,
    shiftRight,
    shiftLeft
)
where

import qualified Data.Map as Map

import qualified Math.Polynomial as MP
import qualified FnReps.Polynomial.UnaryPower.Poly.Basics as FP

import Data.Ratio
import Data.List
import AERN2.Num

data IntPoly = IntPoly Terms

type Degree = Integer

instance Show IntPoly where
    show (IntPoly terms) =
       intercalate " + " $
            map showTerm $ reverse $ sortBy (\(a,_) (b,_) -> compare b a) $ reverse $ terms_toList terms
        where
        showTerm (deg, coeff) = show coeff ++ showPower
            where
            showPower
                | deg == 0 = ""
                | otherwise = "*x^" ++ show deg 

type Terms = Map.Map Degree Integer
terms_size :: Terms -> Int
terms_size = Map.size
terms_empty :: Terms
terms_empty = Map.empty
terms_degree :: Terms -> Degree
terms_degree = fst . Map.findMax
terms_degrees :: Terms -> [Degree]
terms_degrees = Map.keys
terms_coeffs :: Terms -> [Integer]
terms_coeffs = Map.elems
terms_insertWith :: (Integer -> Integer -> Integer) -> Degree -> Integer -> Terms -> Terms
terms_insertWith = Map.insertWith
terms_fromList :: [(Degree, Integer)] -> Terms
terms_fromList = Map.fromList
terms_toList :: Terms -> [(Degree, Integer)]
terms_toList = Map.toList
terms_lookupCoeff :: Terms -> Degree -> Integer
terms_lookupCoeff terms deg = case Map.lookup deg terms of Nothing -> 0; Just cf -> cf
terms_lookupCoeffDoubleConstTerm :: Terms -> Degree -> Integer
terms_lookupCoeffDoubleConstTerm terms deg 
    | deg == 0 = 2 * (terms_lookupCoeff terms deg)
    | otherwise = terms_lookupCoeff terms deg
terms_unionWith :: (Integer -> Integer -> Integer) -> Terms -> Terms -> Terms
terms_unionWith = Map.unionWith
terms_filter :: (Degree -> Integer -> Bool) -> Terms -> Terms
terms_filter = Map.filterWithKey

fromList :: [(Degree, Integer)] -> IntPoly
fromList termsAsList =
    IntPoly (terms_fromList termsAsList)

degree :: IntPoly -> Degree
degree (IntPoly ts) = terms_degree ts

instance CanMulA (->) Integer IntPoly where
    type MulTypeA (->) Integer IntPoly = IntPoly
    mulA (c, IntPoly ts) = IntPoly $ Map.map (\x -> c*x) ts

instance CanAddA (->) IntPoly IntPoly where
    type AddTypeA (->) IntPoly IntPoly = IntPoly
    addA (IntPoly ts, IntPoly ts') = IntPoly $ terms_unionWith (+) ts ts'
    
instance CanNegA (->) IntPoly where
    negA (IntPoly terms) = IntPoly $ fmap neg terms 

instance CanMulA (->) IntPoly IntPoly where
    type MulTypeA (->) IntPoly IntPoly = IntPoly
    mulA(IntPoly ts, IntPoly ts') = Map.foldl' (+) (fromList [(0, 0)]) $ Map.mapWithKey (\p c -> c*(IntPoly $ Map.mapKeys (\p' -> p' + p) ts')) ts

instance CanAddA (->) Integer IntPoly where
    type AddTypeA (->) Integer IntPoly = IntPoly
    addA (c, IntPoly ts) = IntPoly $ Map.insertWith (+) 0 c ts
    
instance CanAddThis IntPoly IntPoly
instance CanAddSameType IntPoly
    
instance CanSub IntPoly IntPoly
instance CanSubThis IntPoly IntPoly
instance CanSubSameType IntPoly    

shiftRight :: Integer -> IntPoly -> IntPoly
shiftRight n (IntPoly ts) = IntPoly $ Map.mapKeys (\p -> p + n) ts

shiftLeft :: Integer -> IntPoly -> IntPoly
shiftLeft n (IntPoly ts) = IntPoly $ Map.filterWithKey (\p _ -> p >= 0)  $ Map.mapKeys (\p -> p - n) ts
               
remIntPoly :: IntPoly -> IntPoly -> IntPoly
remIntPoly p q = if degree q == 0 then p else (fromList [(0,0)]) + (fromFracPoly $ MP.remPoly (toFracPoly p) (toFracPoly q))               
                    
derivative :: IntPoly -> IntPoly
derivative (IntPoly ts) = if Map.null ts' then fromList [(0,0)] else IntPoly ts' 
                          where
                          ts' = Map.filterWithKey (\k _ -> k >= 0) $ Map.mapKeys (\k -> k - 1) $ Map.mapWithKey (\p c -> c*p) ts                
                      
separablePart :: IntPoly -> IntPoly
separablePart p = remIntPoly p (derivative p)
                    
normaliseFracList :: [(Integer,Rational)] -> [Rational]
normaliseFracList xs = map ((lcmd*).snd) xs
                       where
                       lcmd = foldl' lcm 1 $ map (denominator.snd) xs
                       
normaliseFracList' :: [Rational] -> [Rational]
normaliseFracList' xs = map (lcmd*) xs
                        where
                        lcmd = foldl' lcm 1 $ map (denominator) xs                       

toFPPoly :: IntPoly -> FP.Poly
toFPPoly (IntPoly ts) = FP.Poly $ Map.map mpBall ts

-- can only be used on [-1,1]
fracListFromFPPoly :: FP.Poly -> ([(Integer,Rational)], MPBall) 
fracListFromFPPoly (FP.Poly ts) = ([ case Map.lookup k ts of 
                                            Nothing -> (k, 0.0)
                                            Just x  -> (k, fracApprox x) 
                                         | k <- [0 .. FP.terms_degree ts] ],
                                   Map.foldl' (+) (mpBall 0) $ Map.map (\x -> abs(x - fracApprox x)) ts)
                                   where
                                   fracApprox x = if (x == mpBall 0) == Just True then 0.0 else toRationalUp x --toRationalUp (mpBall 0.0) seems to be extremely slow                                

toFracList :: IntPoly -> [Rational]
toFracList (IntPoly ts) = [ case Map.lookup k ts of
                            Nothing -> 0.0
                            Just x  -> toRational x
                         | k <- [0 .. terms_degree ts] ]                       

fromFracList :: [Rational] -> IntPoly
fromFracList xs = IntPoly $ Map.fromList $ zipWith (\x y -> (x,numerator y)) [0..] xs

toFracPoly :: IntPoly -> MP.Poly Rational
toFracPoly p = MP.poly MP.LE $ toFracList p

fromFracPoly :: MP.Poly Rational -> IntPoly
fromFracPoly p = fromFracList $ normaliseFracList' $ MP.polyCoeffs MP.LE p                                   