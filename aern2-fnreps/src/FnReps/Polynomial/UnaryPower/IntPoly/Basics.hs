module FnReps.Polynomial.UnaryPower.IntPoly.Basics
(
    module AERN2.Num,
    toFPPoly,
    fracListFromFPPoly, --TODO: somewhat unsafe as this function can only be used on [-1,1]
    fromFracList,
    normaliseFracList,
    derivative,
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
    shiftLeft,
    reduceCoefs,
    reduceTerms,
    
    quo,
    isZero,
    zero,
    one,
    leadingCoefficient,
    coeff
)
where

import qualified Data.Map as Map
import qualified FnReps.Polynomial.UnaryPower.Poly.Basics as FP

import Data.Ratio
import Data.List
import AERN2.Num

import Debug.Trace

import qualified Prelude as P

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
degree (IntPoly ts) = 
  let
  nonZeroTs = Map.filter (/= 0) ts
  in
    if Map.null nonZeroTs then
      -1
    else
      fst $ Map.findMax nonZeroTs
      
coeff :: Degree -> IntPoly -> Integer
coeff deg (IntPoly ts) = terms_lookupCoeff ts deg      
      
leadingCoefficient :: IntPoly -> Integer 
leadingCoefficient p@(IntPoly ts) = 
  terms_lookupCoeff ts (degree p)

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

-- truncated division - only correct if exact
instance CanDivA (->) IntPoly Integer where
  type DivTypeA (->) IntPoly Integer = IntPoly
  divA (IntPoly ts, c) = 
    IntPoly $ Map.map (\x -> P.div x c) ts

isZero :: IntPoly -> Bool
isZero (IntPoly ts) = Map.null $ Map.filter (/= 0) ts

zero :: IntPoly
zero = fromList [(0,0)]

one :: IntPoly
one = fromList [(0,1)]

{-| Quotient of Euclidean division. Only valid if result is again an integer polynomial. -}
quo :: IntPoly -> IntPoly -> IntPoly
quo p q = 
  aux zero p dp
  where
  dp = degree p
  dq = degree q
  xPow k = fromList [(k,1)] 
  aux c r j = 
    if j == dq - 1 then
      c
    else 
      let
      lt = (coeff j r `P.div` leadingCoefficient q) * (xPow (j - dq))
      c' = c + lt
      r' = r - lt * q
      in
      aux c' r' (j - 1)

shiftRight :: Integer -> IntPoly -> IntPoly
shiftRight n (IntPoly ts) = IntPoly $ Map.mapKeys (\p -> p + n) ts

shiftLeft :: Integer -> IntPoly -> IntPoly
shiftLeft n (IntPoly ts) = IntPoly $ Map.filterWithKey (\p _ -> p >= 0)  $ Map.mapKeys (\p -> p - n) ts
                    
derivative :: IntPoly -> IntPoly
derivative (IntPoly ts) = 
  case Map.lookup 0 ts' of 
    Just _  -> IntPoly ts'
    Nothing -> IntPoly (Map.insert 0 0 ts') 
  where
  ts' = Map.filterWithKey (\k _ -> k >= 0) $ Map.mapKeys (\k -> k - 1) $ Map.mapWithKey (\p c -> c*p) ts                

reduceCoefs :: IntPoly -> IntPoly
reduceCoefs (IntPoly ts) = IntPoly $ reduceTerms ts

reduceTerms :: Terms -> Terms
reduceTerms ts = 
  Map.map (\c -> c `P.div` tgcd) ts
  where
  cs = map snd $ terms_toList ts
  tgcd = foldl' gcd (head cs) (tail cs)
                    
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
                                   fracApprox = toRationalUp                                

toFracList :: IntPoly -> [Rational]
toFracList (IntPoly ts) = [ case Map.lookup k ts of
                            Nothing -> 0.0
                            Just x  -> toRational x
                         | k <- [0 .. terms_degree ts] ]                       

fromFracList :: [Rational] -> IntPoly
fromFracList xs = IntPoly $ Map.fromList $ zipWith (\x y -> (x,numerator y)) [0..] xs
                       