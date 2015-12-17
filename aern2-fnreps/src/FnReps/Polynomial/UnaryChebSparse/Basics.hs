module FnReps.Polynomial.UnaryChebSparse.Basics 
(
    module AERN2.Real,
    UnaryChebSparse(..),
    fromList,
    fromListRationalWithPrec,
    Terms,
    Degree,
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
    terms_filter
)
where

import qualified Data.List as List
import qualified Data.Map as Map
--import qualified Data.HashMap.Strict as HM

import AERN2.Real

{-|
    Unary polynomials over the domain @[-1,1]@ with interval coefficients in the Chebyshev basis.
    The interval coefficients are supposed to have zero radius, except in the constant term.
-}
data UnaryChebSparse = 
    UnaryChebSparse
    {
        unaryChebSparse_terms :: Terms
    }
--    deriving (Show)

instance Show UnaryChebSparse where
    show (UnaryChebSparse terms) =
        List.intercalate " + " $
            map showTerm $ reverse $ List.sortBy (\(a,_) (b,_) -> compare b a) $ reverse $ terms_toList terms
        where
        showTerm (deg, coeff) = show coeff ++ showPower
            where
            showPower
                | deg == 0 = ""
                | otherwise = "*T_" ++ show deg  

type Degree = Integer

fromList :: [(Degree, MPBall)] -> UnaryChebSparse
fromList termsAsList =
    UnaryChebSparse (terms_fromList termsAsList)

fromListRationalWithPrec :: Precision -> [(Degree, Rational)] -> UnaryChebSparse
fromListRationalWithPrec p termsAsList =
    UnaryChebSparse (terms_fromList $ map r2b termsAsList)
    where
    r2b (deg, q) = (deg, rational2BallP p q)


type Terms = Map.Map Degree MPBall
terms_size :: Terms -> Integer
terms_size = fromInt . Map.size
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
terms_lookupCoeff terms deg = case Map.lookup deg terms of Nothing -> (integer 0); Just cf -> cf
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

instance CanNeg UnaryChebSparse where
    neg (UnaryChebSparse terms) = 
        UnaryChebSparse $ fmap neg terms 

instance CanNegSameType UnaryChebSparse

instance CanAdd UnaryChebSparse UnaryChebSparse where
    (UnaryChebSparse termsL) `add` (UnaryChebSparse termsR) =
        UnaryChebSparse $ terms_unionWith (+) termsL termsR

instance CanAddThis UnaryChebSparse UnaryChebSparse
instance CanAddSameType UnaryChebSparse
    
instance CanSub UnaryChebSparse UnaryChebSparse
instance CanSubThis UnaryChebSparse UnaryChebSparse
instance CanSubSameType UnaryChebSparse
    
