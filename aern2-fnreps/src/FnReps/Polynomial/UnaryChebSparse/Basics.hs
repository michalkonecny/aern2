module FnReps.Polynomial.UnaryChebSparse.Basics 
(
    RA,
    UnaryChebSparse(..),
    fromList,
    Terms,
    Degree,
    terms_size,
    terms_empty,
    terms_degrees,
    terms_coeffs,
    terms_insertWith,
    terms_fromList,
    terms_toList,
    terms_lookupDefault,
    terms_unionWith,
    terms_filter
)
where

import qualified Data.Map as Map
--import qualified Data.HashMap.Strict as HM

import AERN2.Real

type RA = MPBall

{-|
    Unary polynomials over the domain [-1,1] with interval coefficients in the Chebyshev basis.
    The interval coefficients are supposed to have a very small width.
-}
data UnaryChebSparse = 
    UnaryChebSparse
    {
        unaryChebSparse_terms :: Terms
    }
--    deriving (Show)

instance Show UnaryChebSparse where
    show (UnaryChebSparse terms) =
        "(UnaryChebSparse " ++ show (terms_toList terms) ++ ")"  

type Degree = Integer

fromList :: [(Degree, RA)] -> UnaryChebSparse
fromList termsAsList =
    UnaryChebSparse (terms_fromList termsAsList)

type Terms = Map.Map Degree RA
terms_size :: Terms -> Degree
terms_size = fromInt . Map.size
terms_empty :: Terms
terms_empty = Map.empty
terms_degrees :: Terms -> [Degree]
terms_degrees = Map.keys
terms_coeffs :: Terms -> [RA]
terms_coeffs = Map.elems
terms_insertWith :: (RA -> RA -> RA) -> Degree -> RA -> Terms -> Terms
terms_insertWith = Map.insertWith
terms_fromList :: [(Degree, RA)] -> Terms
terms_fromList = Map.fromList
terms_toList :: Terms -> [(Degree, RA)]
terms_toList = Map.toList
terms_lookupDefault :: RA -> Degree -> Terms -> RA
terms_lookupDefault d k m = case Map.lookup k m of Nothing -> d; Just v -> v
terms_unionWith :: (RA -> RA -> RA) -> Terms -> Terms -> Terms
terms_unionWith = Map.unionWith
terms_filter :: (Degree -> RA -> Bool) -> Terms -> Terms
terms_filter = Map.filterWithKey

-- alternative map implementation:
--type Terms = HM.HashMap Integer RA
--terms_empty :: Terms
--terms_empty = HM.empty
--terms_degrees :: Terms -> [Integer]
--terms_degrees = HM.keys
--terms_insertWith :: (RA -> RA -> RA) -> Integer -> RA -> Terms -> Terms
--terms_insertWith = HM.insertWith
--terms_fromList :: [(Integer, RA)] -> Terms
--terms_fromList = HM.fromList
--terms_toList :: Terms -> [(Integer, RA)]
--terms_toList = HM.toList
--terms_lookupDefault :: RA -> Integer -> Terms -> RA
--terms_lookupDefault = HM.lookupDefault
--terms_unionWith :: (RA -> RA -> RA) -> Terms -> Terms -> Terms
--terms_unionWith = HM.unionWith

instance CanNeg UnaryChebSparse where
    type NegType UnaryChebSparse = UnaryChebSparse
    neg (UnaryChebSparse terms) = 
        UnaryChebSparse $ fmap neg terms 

instance CanNegSameType UnaryChebSparse

instance CanAdd UnaryChebSparse UnaryChebSparse where
    type AddType UnaryChebSparse UnaryChebSparse = UnaryChebSparse
    (UnaryChebSparse termsL) `add` (UnaryChebSparse termsR) =
        UnaryChebSparse $ terms_unionWith (+) termsL termsR

instance CanAddThis UnaryChebSparse UnaryChebSparse
instance CanAddSameType UnaryChebSparse
    
instance CanSub UnaryChebSparse UnaryChebSparse
instance CanSubThis UnaryChebSparse UnaryChebSparse
instance CanSubSameType UnaryChebSparse
    
