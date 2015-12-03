module FnReps.Polynomial.UnaryChebSparse.Basics 
(
    RA,
    UnaryChebSparse(..),
    fromList,
    Terms,
    terms_size,
    terms_empty,
    terms_keys,
    terms_insertWith,
    terms_fromList,
    terms_toList,
    terms_lookupDefault,
    terms_unionWith
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

fromList :: [(Integer, RA)] -> UnaryChebSparse
fromList termsAsList =
    UnaryChebSparse (terms_fromList termsAsList)

type Terms = Map.Map Integer RA
terms_size :: Terms -> Integer
terms_size = fromInt . Map.size
terms_empty :: Terms
terms_empty = Map.empty
terms_keys :: Terms -> [Integer]
terms_keys = Map.keys
terms_insertWith :: (RA -> RA -> RA) -> Integer -> RA -> Terms -> Terms
terms_insertWith = Map.insertWith
terms_fromList :: [(Integer, RA)] -> Terms
terms_fromList = Map.fromList
terms_toList :: Terms -> [(Integer, RA)]
terms_toList = Map.toList
terms_lookupDefault :: RA -> Integer -> Terms -> RA
terms_lookupDefault d k m = case Map.lookup k m of Nothing -> d; Just v -> v
terms_unionWith :: (RA -> RA -> RA) -> Terms -> Terms -> Terms
terms_unionWith = Map.unionWith

-- alternative map implementation:
--type Terms = HM.HashMap Integer RA
--terms_empty :: Terms
--terms_empty = HM.empty
--terms_keys :: Terms -> [Integer]
--terms_keys = HM.keys
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
    
