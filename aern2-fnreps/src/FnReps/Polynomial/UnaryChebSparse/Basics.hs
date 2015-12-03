module FnReps.Polynomial.UnaryChebSparse.Basics 
(
    RA,
    UnaryChebSparse(..),
    fromList
)
where

import AERN2.Real

import qualified Data.HashMap.Strict as HM

type RA = MPBall

{-|
    Unary polynomials over the domain [-1,1] with interval coefficients in the Chebyshev basis.
    The interval coefficients are supposed to have a very small width.
-}
data UnaryChebSparse = 
    UnaryChebSparse
    {
        unaryChebSparse_terms :: HM.HashMap Integer RA
    }
--    deriving (Show)

fromList :: [(Integer, RA)] -> UnaryChebSparse
fromList termsAsList =
    UnaryChebSparse (HM.fromList termsAsList)

instance Show UnaryChebSparse where
    show (UnaryChebSparse terms) =
        "(UnaryChebSparse " ++ show (HM.toList terms) ++ ")"  

instance CanNeg UnaryChebSparse where
    type NegType UnaryChebSparse = UnaryChebSparse
    neg (UnaryChebSparse terms) = 
        UnaryChebSparse $ fmap neg terms 

instance CanNegSameType UnaryChebSparse

instance CanAdd UnaryChebSparse UnaryChebSparse where
    type AddType UnaryChebSparse UnaryChebSparse = UnaryChebSparse
    (UnaryChebSparse termsL) `add` (UnaryChebSparse termsR) =
        UnaryChebSparse $ HM.unionWith (+) termsL termsR

instance CanAddThis UnaryChebSparse UnaryChebSparse
instance CanAddSameType UnaryChebSparse
    
instance CanSub UnaryChebSparse UnaryChebSparse
instance CanSubThis UnaryChebSparse UnaryChebSparse
instance CanSubSameType UnaryChebSparse
    
