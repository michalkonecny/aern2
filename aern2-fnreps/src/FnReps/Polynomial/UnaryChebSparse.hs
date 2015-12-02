module FnReps.Polynomial.UnaryChebSparse where

import FnReps.Polynomial.UnaryChebSparse.DCTMultiplication

import AERN2.Real

import qualified Data.HashMap.Strict as HM

_example1 :: UnaryChebSparse
_example1 = UnaryChebSparse (HM.fromList [(0,integer2BallP (prec 100) 1)])

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
    
instance CanMul UnaryChebSparse UnaryChebSparse where
    type MulType UnaryChebSparse UnaryChebSparse = UnaryChebSparse
    (UnaryChebSparse termsL) `mul` (UnaryChebSparse termsR) =
        UnaryChebSparse $ multiplyDCT_terms termsL termsR

instance CanMulBy UnaryChebSparse UnaryChebSparse
instance CanMulSameType UnaryChebSparse
    

