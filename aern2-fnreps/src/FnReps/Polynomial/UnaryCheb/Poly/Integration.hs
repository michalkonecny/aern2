{-# LANGUAGE UndecidableInstances #-}
module FnReps.Polynomial.UnaryCheb.Poly.Integration where

import AERN2.RealFunction
import Control.Arrow (arr)

import FnReps.Polynomial.UnaryCheb.Poly.Basics
import FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding ()

instance (ArrowReal to MPBall) => CanIntegrateUnaryFnA to Poly where
    integrateUnaryFnA = arr aux 
        where
        aux (p, a, b) =
            evalAtPointUnaryFnA (pPrimit, b) - evalAtPointUnaryFnA (pPrimit, a)
            where
            pPrimit = primitive_function p

primitive_function :: Poly -> Poly
primitive_function (Poly terms) =
    normaliseCoeffs $
        Poly $ terms_fromListAddCoeffs $
            concat $ map oneTerm $ terms_toList terms
    where
    oneTerm (n,a)
        | n == 0 = [(1,a)] 
        | n == 1 = [(0,a/4), (2,a/4)]
        | otherwise = 
            [(n-1, -a/(2*(n-1))), 
             (n+1, a/(2*(n+1)))]
