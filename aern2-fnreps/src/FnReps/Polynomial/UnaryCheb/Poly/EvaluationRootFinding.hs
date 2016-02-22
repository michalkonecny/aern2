{-# LANGUAGE Arrows, ScopedTypeVariables, FlexibleContexts, TypeOperators, TemplateHaskell, OverloadedStrings #-}
module FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding 
(
    evalDirectA, evalDirectOnBall, evalLipschitzOnBall 
--   , toPowerBase
    , evalExample1, evalExample2
)
where

import AERN2.Num
import FnReps.Polynomial.UnaryCheb.Poly.Basics
--import FnReps.Polynomial.UnaryPowerBase

import Control.Arrow

import Debug.Trace (trace)

shouldTrace :: Bool
--shouldTrace = False
shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id


{- examples/mini tests -}

{-|
    This example evaluates with a good accuracy when using ppKeepExact
    in the definition of evalDirectOnBall:
        
    >  [-9.994985025164541e-4 ± 6.276990874746879e-30]
    >  (0.08 secs, 83619528 bytes)
     
    This example loses accuracy when replacing ppKeepExact with ppUseMax
    in the definition of evalDirectOnBall:
    
    >  [-9.994985025164541e-4 ± 4.820648236480696e173]
    >  (0.08 secs, 109772888 bytes)
-}
evalExample1 :: MPBall
evalExample1 =
    evalDirectOnBall poly (mpBall 0.5)
    where
    poly =
        normaliseCoeffs $
            fromListRationalWithPrec (prec 100) [(n, (1/n))| n <- [1..1000] ]

evalExample2 :: MPBall
evalExample2 =
    evalLipschitzOnBall poly (endpoints2Ball (mpBall (0.5-(1/10^(20)))) (mpBall (0.5 + 1/10^(20))))
    where
    poly =
        normaliseCoeffs $
            fromListRationalWithPrec (prec 100) [(n, (1/n))| n <- [1..1000] ]

--{-|
--    An evaluation of the polynomial at the ball x using Clenshaw Algorithm
--    (https://en.wikipedia.org/wiki/Clenshaw_algorithm#Special_case_for_Chebyshev_series). 
---}
--toPowerBase :: Poly -> UnaryPowerBase
--toPowerBase p = evalDirectA (p, UnaryPowerBase [mpBall 0, mpBall 1])


{-|
    An evaluation of the polynomial at x using Clenshaw Algorithm
    (https://en.wikipedia.org/wiki/Clenshaw_algorithm#Special_case_for_Chebyshev_series). 
-}
evalDirectA :: 
    (ArrowReal to ra,
     CanAddMulScalarA to ra MPBall) 
    => 
    (Poly, ra) `to` ra
evalDirectA =
    proc (Poly terms, x) ->
        do
        let n = terms_degree terms
        z <- convertA -< 0 
        bs <- aux -< (terms,x,n,z,z)
        let _ = x : z : bs
        let (b0:_:b2:_) = reverse bs 
        $(exprA[| let [b0,b2] = vars in (b0 - b2)/2|]) -< (b0,b2)
    where
    aux =
        proc (terms,x,k,bKp2,bKp1) ->
            do
            bKPre <- $(exprA [| let [x, bKp2, bKp1] = vars in 2 * x * bKp1 - bKp2 |]) -< (x, bKp2, bKp1)
            let ak = terms_lookupCoeffDoubleConstTerm terms k
            bK <- addA -< (ak, bKPre)
            if k == 0
                then returnA -< [bKp2, bKp1, bK] 
                else
                    do
                    rest <- aux -< (terms,x, k-1, bKp1, bK)
                    returnA -< bKp2 : rest
     


{-|
    An evaluation of the polynomial at the ball x using Clenshaw Algorithm
    (https://en.wikipedia.org/wiki/Clenshaw_algorithm#Special_case_for_Chebyshev_series). 
-}
evalDirectOnBall :: Poly -> MPBall -> MPBall
evalDirectOnBall poly x =  
    runWithPrecisionPolicy evalDirectA (ppKeepExact defaultPrecision) (poly, x)

{-|
    An evaluation of the polynomial at the ball x using an estimated Lipschitz constant on x. 
-}
evalLipschitzOnBall :: Poly -> MPBall -> MPBall
evalLipschitzOnBall p@(Poly terms) b =
    maybeTrace
    (
        "evalLipschitzOnBall:" ++
        "\n lp = " ++ show lp ++
        "\n b_centre = " ++ show b_centre ++
        "\n b_errorBall = " ++ show b_errorBall ++
        "\n evalDirectOnBall p b_centre = " ++ show (evalDirectOnBall p b_centre)
    )
    result
    where
    result = (evalDirectOnBall p b_centre) + b_errorBall * lp
    (b_centre, b_errorBall) = getCentreAndErrorBall b
    lp = sum (map abs $ terms_coeffs terms) * (terms_degree terms)^2

{-|
    This function is not implemented yet.  It is not yet clear whether it will be needed. 

    Take a interval polynomial P that has admits(*) only polynomials 
    without non-simple roots and return a list of balls that contain all the roots
    and each ball contains at least one root.
    
    * An interval polynomial P admits a (non-interval) polynomial p if each coefficient
    of p is inside the corresponding interval coefficient of P.
-}
_findAllRoots :: Accuracy -> Poly -> [MPBall]
_findAllRoots = error "findAllRoots not implemented yet"
{-
    TODO:
    
    First segment the domain until for each segment S, 
    either P is clearly positive on S or clearly negative on S
    or P' (the nominal derivative of P) is clearly positive on S or clearly negative on S.
    
    Then apply Newton method until the improvement is negligible compared to the interval size
    or the given accuracy threshold is reached. 
    
-}
