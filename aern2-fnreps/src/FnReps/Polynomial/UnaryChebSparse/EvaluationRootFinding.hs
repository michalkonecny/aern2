{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module FnReps.Polynomial.UnaryChebSparse.EvaluationRootFinding 
(
    evalDirect, evalDirectOnBall, toPowerBase
)
where

import AERN2.Num
import FnReps.Polynomial.UnaryChebSparse.Basics
import FnReps.Polynomial.UnaryPowerBase


{-|
    An evaluation of the polynomial at the ball x using Clenshaw Algorithm
    (https://en.wikipedia.org/wiki/Clenshaw_algorithm#Special_case_for_Chebyshev_series). 
-}
toPowerBase :: UnaryChebSparse -> UnaryPowerBase
toPowerBase p = evalDirect p (UnaryPowerBase [mpBall 0, mpBall 1])


{-|
    An evaluation of the polynomial at x using Clenshaw Algorithm
    (https://en.wikipedia.org/wiki/Clenshaw_algorithm#Special_case_for_Chebyshev_series). 
-}
evalDirect :: 
    (Ring ra, 
     CanAddMulDivScalar ra Integer,
     CanAddMulScalar ra MPBall) 
    => 
    UnaryChebSparse -> ra -> ra
evalDirect (UnaryChebSparse terms) (x :: ra) =
    (b0 - b2)/2
    where
    n = terms_degree terms
    (b0:_:b2:_) = bs
    bs :: [ra]
    bs = reverse $ aux n (convert 0) (convert 0)
    aux k bKp2 bKp1 
        | k == 0 = [bKp2, bKp1, bK] 
        | otherwise = bKp2 : aux (k - 1) bKp1 bK
        where
        bK = (a k) + 2 * x * bKp1 - bKp2
    a k = terms_lookupCoeffDoubleConstTerm terms k 


{-|
    An evaluation of the polynomial at the ball x using Clenshaw Algorithm
    (https://en.wikipedia.org/wiki/Clenshaw_algorithm#Special_case_for_Chebyshev_series). 
-}
evalDirectOnBall :: UnaryChebSparse -> MPBall -> MPBall
evalDirectOnBall = evalDirect

{-|
    An evaluation of the polynomial at the ball x using an estimated Lipschitz constant on x. 
-}
_evalOnBallUsingLipschitz :: UnaryChebSparse -> MPBall -> MPBall
_evalOnBallUsingLipschitz =
    error "evalOnBallUsingLipschitz not implemented yet"

{-|
    This function is not implemented yet.  It is not yet clear whether it will be needed. 

    Take a interval polynomial P that has admits(*) only polynomials 
    without non-simple roots and return a list of balls that contain all the roots
    and each ball contains at least one root.
    
    * An interval polynomial P admits a (non-interval) polynomial p if each coefficient
    of p is inside the corresponding interval coefficient of P.
-}
_findAllRoots :: Accuracy -> UnaryChebSparse -> [MPBall]
_findAllRoots = error "findAllRoots not implemented yet"
{-
    TODO:
    
    First segment the domain until for each segment S, 
    either P is clearly positive on S or clearly negative on S
    or P' (the nominal derivative of P) is clearly positive on S or clearly negative on S.
    
    Then apply Newton method until the improvement is negligible compared to the interval size
    or the given accuracy threshold is reached. 
    
-}
