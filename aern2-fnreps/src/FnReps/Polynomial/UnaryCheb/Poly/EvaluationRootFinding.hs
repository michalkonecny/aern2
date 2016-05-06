{-# LANGUAGE Arrows, ScopedTypeVariables, FlexibleContexts, TypeOperators, TemplateHaskell, OverloadedStrings, UndecidableInstances #-}
module FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding 
(
    evalDirectA, evalDirectOnBall, evalLipschitzOnBall
    , range
    , compose
    , shiftDomainBy
    , evalExample1, evalExample2
    , composeExample1
)
where

import AERN2.Num
import AERN2.RealFunction

import FnReps.Polynomial.UnaryCheb.Poly.Basics
import FnReps.Polynomial.UnaryCheb.Poly.SizeReduction
import FnReps.Polynomial.UnaryCheb.Poly.Cheb2Power
import FnReps.Polynomial.UnaryCheb.Poly.DCTMultiplication ()

import qualified FnReps.Polynomial.UnaryPower.Poly.EvaluationRootFinding as Power

import qualified FnReps.Polynomial.UnaryPower.IntPoly.Basics as IntPolyB

import qualified FnReps.Polynomial.UnaryPower.IntPoly.EvaluationRootFinding as IntPolyEV

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

{-|
   The function enclosures p1, p2, samples of the exact composition p1 o p2 
   and its enclosure computed here are plotted at:
   http://fooplot.com/plot/tutd2wzuat 
-}
composeExample1 :: Poly
composeExample1 =
    compose 4 NormZero p1 p2
    where
    p1 = polyAddToRadius (x*(1+x)) (mpBall 0)
    p2 = polyAddToRadius (x*(1-x)/3) (mpBall 0.25)
    x = projUnaryFnA polyFixedDomain :: Poly

{- RealUnaryFnA instance -}

instance
    (ArrowReal to MPBall) => 
    RealUnaryFnA to Poly
    where
    type UnaryFnDomPoint Poly = Rational
    type UnaryFnPoint Poly = MPBall
    getDomainUnaryFnA =
        arr $ const polyFixedDomain
    constUnaryFnA =
        proc (_dom, value) ->
            returnA -< constPoly value
    projUnaryFnA =
        proc (_dom) ->
            do
            a1 <- convertA -< 1
            returnA -< fromList [(1,a1)]
    rangeOnIntervalUnaryFnA = arr aux
        where
        aux (poly@(Poly terms),Interval l r) =
            approxRange l r acc poly
            where
            c = terms_lookupCoeff terms 0
            acc = getFiniteAccuracy $ ballCentre c
    evalAtDomPointUnaryFnA =
        proc (f, x) ->
            do
            xB <- convertA -< x
            evalAtPointUnaryFnA -< (f,xB)
    evalAtPointUnaryFnA =
        proc (f, x) ->
            do
            case getAccuracy x of
                Exact ->
                    returnA -< evalDirectOnBall f x
                _ ->  
                    returnA -< evalLipschitzOnBall f x


shiftDomainBy :: Rational -> Poly -> Poly
shiftDomainBy a p1 =
    normaliseCoeffs $
    evalDirect p1 (x+a)
    where
    x = setPrecision p $ projUnaryFnA polyFixedDomain :: Poly
    p = getPrecision p1

{-|
    (compose f g)(x) encloses f(g(x))
-}
compose :: Degree -> NormLog -> Poly -> Poly -> Poly
compose d sw p1 p2 =
    maybeTrace
    (
        "compose:"
        ++ "\n p2 = " ++ show p2
        ++ "\n p2 ~ " ++ showA p2
        ++ "\n p2Range = " ++ show p2Range
        ++ "\n p1 = " ++ show p1
        ++ "\n p1 ~ " ++ showA p1
        ++ "\n p1DerivRange = " ++ show p1DerivRange
        ++ "\n result = " ++ show result
        ++ "\n result ~ " ++ showA p1_o_p2C
    ) $
    result
    where
    showA p = show $ getApproximate (bits 40) (cheb2Power p)
    result = polyAddToRadius p1_o_p2C (p2R*(abs p1DerivRange)) 
    -- compose centre of p2 into p1:
    p1_o_p2C = reduceDegreeAndSweep d sw $ evalDirect p1 (polyCentre p2)
    -- compute the range of p2:
    p2Range = rangeOnIntervalUnaryFnA (p2, polyFixedDomain)
    -- compute the range of the derivative of p1 over the range of p2:
    p1DerivRange = (endpoints2Ball l r) / denom
        where
        Interval l r = Power.range acc p1DerivPower p2Range
        (p1IntPower, denom, _err) = cheb2IntPower p1
        p1DerivIntPower = IntPolyB.derivative p1IntPower
        p1DerivPower = IntPolyB.toFPPoly p1DerivIntPower
        acc = getFiniteAccuracy $ ballCentre $ terms_lookupCoeff (poly_terms p1) 0
    p2R = polyRadius p2
    

{-|
    An evaluation of the polynomial at x using Clenshaw Algorithm
    (https://en.wikipedia.org/wiki/Clenshaw_algorithm#Special_case_for_Chebyshev_series). 
-}
evalDirect :: 
    (CanAddSameType ra, CanSubSameType ra, CanMulSameType ra, 
     Convertible Integer ra, CanAddMulDivScalar ra Integer,
     CanAddMulScalar ra MPBall) 
    => 
    Poly -> ra -> ra
evalDirect (Poly terms) (x :: ra) =
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
    An evaluation of the polynomial at x using Clenshaw Algorithm
    (https://en.wikipedia.org/wiki/Clenshaw_algorithm#Special_case_for_Chebyshev_series). 
    An arrow-generic version.
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
    setPrecision p $
    runWithPrecisionPolicy evalDirectA (ppKeepExact defaultPrecision) (poly, x)
    where
    p = getPrecision x `max` getPrecision poly

{-|
    An evaluation of the polynomial at the ball x using an estimated Lipschitz constant on x. 
-}
evalLipschitzOnBall :: Poly -> MPBall -> MPBall
evalLipschitzOnBall p@(Poly terms) b =
--    maybeTrace
--    (
--        "evalLipschitzOnBall:" ++
--        "\n lp = " ++ show lp ++
--        "\n b_centre = " ++ show b_centre ++
--        "\n b_errorBall = " ++ show b_errorBall ++
--        "\n evalDirectOnBall p b_centre = " ++ show (evalDirectOnBall p b_centre)
--    )
    result
    where
    result = (evalDirectOnBall p b_centre) + b_errorBall * lp
    (b_centre, b_errorBall) = getCentreAndErrorBall b
    lp = sum (map abs $ terms_coeffs terms) * (terms_degree terms)^2

range :: Accuracy -> Poly -> Interval MPBall -> Interval MPBall
range ac p (Interval l r) =
    maybeTrace ("range: ac = " ++ show ac) $
    approxRange (toRationalDown l) (toRationalUp r) ac p

approxRange :: Rational -> Rational -> Accuracy -> Poly -> Interval MPBall
approxRange l r ac p = 
    Interval minValue maxValue
    where
    (p', _denom, _errBall) = cheb2IntPower $ p
    dp'  = IntPolyB.derivative $ p'
    dp'' = IntPolyB.toFPPoly $ dp'
    criticalPoints = 
        map (\(Interval a b) -> Power.approximateRootByBisection a b ac dp'') $ 
            IntPolyEV.isolateRoots l r dp'
    criticalValues = 
        [evalDirectOnBall p (mpBall l), evalDirectOnBall p (mpBall r)] 
        ++ map (evalLipschitzOnBall p) criticalPoints
    minValue = foldl1 min criticalValues
    maxValue = foldl1 max criticalValues

{-
    The following function is not implemented yet.  It is not yet clear whether it will be needed. 

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

{-

_approximateRoot :: Rational -> Rational -> Accuracy -> Poly -> MPBall
_approximateRoot l r a p = case evalDirectOnRational p l > 0 of
                                        Just False -> aux l r a p False
                                        Just True  -> aux l r a p True
                                        Nothing    -> ri2ball (Interval l r) a
                                      where
                                      aux l' r' a' p' posL =
                                       if getAccuracy (ri2ball (Interval l' r') (a + 2)) >= a then
                                            ri2ball (Interval l' r') a
                                       else case trisect l' r' posL p of
                                        Just (l'',r'',posL') -> aux l'' r'' a' p' posL'
                                        Nothing -> ri2ball (Interval l' r') a        
                                        
trisect :: Rational -> Rational -> Bool -> Poly -> Maybe (Rational,Rational, Bool) -- l', r', l' positive?
trisect l r posL p = case (posML,posMR) of
                        (Just True, _) -> Just $ if not posL then (l,ml,posL) else (ml,r,True)
                        (Just False,_) -> Just $ if posL     then (l,ml,posL) else (ml,r,False)
                        (_, Just True) -> Just $ if posL     then (mr,r,True) else (l,mr, posL)
                        (_, Just False)-> Just $ if not posL then (mr,r, False) else (l, mr, posL)
                        (_,_)          -> Nothing
                     where
                     ml = (9*l + 7*r)/16
                     mr = (7*l + 9*r)/16
                     posML = evalDirectOnRational p ml > 0
                     posMR = evalDirectOnRational p mr > 0

evalDirectOnRational :: Poly -> Rational -> MPBall
evalDirectOnRational poly@(Poly ts) x = 
    evalDirectOnBall poly (rational2BallP (getPrecision $ head $ terms_coeffs ts) x)
-}
