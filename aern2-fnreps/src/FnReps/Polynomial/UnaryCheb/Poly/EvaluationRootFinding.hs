{-# LANGUAGE Arrows, ScopedTypeVariables, FlexibleContexts, TypeOperators, TemplateHaskell, OverloadedStrings, UndecidableInstances #-}
module FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding 
(
    evalDirectA, evalDirectOnBall, evalLipschitzOnBall
    , range, approxRange, sampledRange
    , compose
    , shiftDomainBy
    , evalExample1, evalExample2
    , composeExample1
    
    , rangeEnriched
    
    --for testing:
    , markovBound,
    poly_maximum,
    oldRange
)
where

import AERN2.Num
import AERN2.RealFunction

import FnReps.Polynomial.UnaryCheb.Poly.Basics
import FnReps.Polynomial.UnaryCheb.Poly.SizeReduction
import FnReps.Polynomial.UnaryCheb.Poly.Cheb2Power
import FnReps.Polynomial.UnaryCheb.Poly.DCTMultiplication ()

import qualified FnReps.Polynomial.UnaryPower.Poly.Basics as PowB
import qualified FnReps.Polynomial.UnaryPower.Poly.EvaluationRootFinding as Power --TODO rename
import Data.Map (Map)
import qualified Data.Map as Map

import qualified FnReps.Polynomial.UnaryPower.IntPoly.Basics as IntPolyB

import qualified FnReps.Polynomial.UnaryPower.IntPoly.EvaluationRootFinding as IntPolyEV

import Control.Arrow

import Debug.Trace (trace)

import Data.PQueue.Max (MaxQueue)
import qualified Data.PQueue.Max as Q

import qualified Prelude as P

shouldTrace :: Bool
--shouldTrace = False
shouldTrace = False

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

markovBound :: Poly -> MPBall
markovBound (Poly terms) = sum (map abs $ terms_coeffs terms) * (terms_degree terms)^2

data MaxInterval = 
  MultiRootInterval
  {
    mi_l :: Rational,
    mi_r :: Rational,
    mi_bernsteinCoefs :: (Integer, Map.Map Integer Integer),
    mi_value :: MPBall
  } 
  | UniqueRootInterval
  {
    mi_l :: Rational,
    mi_r :: Rational,
    mi_leftPositive :: Bool,
    mi_value :: MPBall
  }
  deriving Eq
  
instance Show MaxInterval where
  show (MultiRootInterval l r _ v) = "l: "++(show l)++" r: "++(show r)++" val: "++(show v)
  show (UniqueRootInterval l r _ v) = "l: "++(show l)++" r: "++(show r)++" val: "++(show v)

instance P.Ord MaxInterval where
  (<=) mi0 mi1 = (toRationalUp $ mi_value mi0) <= (toRationalUp $ mi_value mi1)

mi_evaluateOnInterval :: (MPBall -> MPBall) -> (Accuracy -> Bool) -> Rational -> Rational -> MPBall
mi_evaluateOnInterval fn accOK l r = 
  --evalLipschitzOnBall p $ ri2ball (Interval l r) (max (bits 2) $ normLog2Accuracy $ getNormLog $ r - l)
  let
    accuracy = normLog2Accuracy $ getNormLog $ r - l
  in
  {-if accuracy > (bits $ -(fromAccuracy $ normLog2Accuracy $ lipNorm)) then
    (evalLipschitzOnBall p $ ri2ball (Interval l r) accuracy)-}
  if accOK accuracy then
    fn $ ri2ball (Interval l r) (max accuracy (bits 100))
  else
    ri2ball (Interval (-10.0) (10.0)) (bits 10)

mi_makeMaxInterval :: Rational -> Rational -> Integer -> (Map.Map Integer Integer) -> IntPolyB.IntPoly -> (MPBall -> MPBall) -> (Accuracy -> Bool) -> MaxQueue MaxInterval
mi_makeMaxInterval l r c bs dip fn accOK =
  case vars of
    0 -> Q.empty
    1 -> Q.singleton $ UniqueRootInterval l r ((IntPolyEV.evalOnRational dip l) > 0.0) val
    _ -> Q.singleton $ MultiRootInterval l r (c,bs) val
  where
  vars = IntPolyEV.signVars bs
  val = mi_evaluateOnInterval fn accOK l r 

mi_split :: Poly -> IntPolyB.IntPoly -> (MPBall -> MPBall) -> (Accuracy -> Bool) -> MaxInterval -> MaxQueue MaxInterval
mi_split p dip fn accOK (MultiRootInterval l r (c, bs) _) = 
  (mi_makeMaxInterval l m c' bsL dip fn accOK) 
  `Q.union` (mi_makeMaxInterval m r c' bsR dip fn accOK) 
  `Q.union` (Q.singleton $ (UniqueRootInterval m m True (fn (mpBall m))))
  where
  m = (l + r)*0.5
  (c', bsL, bsR) = IntPolyEV.bernsteinCoefs l r m c bs

mi_split p dip fn accOK (UniqueRootInterval l r lp _) =
  if mv == 0.0 then
    Q.singleton $ UniqueRootInterval m m False (evalLipschitzOnBall p (mpBall m))
  else
    if mp == lp then
      Q.singleton $ UniqueRootInterval m r mp (mi_evaluateOnInterval fn accOK m r)
    else
      Q.singleton $ UniqueRootInterval l m lp (mi_evaluateOnInterval fn accOK l m)
  where
  m = (l + r)*0.5
  mv = IntPolyEV.evalOnRational dip m
  mp = mv > 0.0
  
mi_singleton :: MaxInterval -> MaxQueue MaxInterval
mi_singleton mi = Q.singleton mi  
  
mi_insert :: MaxInterval -> MaxQueue MaxInterval -> MaxQueue MaxInterval
mi_insert mi mis = Q.insert mi mis
  
poly_maximum_enriched :: (MPBall -> MPBall) -> (Accuracy -> Bool) -> Accuracy -> Rational -> Rational -> Poly -> MPBall
poly_maximum_enriched evalFn accOK acc l r p = 
  aux $ rightEndpoint `mi_insert` (leftEndpoint `mi_insert` initialInterval)
  where
  rightEndpoint = UniqueRootInterval r r True (evalFn $ mpBall r)
  leftEndpoint  = UniqueRootInterval l l True (evalFn $ mpBall l)
  (ip, _, _) = cheb2IntPower p
  dip = IntPolyB.derivative ip
  (c, bs) = IntPolyEV.initialBernsteinCoefs l r dip
  initialInterval = mi_makeMaxInterval l r c bs dip evalFn accOK
  pAcc = getAccuracy p
  aux is =
    if getAccuracy best >= (min acc $ pAcc) 
      || (getAccuracy $ 
            endpoints2Ball 
            (rational2BallP (prec $ fromAccuracy $ acc + 1) bestL) 
            (rational2BallP (prec $ fromAccuracy $ acc + 1) bestR))
         >= acc
    then
      best
    else 
      let
        split = (mi_split p dip evalFn accOK $ first)
      in
        aux $ Q.foldlDesc (\mis mi -> mi_insert mi mis) rest split
    where
    (first, rest) = Q.deleteFindMax is
    best = mi_value $ first
    bestL = mi_l $ first
    bestR = mi_r $ first -- TODO needed?     
  
poly_maximum :: Accuracy -> Rational -> Rational -> Poly -> MPBall
poly_maximum acc l r p = 
  poly_maximum_enriched (evalLipschitzOnBall p) accOK acc l r p
  where
  lipNorm = getNormLog $ markovBound p
  accOK accuracy = accuracy > (bits $ -(fromAccuracy $ normLog2Accuracy $ lipNorm))
  
sampledRange ::  Rational -> Rational -> Integer -> Poly -> Interval MPBall
sampledRange l r depth p =
    Interval minValue maxValue
    where
    minValue = foldl1 min samples
    maxValue = foldl1 max samples
    samples = map eval samplePoints
    eval = curry evalAtDomPointUnaryFnA p
    samplePoints :: [Rational]
    samplePoints = [(l*i + r*(size - i))/size | i <- [0..size]]
    size = 2^depth

oldRange :: Accuracy -> Poly -> Interval MPBall -> Interval MPBall
oldRange ac p (Interval l r) =
  approxRange (toRationalDown l) (toRationalUp r) ac p

newRange :: Accuracy -> Poly -> Interval MPBall -> Interval MPBall
newRange ac p (Interval l r) =
  Interval mn mx
  where
  l' = toRationalDown l
  r' = toRationalUp r
  mx = poly_maximum ac l' r' p
  mn = -(poly_maximum ac l' r' (-p))

newRangeEnriched :: Accuracy -> Poly -> (MPBall -> MPBall) -> (Accuracy -> Bool) -> Interval MPBall -> Interval MPBall
newRangeEnriched ac p evalFn accOK (Interval l r) =
  Interval mn mx
  where
  l' = toRationalDown l
  r' = toRationalUp r
  mx = poly_maximum_enriched evalFn accOK ac l' r' p
  mn = -(poly_maximum_enriched evalFn accOK ac l' r' (-p))

range :: Accuracy -> Poly -> Interval MPBall -> Interval MPBall
range ac p@(Poly ts) i = 
  if terms_degree ts <= 50 then
    oldRange ac p i
  else
    newRange ac p i

rangeEnriched :: Accuracy -> Poly -> (MPBall -> MPBall) -> (Accuracy -> Bool) -> Interval MPBall -> Interval MPBall
rangeEnriched ac p@(Poly ts) evalFn accOK i = 
  if terms_degree ts <= 50 then
    oldRange ac p i
  else
    newRangeEnriched ac p evalFn accOK i

approxRange :: Rational -> Rational -> Accuracy -> Poly -> Interval MPBall
approxRange l r ac p = 
    Interval minValue maxValue
    where
    (p', _denom, _errBall) = cheb2IntPower $ p
    dp'  = IntPolyB.reduceCoefs $ IntPolyB.derivative $ p'
    --dp'' = IntPolyB.toFPPoly $ dp'
    criticalPoints = 
        {-map (\(Interval a b) -> Power.approximateRootByBisection a b ac dp'') $ -}
          map (\(Interval a b) -> IntPolyEV.approximateRoot a b ac dp') $ 
            IntPolyEV.isolateRoots l r dp'
    criticalValues = 
        [evalDirectOnBall p (mpBall l), evalDirectOnBall p (mpBall r)] 
        ++ map (evalLipschitzOnBall p) criticalPoints
    minValue = foldl1 min criticalValues
    maxValue = foldl1 max criticalValues
        