module FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Basics 
(
fromPoly,
linearPolygon,
lift2PPoly,
FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Basics.normaliseCoeffs,
setPrecision_ppoly,
addToErrorTerm,
PPoly(..)
)
where

import AERN2.Num
import FnReps.Polynomial.UnaryCheb.Poly as Poly
import FnReps.Polynomial.UnaryCheb.Poly.Basics
import qualified FnReps.Polynomial.UnaryPower.Poly as PowPoly
import Data.List as List

{-data Overlap = Infinite | Rational

instance CanMinMaxA (->) Overlap Overlap where
    minA (Infinite, x) = x
    minA (x, Infinite) = x
    minA (x, y) = minA (x,y)
    maxA (Infinite,_) = Infinite
    maxA (_, Infinite) = Infinite
    maxA (x,y) = maxA (x,y)-}

data PPoly = PPoly 
        {
            ppoly_pieces :: [(Interval Rational, Poly)],
            ppoly_overlap :: Rational 
        }

data ApproxPPoly = ApproxPPoly 
        {
            approx_ppoly_pieces :: [(Interval Rational, PowPoly.Poly)],
            approx_ppoly_accuracy :: Accuracy
        }
        

instance HasApproximate PPoly where
    type Approximate PPoly = ApproxPPoly
    getApproximate bts (PPoly pieces _) = ApproxPPoly (map (\(i,p) -> (i, cheb2Power p)) pieces) bts
    
instance Show ApproxPPoly where
    show (ApproxPPoly pcs bts) = foldl' (++) "" $ map (\(i,p) -> (show i) ++ " : " ++ (show $ getApproximate bts p) ++ "\n") pcs    

instance Show PPoly where
    show (PPoly pieces _) = foldl' (++) "" $ map (\(i,p) -> (show i) ++ ": " ++ show p++"\n") pieces

fromPoly :: Poly -> PPoly
fromPoly p = PPoly [(Interval (-1.0) 1.0, p)] 10.0

linearPolygon :: [(Rational, MPBall)] -> Rational -> PPoly
linearPolygon ((x,y) : xys) overlap = aux xys x y []
                                where
                                aux [] _ _ res = PPoly (reverse res) overlap
                                aux ((x',y'):xys) x y res = aux xys x' y' ((Interval x x',linSpline x y x' y') : res)
                                linSpline x y x' y' = Poly.normaliseCoeffs $ Poly.fromList  [(0, (y*(x' - x) - x*(y' - y))/(x' - x)), (1, (y' - y)/(x' - x))] -- TODO Poly.fromList should already provided normalised coeffs
linearPolygon [] _ = error "linearPolygon must be provided with a list of at least 2 points"                            

lift2PPoly :: (Poly -> Poly) -> (PPoly -> PPoly)
lift2PPoly f (PPoly pieces overlap) = PPoly (map (\(i,p) -> (i, f p)) pieces) overlap

normaliseCoeffs :: PPoly -> PPoly
normaliseCoeffs = lift2PPoly Poly.normaliseCoeffs
        
refine :: PPoly -> PPoly -> [(Interval Rational, Poly, Poly)]
refine (PPoly ps _) (PPoly qs _) = 
         reverse $ aux [] ps qs
         where
         aux res (x : xs) (y : ys) = let (firstLarger, intr, diff) = intersectionAndDifference x y in
                                     case diff of
                                        Nothing -> aux (intr:res) xs ys
                                        Just i  -> if firstLarger then
                                                    aux (intr:res) (i:xs) ys
                                                   else
                                                    aux (intr:res) xs (i:ys)
         aux res [] [] = res
         aux xs ys _ = error $ "PPoly refine: Lists don't match up. Left with "++(show xs) ++ " and "++ (show ys)

--precondition: both intervals have the same left end-point
intersectionAndDifference :: (Interval Rational, Poly) -> (Interval Rational, Poly) -> (Bool, (Interval Rational, Poly, Poly), Maybe (Interval Rational, Poly))
intersectionAndDifference (Interval l r, p) (Interval l' r', p') = 
                                if l /= l' then
                                    error $ "PPoly intersectionAndDifference: precondition violated. Intervals are [" ++ (show l) ++ "," ++(show r)++"] and ["++(show l')++ ","++(show r')++"]."
                                else
                                    (firstLarger, (intr, p, p'), diff)
                                where
                                firstLarger = r > r'
                                intr = Interval l $ min r r'
                                diff = if r == r' then
                                        Nothing
                                       else if r > r' then
                                        Just $ (Interval r' r, p)
                                       else
                                        Just $ (Interval r r', p')                              

setPrecision_ppoly :: Precision -> PPoly -> PPoly
setPrecision_ppoly p = lift2PPoly (setPrecision p)

addToErrorTerm :: MPBall -> PPoly -> PPoly
addToErrorTerm e = lift2PPoly (\p -> polyAddToRadius p e)
                                
instance CanAddA (->) PPoly PPoly where
    type AddTypeA (->) PPoly PPoly = PPoly
    addA (a, b) = PPoly ([ (i, p + q) | (i,p,q) <- refine a b]) $ min (ppoly_overlap a) (ppoly_overlap b)
    
instance CanMulA (->) PPoly PPoly where
    type MulTypeA (->) PPoly PPoly = PPoly
    mulA (a, b) = PPoly ([ (i, p * q) | (i,p,q) <- refine a b]) $ min (ppoly_overlap a) (ppoly_overlap b)
    
instance CanMulA (->) Poly PPoly where
    type MulTypeA (->) Poly PPoly = PPoly
    mulA (a, b) = (fromPoly a) * b    
    
instance CanMulA (->) PPoly Poly where
    type MulTypeA (->) PPoly Poly = PPoly
    mulA (a,b) = a*(fromPoly b)    

instance CanNegA (->) PPoly where
    negA (PPoly pieces overlap) = 
        PPoly (fmap (\(i,p) -> (i,-p)) pieces) overlap 

instance CanNegSameType PPoly

instance CanAddThis PPoly PPoly
instance CanAddSameType PPoly
    
instance CanSub PPoly PPoly
instance CanSubThis PPoly PPoly
instance CanSubSameType PPoly    

{- Mixed operations with Integer -}
    
instance CanAddMulScalar PPoly Integer
instance CanAddMulDivScalar PPoly Integer
    
instance CanAddA (->) PPoly Integer where
    type AddTypeA (->) PPoly Integer = PPoly
    addA (p, n) = lift2PPoly (+n) $ p
    
instance CanAddA (->) Integer PPoly where
    type AddTypeA (->) Integer PPoly = PPoly
    addA (n, p) = lift2PPoly (n+) $ p

instance CanAddThis PPoly Integer

instance CanSub PPoly Integer
instance CanSubThis PPoly Integer

instance CanSubA (->) Integer PPoly where
    type SubTypeA (->) Integer PPoly = PPoly
    subA (n, p) = lift2PPoly (n-) $ p

instance CanMulA (->) PPoly Integer where
    type MulTypeA (->) PPoly Integer = PPoly
    mulA (p , n) = lift2PPoly (*n) $ p
    
instance CanMulA (->) Integer PPoly where
    type MulTypeA (->) Integer PPoly = PPoly
    mulA (n, p) = lift2PPoly (n*) $ p

instance CanMulBy PPoly Integer

instance CanDivA (->) PPoly Integer where
    type DivTypeA (->) PPoly Integer = PPoly
    divA (p, n) = lift2PPoly (/n) $ p
    
instance CanDivBy PPoly Integer
    
{- Mixed operations with Rational -}
    
instance CanAddMulScalar PPoly Rational
instance CanAddMulDivScalar PPoly Rational

instance CanAddA (->) PPoly Rational where
    type AddTypeA (->) PPoly Rational = PPoly
    addA (p, n) = lift2PPoly (+n) p
    
instance CanAddA (->) Rational PPoly where
    type AddTypeA (->) Rational PPoly = PPoly
    addA (n, p) = lift2PPoly (n+) p

instance CanAddThis PPoly Rational

instance CanSub PPoly Rational
instance CanSubThis PPoly Rational

instance CanSubA (->) Rational PPoly where
    type SubTypeA (->) Rational PPoly = PPoly
    subA (n, poly) = addA (n,  neg poly)

instance CanMulA (->) PPoly Rational where
    type MulTypeA (->) PPoly Rational = PPoly
    mulA (p, n) = lift2PPoly (*n)  p
    
instance CanMulA (->) Rational PPoly where
    type MulTypeA (->) Rational PPoly = PPoly
    mulA (n, p) = lift2PPoly (n*) p

instance CanMulBy PPoly Rational

instance CanDivA (->) PPoly Rational where
    type DivTypeA (->) PPoly Rational = PPoly
    divA (p, n) = lift2PPoly (*n) p
    
instance CanDivBy PPoly Rational

{- Mixed operations with MPBall -}
    
instance CanAddMulScalar PPoly MPBall
instance CanAddMulDivScalar PPoly MPBall
    
instance CanAddA (->) PPoly MPBall where
    type AddTypeA (->) PPoly MPBall = PPoly
    addA (p, n) = lift2PPoly (+n) p
    
instance CanAddA (->) MPBall PPoly where
    type AddTypeA (->) MPBall PPoly = PPoly
    addA (n, p) = lift2PPoly (n+) p

instance CanAddThis PPoly MPBall

instance CanSub PPoly MPBall
instance CanSubThis PPoly MPBall

instance CanSubA (->) MPBall PPoly where
    type SubTypeA (->) MPBall PPoly = PPoly
    subA (n, poly) = addA (n,  neg poly)

instance CanMulA (->) PPoly MPBall where
    type MulTypeA (->) PPoly MPBall = PPoly
    mulA (p, n) = lift2PPoly (*n) p
    
instance CanMulA (->) MPBall PPoly where
    type MulTypeA (->) MPBall PPoly = PPoly
    mulA (n, p) = lift2PPoly (*n) p

instance CanMulBy PPoly MPBall

instance CanDivA (->) PPoly MPBall where
    type DivTypeA (->) PPoly MPBall = PPoly
    divA (p, n) = lift2PPoly (*n) p
    
instance CanDivBy PPoly MPBall
        