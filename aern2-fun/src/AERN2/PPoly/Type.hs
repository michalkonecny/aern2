module AERN2.PPoly.Type
where

import Numeric.MixedTypes
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import AERN2.MP.ErrorBound
import AERN2.Interval

import AERN2.Poly.Basics
import AERN2.Poly.Cheb
import AERN2.Poly.Cheb.Ring

type Cheb = ChPoly MPBall

data PPoly =
  PPoly {ppoly_pieces  :: [(DyadicInterval, Poly MPBall)],
         ppoly_overlap :: Dyadic,
         ppoly_dom     :: DyadicInterval}

data PPolyBall =
  PPolyBall {ppolyball_centre :: PPoly, ppolyball_radius :: ErrorBound}

fromPoly :: Cheb -> PPoly
fromPoly p = PPoly [(Interval (dyadic $ -1) (dyadic 1), chPoly_poly p)] (dyadic 10) (chPoly_dom p)

{-linearPolygon :: [(Rational, MPBall)] -> Rational -> PPoly
linearPolygon ((x,y) : xys) overlap = aux xys x y []
 where
 aux [] _ _ res = PPoly (reverse res) overlap
 aux ((x',y'):xys) x y res = aux xys x' y' ((Interval x x',linSpline x y x' y') : res)
 linSpline x y x' y' = Poly.normaliseCoeffs $ Poly.fromList  [(0, (y*(x' - x) - x*(y' - y))/(x' - x)), (1, (y' - y)/(x' - x))] -- TODO Poly.fromList should already provided normalised coeffs
linearPolygon [] _ = error "linearPolygon must be provided with a list of at least 2 points"-}

lift2PPoly :: (Poly MPBall -> Poly MPBall) -> (PPoly -> PPoly)
lift2PPoly f (PPoly pieces overlap dom) = PPoly (map (\(i,p) -> (i, f p)) pieces) overlap dom

refine :: PPoly -> PPoly -> [(DyadicInterval, Poly MPBall, Poly MPBall)]
refine (PPoly ps _ _) (PPoly qs _ _) =
   reverse $ aux [] ps qs
   where
   aux res (x : xs) (y : ys) =
    let (firstLarger, intr, diff) = intersectionAndDifference x y
      in
      case diff of
         Nothing -> aux (intr:res) xs ys
         Just i  -> if firstLarger then
                     aux (intr:res) (i:xs) ys
                    else
                       aux (intr:res) xs (i:ys)
   aux res [] [] = res
   aux _ _ _ = error $ "PPoly refine: Lists don't match up."

--precondition: both intervals have the same left end-point
intersectionAndDifference ::
  (DyadicInterval, Poly MPBall) -> (DyadicInterval, Poly MPBall)
  -> (Bool, (DyadicInterval, Poly MPBall, Poly MPBall), Maybe (DyadicInterval, Poly MPBall))
intersectionAndDifference (Interval l r, p) (Interval l' r', p') =
   if l /= l' then
       error $ "PPoly intersectionAndDifference: precondition violated. Intervals are [" ++ (show l) ++ "," ++(show r)++"] and ["++(show l')++ ","++(show r')++"]."
   else
       (firstLarger, (intr, p, p'), diff)
   where
   firstLarger = r > r'
   intr = Interval l $ min r r'
   diff =
    if r == r' then
      Nothing
    else if r > r' then
      Just $ (Interval r' r, p)
    else
      Just $ (Interval r r', p')

{- instances -}

instance CanAddAsymmetric PPoly PPoly where
  type AddType PPoly PPoly = PPoly
  add a b =
    PPoly [(i, p + q) | (i,p,q) <- refine a b]
          (min (ppoly_overlap a) (ppoly_overlap b)) (ppoly_dom a) -- TODO: how to handle polys with different domains?

instance CanMulAsymmetric PPoly PPoly where
  type MulType PPoly PPoly = PPoly
  mul a b =
    PPoly [(i, mulCheb p q) | (i,p,q) <- refine a b]
          (min (ppoly_overlap a) (ppoly_overlap b)) (ppoly_dom a) -- TODO: how to handle polys with different domains?

instance CanMulAsymmetric Cheb PPoly where
  type MulType Cheb PPoly = PPoly
  mul a b = (fromPoly a) * b

instance CanMulAsymmetric PPoly Cheb where
    type MulType PPoly Cheb = PPoly
    mul a b = a*(fromPoly b)

instance CanNeg PPoly where
  type NegType PPoly = PPoly
  negate (PPoly pieces overlap dom) =
      PPoly (fmap (\(i,p) -> (i,-p)) pieces) overlap dom

{- Mixed operations with Integer -}

instance CanAddAsymmetric PPoly Integer where
    type AddType PPoly Integer = PPoly
    add p n = lift2PPoly (+n) $ p

instance CanAddAsymmetric Integer PPoly where
    type AddType Integer PPoly = PPoly
    add n p = lift2PPoly (n+) $ p

instance CanMulAsymmetric PPoly Integer where
    type MulType PPoly Integer = PPoly
    mul p n = lift2PPoly (*n) $ p

instance CanMulAsymmetric Integer PPoly where
    type MulType Integer PPoly = PPoly
    mul n p = lift2PPoly (n*) $ p

instance CanDiv PPoly Integer where
    type DivType PPoly Integer = PPoly
    divide p n = lift2PPoly (/n) $ p

{- Mixed operations with MPBall -}

instance CanAddAsymmetric PPoly MPBall where
    type AddType PPoly MPBall = PPoly
    add p n = lift2PPoly (+n) p

instance CanAddAsymmetric MPBall PPoly where
    type AddType MPBall PPoly = PPoly
    add n p = lift2PPoly (n+) p

instance CanMulAsymmetric PPoly MPBall where
    type MulType PPoly MPBall = PPoly
    mul p n = lift2PPoly (*n) p

instance CanMulAsymmetric MPBall PPoly where
    type MulType MPBall PPoly = PPoly
    mul n p = lift2PPoly (*n) p

instance CanDiv PPoly MPBall where
    type DivType PPoly MPBall = PPoly
    divide p n = lift2PPoly (/ n) p
