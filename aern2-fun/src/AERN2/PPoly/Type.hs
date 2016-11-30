module AERN2.PPoly.Type
where

import Numeric.MixedTypes

import Data.List

import AERN2.Normalize

import AERN2.MP.Ball (IsBall(..), MPBall)
import AERN2.MP.Dyadic
import AERN2.MP.ErrorBound
import AERN2.Interval

import AERN2.Poly.Ball
import AERN2.Poly.Cheb

type Cheb = ChPoly MPBall

data PPoly =
  PPoly {ppoly_pieces  :: [(DyadicInterval, PolyBall)],
         ppoly_overlap :: Dyadic,
         ppoly_dom     :: DyadicInterval}

fromPoly :: Cheb -> PPoly
fromPoly p = fromPolyBall (Ball p (errorBound 0))

fromPolyBall :: PolyBall -> PPoly
fromPolyBall f@(Ball p _) =
  PPoly [(Interval (dyadic $ -1) (dyadic 1), f)] (dyadic 10) (chPoly_dom p)

{-linearPolygon :: [(Rational, MPBall)] -> Rational -> PPoly
linearPolygon ((x,y) : xys) overlap = aux xys x y []
 where
 aux [] _ _ res = PPoly (reverse res) overlap
 aux ((x',y'):xys) x y res = aux xys x' y' ((Interval x x',linSpline x y x' y') : res)
 linSpline x y x' y' = Poly.normaliseCoeffs $ Poly.fromList  [(0, (y*(x' - x) - x*(y' - y))/(x' - x)), (1, (y' - y)/(x' - x))] -- TODO Poly.fromList should already provided normalised coeffs
linearPolygon [] _ = error "linearPolygon must be provided with a list of at least 2 points"-}

liftBall2PPoly :: (PolyBall -> PolyBall) -> (PPoly -> PPoly)
liftBall2PPoly f (PPoly ps ov dom)  =
  PPoly (map (domify f) ps) ov dom
  where
  domify :: (PolyBall -> PolyBall)
    -> (DyadicInterval, PolyBall)
    -> (DyadicInterval, PolyBall)
  domify g (i, p) = (i, g p)

liftCheb2PPoly :: (Cheb -> Cheb) -> (PPoly -> PPoly)
liftCheb2PPoly f =
  liftBall2PPoly (ballify f)
  where
  ballify g (Ball c r) =
    normalize $ Ball (g $ updateRadius (+r) c) (errorBound 0)

{-lift2PPoly :: (Poly MPBall -> Poly MPBall) -> (PPoly -> PPoly)
lift2PPoly f (PPoly pieces overlap dom) =
  PPoly (map (\(i,p) -> (i, f p)) pieces) overlap dom-}

refine :: PPoly -> PPoly
  -> [(DyadicInterval, PolyBall, PolyBall)]
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
  (DyadicInterval, PolyBall) -> (DyadicInterval, PolyBall)
  -> (Bool, (DyadicInterval, PolyBall, PolyBall),
      Maybe (DyadicInterval, PolyBall))
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
      Just (Interval r' r, p)
    else
      Just (Interval r r', p')

{- instances -}

instance IsBall PPoly where
  type CentreType PPoly = PPoly
  radius (PPoly ps _ _) =
    foldl' (+) (errorBound 0) $ map (\(_, p) -> radius p) ps
  centre = liftCheb2PPoly centre
  centreAsBall = centre
  centreAsBallAndRadius cp = (centre cp, radius cp)
  updateRadius updateFn = liftCheb2PPoly (updateRadius updateFn)

{- arithmetic -}

instance CanAddAsymmetric PPoly PPoly where
  type AddType PPoly PPoly = PPoly
  add a b =
    PPoly [(i, p + q) | (i,p,q) <- refine a b]
          (min (ppoly_overlap a) (ppoly_overlap b)) (ppoly_dom a) -- TODO: how to handle polys with different domains?

instance CanMulAsymmetric PPoly PPoly where
  type MulType PPoly PPoly = PPoly
  mul a b =
    PPoly [(i, p * q) | (i,p,q) <- refine a b]
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
    add p n = liftBall2PPoly (+n) $ p

instance CanAddAsymmetric Integer PPoly where
    type AddType Integer PPoly = PPoly
    add n p = liftBall2PPoly (n+) $ p

instance CanMulAsymmetric PPoly Integer where
    type MulType PPoly Integer = PPoly
    mul p n = liftBall2PPoly (*n) $ p

instance CanMulAsymmetric Integer PPoly where
    type MulType Integer PPoly = PPoly
    mul n p = liftBall2PPoly (n*) $ p

instance CanDiv PPoly Integer where
    type DivType PPoly Integer = PPoly
    divide p n = liftBall2PPoly (/n) $ p

{- Mixed operations with MPBall -}

instance CanAddAsymmetric PPoly MPBall where
    type AddType PPoly MPBall = PPoly
    add p n = liftBall2PPoly (+n) p

instance CanAddAsymmetric MPBall PPoly where
    type AddType MPBall PPoly = PPoly
    add n p = liftBall2PPoly (n+) p

instance CanMulAsymmetric PPoly MPBall where
    type MulType PPoly MPBall = PPoly
    mul p n = liftBall2PPoly (*n) p

instance CanMulAsymmetric MPBall PPoly where
    type MulType MPBall PPoly = PPoly
    mul n p = liftBall2PPoly (*n) p

instance CanDiv PPoly MPBall where
    type DivType PPoly MPBall = PPoly
    divide p n = liftBall2PPoly (/ n) p
