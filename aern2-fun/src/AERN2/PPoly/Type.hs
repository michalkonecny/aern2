module AERN2.PPoly.Type
where

import Numeric.MixedTypes

import Data.List
import Data.Maybe

import AERN2.Normalize

import AERN2.MP.Ball (IsBall(..), MPBall, mpBall)
import AERN2.MP.Precision
import AERN2.MP.Accuracy
import AERN2.MP.Dyadic
import AERN2.MP.ErrorBound
import AERN2.Interval

import AERN2.RealFun.Operations

import AERN2.Poly.Ball (PolyBall, Ball(..))
import qualified AERN2.Poly.Ball as PolyBall
import AERN2.Poly.Cheb
import AERN2.Poly.Basics (terms_fromList, Poly(..))

import Control.Arrow (second)

import Debug.Trace

type Cheb = ChPoly MPBall

data PPoly =
  PPoly {ppoly_pieces  :: [(DyadicInterval, PolyBall)],
         ppoly_dom     :: DyadicInterval}

instance HasDomain PPoly where
  type Domain PPoly = DyadicInterval
  getDomain = ppoly_dom

fromPoly :: Cheb -> PPoly
fromPoly p = fromPolyBall (Ball p (errorBound 0))

fromPolyBall :: PolyBall -> PPoly
fromPolyBall f@(Ball p _) =
  PPoly [(Interval (dyadic $ -1) (dyadic 1), f)] (chPoly_dom p)

linearPolygon :: [(Dyadic, MPBall)] -> DyadicInterval -> PPoly
linearPolygon ((x,y) : xys) dom =
  aux xys x y []
  where
  aux [] _ _ res = PPoly (reverse res) dom
  aux ((x',y'):xys) x y res =
    aux xys x' y' ((Interval x x', linSpline x y x' y') : res)
  linSpline a fa b fb =
    Ball
      (ChPoly
        dom
        --(Poly $ terms_fromList [(0, (y*(x' - x) - x*(y' - y))/(x' - x)), (1, (y' - y)/(x' - x))]))
        (Poly $ terms_fromList
                [(0, constantTerm a fa b fb (getPrecision fa) (getPrecision fb) Nothing),
                 (1, linearTerm a fa b fb (getPrecision fa) (getPrecision fb)  Nothing)])
        Nothing)
      (errorBound 0)
  linearTerm a fa b fb p q prev =
    let
    a' = setPrecision p (mpBall a)
    b' = setPrecision p (mpBall b)
    fa' = setPrecision p fa
    fb' = setPrecision p fb
    try =
      (fb' - fa')/(b' - a')
    in
      if isJust prev
      && getAccuracy try <= getAccuracy (fromJust prev) then
        try
      else
        linearTerm a fa b fb (p + q) p (Just try)
  constantTerm a fa b fb p q prev =
    let
    a' = setPrecision p (mpBall a)
    b' = setPrecision p (mpBall b)
    fa' = setPrecision p fa
    fb' = setPrecision p fb
    try =
      fa'- (a'*(fb' - fa'))/(b' - a')
    in
      if isJust prev
      && getAccuracy try <= getAccuracy (fromJust prev) then
        try
      else
        constantTerm a fa b fb (p + q) p (Just try)
linearPolygon [] _ =
  error "linearPolygon must be provided with a list of at least 2 points"

liftBall2PPoly :: (PolyBall -> PolyBall) -> (PPoly -> PPoly)
liftBall2PPoly f (PPoly ps dom)  =
  PPoly (map (domify f) ps) dom
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
    let
      ballAsCheb =
        case chPoly_maybeLip c of
          Nothing  -> updateRadius (+r) c
          Just lip -> chPoly_setLip lip $ updateRadius (+r) c
    in
    normalize $ Ball (g ballAsCheb) (errorBound 0)

{-lift2PPoly :: (Poly MPBall -> Poly MPBall) -> (PPoly -> PPoly)
lift2PPoly f (PPoly pieces overlap dom) =
  PPoly (map (\(i,p) -> (i, f p)) pieces) overlap dom-}

refine :: PPoly -> PPoly
  -> [(DyadicInterval, PolyBall, PolyBall)]
refine f@(PPoly ps _) g@(PPoly qs _) =
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
   aux _ _ _ =
     error $
      "PPoly refine: Lists don't match up between \n"++(show f)++ " and \n"++(show g)

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
  radius (PPoly ps _) =
    foldl' (+) (errorBound 0) $ map (\(_, p) -> radius p) ps
  centre = liftCheb2PPoly centre
  centreAsBall = centre
  centreAsBallAndRadius cp = (centre cp, radius cp)
  updateRadius updateFn = liftCheb2PPoly (updateRadius updateFn)

instance HasAccuracy PPoly where
  getAccuracy (PPoly ps _) =
    foldl' min Exact [getAccuracy p | (_,p) <- ps]

instance HasPrecision PPoly where
  getPrecision (PPoly ps _) =
    foldl' max (prec 2) [getPrecision p | (_,p) <- ps]

instance CanSetPrecision PPoly where
  setPrecision pr (PPoly ps dom) =
    PPoly
      (map (second (setPrecision pr)) ps)
      dom

instance Show PPoly where
  show (PPoly ps _) =
    concatMap (\(i,p) -> show (i,p) ++ "\n") ps

{- -}

multiplyWithBounds :: PPoly -> MPBall -> PPoly -> MPBall -> PPoly
multiplyWithBounds a ba b bb =
  PPoly [(i, PolyBall.multiplyWithBounds p ba q bb) | (i,p,q) <- refine a b]
        (ppoly_dom a)

{- arithmetic -}

instance CanAddAsymmetric PPoly PPoly where
  type AddType PPoly PPoly = PPoly
  add a b =
    PPoly [(i, p + q) | (i,p,q) <- refine a b]
          (ppoly_dom a) -- TODO: how to handle polys with different domains?

instance CanMulAsymmetric PPoly PPoly where
  type MulType PPoly PPoly = PPoly
  mul a b =
    PPoly [(i, p * q) | (i,p,q) <- refine a b]
          (ppoly_dom a) -- TODO: how to handle polys with different domains?

instance CanMulAsymmetric Cheb PPoly where
  type MulType Cheb PPoly = PPoly
  mul a b = (fromPoly a) * b

instance CanMulAsymmetric PPoly Cheb where
    type MulType PPoly Cheb = PPoly
    mul a b = a*(fromPoly b)

instance CanNeg PPoly where
  type NegType PPoly = PPoly
  negate (PPoly pieces dom) =
      PPoly (fmap (\(i,p) -> (i,-p)) pieces) dom

instance CanSub PPoly PPoly where
  type SubType PPoly PPoly = PPoly
  sub a b =
    PPoly [(i, p - q) | (i,p,q) <- refine a b]
          (ppoly_dom a) -- TODO: how to handle polys with different domains?

{- Mixed operations with Integer -}

instance CanAddAsymmetric PPoly Integer where
    type AddType PPoly Integer = PPoly
    add p n = liftBall2PPoly (+n) $ p

instance CanSub PPoly Integer where
    type SubType PPoly Integer = PPoly
    sub p n = liftBall2PPoly (\x -> x - n) $ p

instance CanSub Integer PPoly where
    type SubType Integer PPoly = PPoly
    sub n p = liftBall2PPoly (\x -> n - x) $ p

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


instance CanSub MPBall PPoly where
    type SubType MPBall PPoly = PPoly
    sub n p = liftBall2PPoly (\x -> n - x) $ p

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
