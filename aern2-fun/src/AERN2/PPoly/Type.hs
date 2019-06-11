{-# LANGUAGE TupleSections #-}
module AERN2.PPoly.Type
where

import MixedTypesNumPrelude

import Data.List
import Data.Maybe

import Control.CollectErrors

-- import AERN2.Normalize

import AERN2.MP.Ball (IsBall(..), MPBall, mpBall)
import AERN2.MP.Precision
import AERN2.MP.Accuracy
import AERN2.MP.Dyadic
import AERN2.MP.ErrorBound
import AERN2.Interval

import AERN2.RealFun.Operations

-- import AERN2.Poly.Ball (PolyBall, Ball(..))
-- import qualified AERN2.Poly.Ball as PolyBall
import AERN2.Poly.Cheb
import AERN2.Poly.Basics (terms_fromList, Poly(..))

import Control.Arrow (second)

import Debug.Trace

type Cheb = ChPoly MPBall

data PPoly =
  PPoly {ppoly_pieces  :: [(DyadicInterval, Cheb)],
         ppoly_dom     :: DyadicInterval}

instance (SuitableForCE es) => CanEnsureCE es PPoly

ppoly_degree :: PPoly -> Integer
ppoly_degree (PPoly ps _) =
  foldl' (\d p -> max d $ (degree . snd) p) 1 ps

instance HasDomain PPoly where
  type Domain PPoly = DyadicInterval
  getDomain = ppoly_dom

fromPoly :: Cheb -> PPoly
fromPoly (ChPoly dom p acG bnds) =
  PPoly [(uInt, (ChPoly uInt p acG bnds))] dom
  where
  uInt = dyadicInterval (-1, 1)

-- fromPoly :: Cheb -> PPoly
-- fromPoly p = fromPolyBall $ normalize (Ball p (errorBound 0))

-- fromPolyBall :: PolyBall -> PPoly
-- fromPolyBall (Ball (ChPoly dom p acG bnds) err) =
--   PPoly [(uInt, Ball (ChPoly uInt p acG bnds) err)] dom
--   where
--   uInt = Interval (dyadic $ -1) (dyadic 1)

linearPolygonI :: [(Dyadic, MPBall)] -> DyadicInterval -> Accuracy -> PPoly
linearPolygonI = linearPolygonNew

linearPolygonNew :: [(Dyadic, MPBall)] -> DyadicInterval -> Accuracy -> PPoly
linearPolygonNew ((x0,y0) : xys0) dom acG =
  aux xys0 x0 y0 []
  where
  aux [] _ _ res = PPoly (reverse res) dom
  aux ((x',y'):xys) x y res =
    aux xys x' y' ((Interval x x', linSpline x y x' y') : res)
  p = prec $ fromAccuracy acG
  linSpline a fa b fb =
    (ChPoly
      (dyadicInterval (-1,1))
      --(Poly $ terms_fromList [(0, (y*(x' - x) - x*(y' - y))/(x' - x)), (1, (y' - y)/(x' - x))]))
      (Poly $ terms_fromList
              [(0, constantTerm a fa b fb),
                (1, linearTerm a fa b fb)])
      acG (ChPolyBounds pmin pmax))
    where
    pmin = fa `min` fb
    pmax = fa `max` fb
  linearTerm a fa b fb =
    let
    a' = raisePrecisionIfBelow p (mpBall a)
    b' = raisePrecisionIfBelow p (mpBall b)
    fa' = raisePrecisionIfBelow p fa
    fb' = raisePrecisionIfBelow p fb
    in
      (fb' - fa')/!(b' - a')
  constantTerm a fa b fb =
    let
    a'  = raisePrecisionIfBelow p (mpBall a)
    b'  = raisePrecisionIfBelow p (mpBall b)
    fa' = raisePrecisionIfBelow p fa
    fb' = raisePrecisionIfBelow p fb
    in
      fa'- (a'*(fb' - fa'))/!(b' - a')
linearPolygonNew [] _ _ =
  error "linearPolygonI must be provided with a list of at least 2 points"

linearPolygonOld :: [(Dyadic, MPBall)] -> DyadicInterval -> Accuracy -> PPoly
linearPolygonOld ((x0,y0) : xys0) dom acG =
  trace("linear polygon ") $
  trace("input: "++ (show $ ((x0,y0) : xys0))) $
  trace("result: "++ (show $ aux xys0 x0 y0 [])) $
  aux xys0 x0 y0 []
  where
  aux [] _ _ res = PPoly (reverse res) dom
  aux ((x',y'):xys) x y res =
    aux xys x' y' ((Interval x x', linSpline x y x' y') : res)
  linSpline a fa b fb =
      (ChPoly
        (dyadicInterval (-1,1))
        --(Poly $ terms_fromList [(0, (y*(x' - x) - x*(y' - y))/(x' - x)), (1, (y' - y)/(x' - x))]))
        (Poly $ terms_fromList
                [(0, constantTerm a fa b fb (getPrecision fa) (getPrecision fb) Nothing),
                 (1, linearTerm a fa b fb (getPrecision fa) (getPrecision fb)  Nothing)])
        acG (ChPolyBounds pmin pmax))
    where
    pmin = fa `min` fb
    pmax = fa `max` fb
  linearTerm a fa b fb p q prev =
    let
    a' = raisePrecisionIfBelow p (mpBall a)
    b' = raisePrecisionIfBelow p (mpBall b)
    fa' = raisePrecisionIfBelow p fa
    fb' = raisePrecisionIfBelow p fb
    try =
      (fb' - fa')/!(b' - a')
    in
      if getAccuracy try >= min (getFiniteAccuracy fa) (getFiniteAccuracy fb)
      || (isJust prev
      && getAccuracy try <= getAccuracy (fromJust prev)) then
        try
      else
        linearTerm a fa b fb (p + q) p (Just try)
  constantTerm a fa b fb p q prev =
    let
    a' = raisePrecisionIfBelow p (mpBall a)
    b' = raisePrecisionIfBelow p (mpBall b)
    fa' = raisePrecisionIfBelow p fa
    fb' = raisePrecisionIfBelow p fb
    try =
      fa'- (a'*(fb' - fa'))/!(b' - a')
    in
      if getAccuracy try >= min (getFiniteAccuracy fa) (getFiniteAccuracy fb)
      || (isJust prev
      && getAccuracy try <= getAccuracy (fromJust prev)) then
        try
      else
        constantTerm a fa b fb (p + q) p (Just try)
linearPolygonOld [] _ _ =
  error "linearPolygonI must be provided with a list of at least 2 points"

liftCheb2PPoly :: (Cheb -> Cheb) -> (PPoly -> PPoly)
liftCheb2PPoly f (PPoly ps dom)  =
  PPoly (map (domify f) ps) dom
  where
  domify :: (Cheb -> Cheb)
    -> (DyadicInterval, Cheb)
    -> (DyadicInterval, Cheb)
  domify g (i, p) = (i, g p)

liftCheb2PPolyCN :: (Cheb -> CN Cheb) -> (PPoly -> CN PPoly)
liftCheb2PPolyCN f (PPoly ps dom)  =
  fmap (\ps2 -> PPoly ps2 dom) (sequence (map (domify f) ps))
  where
  domify :: (Cheb -> CN Cheb)
    -> (DyadicInterval, Cheb)
    -> CN (DyadicInterval, Cheb)
  domify g (i, p) = fmap (i,) (g p)

{-lift2PPoly :: (Poly MPBall -> Poly MPBall) -> (PPoly -> PPoly)
lift2PPoly f (PPoly pieces overlap dom) =
  PPoly (map (\(i,p) -> (i, f p)) pieces) overlap dom-}

refine :: PPoly -> PPoly
  -> [(DyadicInterval, Cheb, Cheb)]
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
  (DyadicInterval, Cheb) -> (DyadicInterval, Cheb)
  -> (Bool, (DyadicInterval, Cheb, Cheb),
      Maybe (DyadicInterval, Cheb))
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
    foldl' max (errorBound 0) $ map (\(_, p) -> radius p) ps
  centre = liftCheb2PPoly centre
  centreAsBall = centre
  centreAsBallAndRadius cp = (centre cp, radius cp)
  updateRadius updateFn = liftCheb2PPoly (updateRadius updateFn)

instance HasAccuracy PPoly where
  getAccuracy (PPoly ps _) =
    foldl' min Exact [getAccuracy p | (_,p) <- ps]

instance HasAccuracyGuide PPoly where
  getAccuracyGuide (PPoly ((_,p):_) _) =
    getAccuracyGuide p
  getAccuracyGuide _ = error "getAccuracyGuide: empty PPoly"

instance CanSetAccuracyGuide PPoly where
  setAccuracyGuide acGuide (PPoly ps dom) =
    (PPoly (map (\(d,p) -> (d, setAccuracyGuide acGuide p)) ps) dom)

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

instance HasFnConstructorInfo PPoly where
  type FnConstructorInfo PPoly = (DyadicInterval, Accuracy)
  getFnConstructorInfo (PPoly ((_,p):_) dom) = (dom, acG)
    where
    (_, acG) = getFnConstructorInfo p
  getFnConstructorInfo _ = error "PPoly getFnConstructorInfo: no segments"

{- -}

-- multiplyWithBounds :: PPoly -> MPBall -> PPoly -> MPBall -> PPoly
-- multiplyWithBounds a ba b bb =
--   PPoly [(i, PolyBall.multiplyWithBounds p ba q bb) | (i,p,q) <- refine a b]
--         (ppoly_dom a)

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
    add p n = liftCheb2PPoly (+n) $ p

instance CanSub PPoly Integer where
    type SubType PPoly Integer = PPoly
    sub p n = liftCheb2PPoly (\x -> x - n) $ p

instance CanSub Integer PPoly where
    type SubType Integer PPoly = PPoly
    sub n p = liftCheb2PPoly (\x -> n - x) $ p

instance CanAddAsymmetric Integer PPoly where
    type AddType Integer PPoly = PPoly
    add n p = liftCheb2PPoly (n+) $ p

instance CanMulAsymmetric PPoly Integer where
    type MulType PPoly Integer = PPoly
    mul p n = liftCheb2PPoly (*n) $ p

instance CanMulAsymmetric Integer PPoly where
    type MulType Integer PPoly = PPoly
    mul n p = liftCheb2PPoly (n*) $ p

instance CanDiv PPoly Integer where
    type DivTypeNoCN PPoly Integer = PPoly
    divideNoCN p n = liftCheb2PPoly (/!n) $ p
    type DivType PPoly Integer = CN PPoly
    divide p n = liftCheb2PPolyCN (/n) $ p

{- Mixed operations with MPBall -}

instance CanAddAsymmetric PPoly MPBall where
    type AddType PPoly MPBall = PPoly
    add p n = liftCheb2PPoly (+n) p


instance CanSub MPBall PPoly where
    type SubType MPBall PPoly = PPoly
    sub n p = liftCheb2PPoly (\x -> n - x) $ p

instance CanSub PPoly MPBall where
    type SubType PPoly MPBall = PPoly
    sub p n = liftCheb2PPoly (\x -> x - n) $ p

instance CanAddAsymmetric MPBall PPoly where
    type AddType MPBall PPoly = PPoly
    add n p = liftCheb2PPoly (n+) p

instance CanMulAsymmetric PPoly MPBall where
    type MulType PPoly MPBall = PPoly
    mul p n = liftCheb2PPoly (*n) p

instance CanMulAsymmetric MPBall PPoly where
    type MulType MPBall PPoly = PPoly
    mul n p = liftCheb2PPoly (*n) p

instance CanDiv PPoly MPBall where
    type DivTypeNoCN PPoly MPBall = PPoly
    divideNoCN p n = liftCheb2PPoly (/!n) p
    type DivType PPoly MPBall = CN PPoly
    divide p n = liftCheb2PPolyCN (/n) p


instance CanSinCos PPoly where
  type SinCosType PPoly = PPoly
  sin (PPoly pieces dom) =
      PPoly (fmap (\(i,p) -> (i, sin p)) pieces) dom
  cos (PPoly pieces dom) =
      PPoly (fmap (\(i,p) -> (i, cos p)) pieces) dom
