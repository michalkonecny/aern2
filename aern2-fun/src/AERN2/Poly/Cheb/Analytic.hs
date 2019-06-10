module AERN2.Poly.Cheb.Analytic
(
    liftAnalytic
  , sinDCT
  , cosDCT
)
where

import MixedTypesNumPrelude
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import AERN2.Poly.Basics
import AERN2.Interval
import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.DCT
import AERN2.Poly.Cheb.Maximum

sinDCT :: ChPoly MPBall -> ChPoly MPBall
sinDCT =
  liftAnalytic (\n b -> if even n then sin(b) else cos(b))

cosDCT :: ChPoly MPBall -> ChPoly MPBall
cosDCT =
  liftAnalytic (\n b -> if even n then cos(b) else sin(b))

liftAnalytic ::
  (Integer -> MPBall -> MPBall) -> -- (n, b) |-> interval enclosure of ±f^(n)(b)
  ChPoly MPBall ->
  ChPoly MPBall
liftAnalytic f p =
  reduceDegreeWithLostAccuracyLimit
    ((getAccuracy res `min` getAccuracyGuide p) - 5) $
  setAccuracyGuide (getAccuracyGuide p) $
  res
  where
  res = updateRadius (+ (errp + chebErr + radius s)) $ evalNoFuzz (centre s) (fromDomToUnitInterval sDom $ centre p)
  (rl :: MPBall, _) = endpoints $ minimumOptimisedWithAccuracy (bits 5) p (mpBall $ l) (mpBall $ r) 5 5
  (_, rr :: MPBall) = endpoints $ maximumOptimisedWithAccuracy (bits 5) p (mpBall $ l) (mpBall $ r) 5 5
  sDom  = Interval (centre rl) (centre rr)
  sDomB = setPrecision (prec 100) $ fromEndpoints rl rr :: MPBall
  fBound k = abs $ f k sDomB
  lip = hdiff * fBound 1
  Interval l r = chPoly_dom p
  errp = errorBound (lip * (mpBall $ radius p))
  targetErr = errorBound $ mpBall $ (dyadic 0.5)^!((fromAccuracy $ getAccuracyGuide p))
  x =
    ChPoly (Interval (dyadic $ -1) (dyadic 1))
            (Poly $ terms_fromList [(0, mpBall 0), (1, mpBall 1)])
            (getAccuracyGuide p)
            ChPolyBounds
  (n, chebErr) = boundTerms 0 (mpBall 1)
  s = lift1_DCT (const n) (f 0 . fromUnitIntervalToDom sDom) x
  fromUnitIntervalToDom (Interval a b) y = a + (b - a)*(y + 1)/!2
  hdiff = (rr - rl)/!2
  boundTerms k (err :: MPBall) =
    let
      finalErr = errorBound $ (fBound k) * err
    in
      if finalErr < targetErr then
        (k, finalErr)
      else
        if k == 0 then
          boundTerms 1 (hdiff*err)
        else
          boundTerms (k + 1) (hdiff*err/!(2*k))

evalNoFuzz ::
  (ChPoly MPBall) -> ChPoly MPBall -> ChPoly MPBall
evalNoFuzz (ChPoly _dom (Poly terms) _acG _) (x :: ChPoly MPBall) =
    (b0 - b2)/!2
    where
    n = terms_degree terms
    (b0:_:b2:_) = bs
    bs :: [ChPoly MPBall]
    bs = reverse $ aux n z z
    z =
      ChPoly (chPoly_dom x)
              (Poly $ terms_fromList [(0, mpBall 0)])
              (bits $ ceiling $ 1.5*(fromAccuracy $ getAccuracyGuide x))
              ChPolyBounds
    aux k bKp2 bKp1
        | k == 0 = [bKp2, bKp1, bK]
        | otherwise = bKp2 : aux (k - 1) bKp1 bK
        where
        bK = (a k) + 2 * x * bKp1 - bKp2
    a k = terms_lookupCoeffDoubleConstTerm terms k
