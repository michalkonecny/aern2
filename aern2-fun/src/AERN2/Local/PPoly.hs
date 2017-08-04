module AERN2.Local.PPoly
where

import MixedTypesNumPrelude
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import AERN2.Local.Basics
import AERN2.Local.Poly
import AERN2.Local.Maximum

import AERN2.Poly.Cheb
import AERN2.Poly.Basics
import AERN2.Poly.Ball

import AERN2.Interval

import AERN2.PPoly.Type as PPoly

type LocalPPoly = Local PPoly

fromPoly :: LocalPoly MPBall -> LocalPPoly
fromPoly = liftLocal1 PPoly.fromPoly

instance GenericMaximum LocalPPoly where
  genericise f l r ac =
    [genericisePoly p (fromUnitIntervalToDom a) (fromUnitIntervalToDom b) | (Interval a b, p) <- psDom]
    where
    PPoly ps dom@(Interval dL dR) = f l r ac
    polys :: [(DyadicInterval, ChPoly MPBall)]
    polys = map (\(i, Ball c err) -> (i, updateRadius (+ err) c)) ps
    psDom = map (\(i, ChPoly _ p acG bnds) -> (i, ChPoly dom p acG bnds)) polys
    fromUnitIntervalToDom x = (dyadic 0.5)*((dR - dL)*x + (dR + dL))


-- makeRational :: ChPoly MPBall -> ChPoly Rational
-- makeRational (ChPoly dom (Poly ts) acG _) =
--   ChPoly dom (Poly $ terms_map (rational . centre) ts) acG Nothing
