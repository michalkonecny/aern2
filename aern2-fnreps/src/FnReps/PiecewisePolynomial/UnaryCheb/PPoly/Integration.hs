module FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Integration 
(integral,
 primitive)
where

import AERN2.Num

import FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Basics as PPB
import AERN2.RealFunction
import qualified FnReps.Polynomial.UnaryCheb.Poly as PB
import qualified FnReps.Polynomial.UnaryCheb.Poly.Integration as PI
import qualified FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding as PE

integral :: PPoly -> MPBall -> MPBall -> MPBall
integral pp l r =
  let
    err = sum [(PB.polyRadius p)*(b - a) | (Interval a b, p) <- piecesMeetingInterval pp (toRationalDown l) (toRationalUp r)]
  in 
  endpoints2Ball (-err) (err) + 
  sum [integrateUnaryFnA (p, max l a, min r b)  
    | (Interval a b, p) <- piecesMeetingInterval (dropAllErrors pp) (toRationalDown l) (toRationalUp r)]
     
-- TODO get rid of loss of precision     
primitive :: PPoly -> PPoly
primitive (PPoly ps ov) = PPoly (reverse $ aux (mpBall 0) polyPrimitives []) ov
  where
  polyPrimitives = map (\(i,p) -> (i, PI.primitive_function p)) ps
  aux _ [] rs = rs
  aux c ((Interval l r,p):xs) rs = 
    let
    vl = PE.evalLipschitzOnBall p (rational2BallP (getPrecision p) l)
    c' = PE.evalLipschitzOnBall p (rational2BallP (getPrecision p) r) - vl
    in
    aux (c + c') xs $ (Interval l r, p - vl + c):rs

